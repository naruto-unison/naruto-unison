-- | Actions that characters can use to affect
-- 'Ninja.health', 'Ninja.barrier', and 'Ninja.defense'.
module Action.Combat
  ( -- * Attacking
    afflict, pierce, damage, demolish, demolishAll
    -- * Defending
  , defend, addDefense
  , barrier, barrierDoes
    -- * Healing
  , heal, restore, setHealth
  , leech
    -- * Special effects
  , sacrifice
  , kill, killHard
  , factory
  ) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Enum.Set.Class (EnumSet)

import           Core.Util ((—), (∉))
import qualified Class.Classed as Classed
import qualified Class.Labeled as Labeled
import qualified Class.Play as P
import           Class.Play (MonadPlay)
import qualified Model.Attack as Attack
import           Model.Attack (Attack)
import qualified Model.Barrier as Barrier
import           Model.Barrier (Barrier)
import           Model.Class (Class(..))
import qualified Model.Context as Context
import qualified Model.Copy as Copy
import qualified Model.Defense as Defense
import           Model.Defense (Defense(Defense))
import           Model.Duration (Duration(..), Turns, incr, sync)
import qualified Model.Effect as Effect
import           Model.Effect (Amount(..), Effect(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja, is)
import           Model.Runnable (RunConstraint)
import qualified Model.Skill as Skill
import           Model.Trap (Trigger(..))
import qualified Engine.Effects as Effects
import qualified Engine.Execute as Execute
import           Engine.Execute (Affected(..))
import qualified Engine.Ninjas as Ninjas
import qualified Engine.Traps as Traps

-- | Reduces incoming damage by depleting the user's 'Ninja.barrier'.
absorbBarrier :: Int -> [Barrier] -> (Int, [Barrier])
absorbBarrier hp [] = (hp, [])
absorbBarrier hp (x:xs)
  | Barrier.amount x <= hp = absorbBarrier (hp - Barrier.amount x) xs
  | otherwise = (0, x { Barrier.amount = Barrier.amount x - hp } : xs)

-- | Reduces incoming damage by depleting the target's 'Ninja.defense'.
absorbDefense :: Int -> [Defense] -> (Int, [Defense])
absorbDefense hp [] = (hp, [])
absorbDefense hp (x:xs)
  | Defense.amount x <= hp = absorbDefense (hp - Defense.amount x) xs
  | otherwise = (0, x { Defense.amount = Defense.amount x - hp } : xs)

-- | Deals damage that ignores 'Reduce' effects, 'Ninja.barrier',
-- and 'Ninja.defense'.
afflict :: ∀ m. MonadPlay m => Int -> m ()
afflict = attack Attack.Afflict
-- | Deals damage that ignores 'Reduce' effects.
pierce :: ∀ m. MonadPlay m => Int -> m ()
pierce = attack Attack.Pierce
-- | Deals damage.
damage :: ∀ m. MonadPlay m => Int -> m ()
damage = attack Attack.Damage
-- | Deals damage to the user's 'Ninja.barrier' and the target's 'Ninja.defense'
-- without affecting the target's 'Ninja.health'.
demolish :: ∀ m. MonadPlay m => Int -> m ()
demolish = attack Attack.Demolish
-- | Removes all 'Ninja.barrier' from the user and 'Ninja.defense' from the
-- target.
demolishAll :: ∀ m. MonadPlay m => m ()
demolishAll = do
    user <- P.user
    P.modify user \n -> n { Ninja.barrier = [] }
    target <- P.target
    P.modify target \n -> n { Ninja.defense = [] }

userAdjust :: Attack -> EnumSet Class -> Ninja -> Float -> Float
userAdjust atk classes nUser x = x
    * strengthen Percent
    * weaken Percent
    + strengthen Flat
    - weaken Flat
  where
    strengthen = Effects.strengthen classes nUser
    weaken
      | atk == Attack.Afflict = Effect.identity
      | otherwise             = Effects.weaken classes nUser

targetAdjust :: Attack -> EnumSet Class -> Ninja -> Float -> Float
targetAdjust atk classes nTarget x = x
    * bleed Percent
    * reduceAfflic Percent
    * reduce Percent
    + bleed Flat
    - reduceAfflic Flat
    - reduce Flat
  where
    bleed         = Effects.bleed classes nTarget
    reduceAfflic  = Effects.reduce (singletonSet Affliction) nTarget
    reduce
      | atk /= Attack.Damage = Effect.identity
      | nTarget `is` Expose  = Effect.identity
      | otherwise            = Effects.reduce classes nTarget


-- | Damage formula.
formula :: Attack -- ^ Attack type.
        -> EnumSet Class -- ^ 'Skill.classes'.
        -> Ninja -- ^ User.
        -> Ninja -- ^ Target.
        -> Int -- ^ Base damage.
        -> Int
formula atk classes nUser nTarget = limit . truncate .
                                    targetAdjust atk' classes nTarget .
                                    userAdjust atk' classes nUser .
                                    fromIntegral
  where
    atk' = case atk of
        Attack.Damage | nUser `is` Pierce -> Attack.Pierce
        _                                 -> atk
    limit = case Effects.limit nTarget of
        Just x | atk /= Attack.Afflict -> min x
        _                              -> id

-- | Internal combat engine. Performs an 'Attack.Afflict', 'Attack.Pierce',
-- 'Attack.Damage', or 'Attack.Demolish' attack.
-- Uses 'Ninjas.adjustHealth' internally.
attack :: ∀ m. MonadPlay m => Attack -> Int -> m ()
attack atk dmg = void $ runMaybeT do
    skill      <- P.skill
    user       <- P.user
    target     <- P.target
    nUser      <- P.nUser
    nTarget    <- P.nTarget
    let classes = insertSet atkClass $ Skill.classes skill
        dmgCalc = formula atk classes nUser nTarget dmg
        (dmg'Barrier, barr) = absorbBarrier dmgCalc $ Ninja.barrier nUser
        handleDefense
          | nTarget `is` Undefend = (,)
          | otherwise             = absorbDefense
        (dmg'Def, defense) = handleDefense dmg'Barrier $ Ninja.defense nTarget

    guard . not $ dmg < Effects.threshold nTarget
               || Direct ∉ classes && nUser `is` Stun atkClass
               || dmgCalc <= 0

    if atk == Attack.Afflict then do
        P.modify target $ Ninjas.adjustHealth (— dmgCalc)
    else if nTarget `is` DamageToDefense then
        let damageDefense = Defense
                { Defense.amount = dmgCalc
                , Defense.user   = user
                , Defense.name   = Skill.name skill
                , Defense.dur    = 0
                }
        in P.modify target \n ->
          n { Ninja.defense = damageDefense : Ninja.defense n }
    else do
        P.modify user \n -> n { Ninja.barrier = barr }
        if atk == Attack.Demolish || dmg'Def <= 0 then
            P.modify target \n -> n { Ninja.defense = defense }
        else if dmg'Def == 0 then return () else
            P.modify target $ Ninjas.adjustHealth (— dmg'Def) .
                \n -> n { Ninja.defense = defense }

    damaged <- (Ninja.health nTarget -) . Ninja.health <$> P.nTarget
    when (damaged > 0) do
        P.trigger user [OnDamage]
        P.trigger target $ OnDamaged <$> toList classes
        P.modify target $ Traps.track PerDamaged damaged

  where
    atkClass = case atk of
        Attack.Afflict -> Affliction
        _              -> NonAffliction

-- | Adds new destructible 'Defense'.
-- Destructible defense acts as an extra bar in front of the 'Ninja.health'
-- of a 'Ninja.Ninja'. All attacks except for 'afflict' attacks must damage and
-- destroy the target's 'Ninja.defense' before they can damage the target.
-- Destructible defense can be temporary or permanent.
defend :: ∀ m. MonadPlay m => Turns -> Int -> m ()
defend (Duration -> dur) amount = P.unsilenced do
    skill      <- P.skill
    user       <- P.user
    target     <- P.target
    nUser      <- P.nUser
    nTarget    <- P.nTarget
    let amount' = Effects.boost user nTarget * amount + Effects.build nUser
        defense = Defense
                      { Defense.amount = amount'
                      , Defense.user   = user
                      , Defense.name   = Skill.name skill
                      , Defense.dur    = Copy.maxDur (Skill.copying skill) .
                                         incr $ sync dur
                      }
    if amount' < 0 then do
        damage (-amount')
        damaged <- (Ninja.health nTarget -) . Ninja.health <$> P.nTarget
        P.modify target $ Traps.track PerDamaged damaged
    else when (amount' > 0) do
        P.trigger user [OnDefend]
        P.modify target \n ->
          n { Ninja.defense = Classed.nonStack skill defense $ Ninja.defense n }

-- | Adds an amount to a 'Defense' that the target already has.
-- If the target does not have any 'Ninja.defense' with a matching
-- 'Defense.name', nothing happens.
-- Uses 'Ninjas.addDefense' internally.
addDefense :: ∀ m. MonadPlay m => Text -> Int -> m ()
addDefense name amount = P.unsilenced do
    user    <- P.user
    target  <- P.user
    nTarget <- P.nTarget
    mapM_ (P.modify target . Ninjas.addDefense amount) .
        find (Labeled.match name user) $ Ninja.defense nTarget


-- | Adds new destructible 'Barrier'.
-- Destructible barrier acts as an extra bar in front of the 'Ninja.health'
-- of a 'Ninja.Ninja'. All attacks except for 'afflict' attacks must damage and
-- destroy the user's 'Ninja.barrier' before they can damage the target.
-- Destructible barrier can be temporary or permanent.
barrier :: ∀ m. MonadPlay m => Turns -> Int -> m ()
barrier dur = barrierDoes dur (const $ return ()) (return ())

-- | Adds a 'Barrier' with an effect that occurs when its duration
-- 'Barrier.finish'es, which is passed as an argument the 'Barrier.amount' of
-- barrier remaining, and an effect that occurs each turn 'Barrier.while' it
-- exists.
barrierDoes :: ∀ m. MonadPlay m => Turns -> (Int -> RunConstraint ())
            -> RunConstraint () -> Int -> m ()
barrierDoes (Duration -> dur) finish while amount = P.unsilenced do
    context    <- P.context
    amount'    <- (amount +) . Effects.build <$> P.nUser
    let skill   = Context.skill context
        target  = Context.target context
        dur'    = Copy.maxDur (Skill.copying skill) $ sync dur
        barr    = Barrier.new context dur' (finish' dur') while' amount'
    if amount' < 0 then do
        P.with Context.reflect do
            nTarget <- P.nTarget
            damage (-amount')
            damaged <- (Ninja.health nTarget -) . Ninja.health <$> P.nTarget
            target' <- P.target
            P.modify target' $ Traps.track PerDamaged damaged
    else when (amount' > 0) $ P.modify target \n ->
        n { Ninja.barrier = Classed.nonStack skill barr $ Ninja.barrier n }
  where
    finish' :: Int -> Int -> RunConstraint ()
    finish' dur'
      | dur' < sync dur = const $ return ()
      | otherwise       = Execute.wrap (singletonSet Trapped) . finish
    while' :: RunConstraint ()
    while' = Execute.wrap (singletonSet Trapped) while

-- | Kills the target. The target can survive if it has the 'Endure' effect.
-- Uses 'Ninjas.kill' internally.
kill :: ∀ m. MonadPlay m => m ()
kill = P.toTarget $ Ninjas.kill True

-- | Kills the target. The target cannot survive by any means.
-- Uses 'Ninjas.kill' internally.
killHard :: ∀ m. MonadPlay m => m ()
killHard = P.toTarget $ Ninjas.kill False

-- | Adjusts 'Ninja.health'.
-- Uses 'Ninjas.setHealth' internally.
setHealth :: ∀ m. MonadPlay m => Int -> m ()
setHealth amt = P.toTarget $ Ninjas.setHealth amt

-- | Adds a flat amount of 'Ninja.health'.
-- Uses 'Ninjas.adjustHealth' internally.
heal :: ∀ m. MonadPlay m => Int -> m ()
heal hp = P.unsilenced do
    nTarget <- P.nTarget
    unless (nTarget `is` Plague || not (Ninja.alive nTarget)) do
        user   <- P.user
        target <- P.target
        nUser  <- P.nUser
        let hp'  = Effects.boost user nTarget * hp + Effects.bless nUser
        P.modify target $ Ninjas.adjustHealth (+ hp')
        damaged <- (Ninja.health nTarget -) . Ninja.health <$> P.nTarget
        when (damaged > 0) . P.modify target $ Traps.track PerDamaged damaged

-- | Restores a percentage of missing 'Ninja.health'.
-- Uses 'Ninjas.adjustHealth' internally.
restore :: ∀ m. MonadPlay m => Int -> m ()
restore percent = P.unsilenced do
    nTarget <- P.nTarget
    unless (nTarget `is` Plague) do
        user   <- P.user
        target <- P.target
        nUser  <- P.nUser
        let hp'  = Effects.boost user nTarget * percent
                   * (100 - Ninja.health nTarget) `quot` 100
                   + Effects.bless nUser
        P.modify target $ Ninjas.adjustHealth (+ hp')
        damaged <- (Ninja.health nTarget -) . Ninja.health <$> P.nTarget
        when (damaged > 0) . P.modify target $ Traps.track PerDamaged damaged

-- | Damages the target and passes the amount of damage dealt to another action.
-- Typically paired with @self . 'heal'@ to effectively drain the target's
-- 'Ninja.health' into that of the user.
-- Uses 'Ninjas.adjustHealth' internally.
leech :: ∀ m. MonadPlay m => Int -> (Int -> m ()) -> m ()
leech hp f = do
    user     <- P.user
    target   <- P.target
    classes  <- Skill.classes <$> P.skill
    hpBefore <- Ninja.health <$> P.nTarget
    P.modify target $ Ninjas.adjustHealth (— hp)
    damaged <- (hpBefore -) . Ninja.health <$> P.nTarget
    when (damaged > 0) do
        f damaged
        P.trigger user [OnDamage]
        P.trigger target $ OnDamaged <$> toList classes
        P.modify target $ Traps.track PerDamaged damaged

-- | Sacrifices some amount of the target's 'Ninja.health' down to a minimum.
-- If the target is the user and has the 'ImmuneSelf' effect, nothing happens.
-- Uses 'Ninjas.sacrifice' internally.
sacrifice :: ∀ m. MonadPlay m
          => Int  -- ^ Minimum 'Ninja.health'.
          -> Int  -- ^ Amount of 'Ninja.health' to sacrifice.
          -> m ()
sacrifice minhp hp = do
    user    <- P.user
    target  <- P.target
    nTarget <- P.nTarget
    unless (user == target && nTarget `is` ImmuneSelf) .
        P.modify target $ Ninjas.sacrifice minhp hp

-- | Resets a 'Ninja.Ninja' to their initial state.
-- Uses 'Ninjas.factory' internally.
factory :: ∀ m. MonadPlay m => m ()
factory = P.toTarget Ninjas.factory
