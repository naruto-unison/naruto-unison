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

import           Core.Util ((—), (∈), intersectsSet)
import qualified Class.Classed as Classed
import qualified Class.Labeled as Labeled
import qualified Class.Play as P
import           Class.Play (MonadPlay)
import qualified Model.Attack as Attack
import           Model.Attack (Attack)
import qualified Model.Barrier as Barrier
import           Model.Barrier (Barrier(Barrier))
import           Model.Class (Class(..), ClassSet)
import qualified Model.Context as Context
import           Model.Context (Context)
import qualified Model.Copy as Copy
import qualified Model.Defense as Defense
import           Model.Defense (Defense(Defense))
import           Model.Duration (Duration(..), Turns, incr, sync)
import qualified Model.Effect as Effect
import           Model.Effect (Amount(..), Effect(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja, is)
import qualified Model.Runnable as Runnable
import           Model.Runnable (Runnable, RunConstraint)
import qualified Model.Skill as Skill
import           Model.Trap (Trigger(..))
import qualified Engine.Effects as Effects
import qualified Engine.Execute as Execute
import           Engine.Execute (Affected(..))
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
    user   <- P.user
    target <- P.target
    P.modify user   \n -> n { Ninja.barrier = [] }
    P.modify target \n -> n { Ninja.defense = [] }

userAdjust :: Attack -> ClassSet -> Ninja -> Float -> Float
userAdjust atk classes nUser x = x
    * strengthen Percent
    * weaken Percent
    + strengthen Flat
    - weaken Flat
  where
    direct = Direct ∈ classes
    strengthen
      | direct    = Effect.identity
      | otherwise = Effects.strengthen classes nUser
    weaken
      | direct                = Effect.identity
      | atk == Attack.Afflict = Effect.identity
      | otherwise             = Effects.weaken classes nUser

targetAdjust :: Attack -> ClassSet -> Ninja -> Float -> Float
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
        -> ClassSet -- ^ 'Skill.classes'.
        -> Ninja -- ^ User.
        -> Ninja -- ^ Target.
        -> Int -- ^ Base damage.
        -> Int
formula atk classes nUser nTarget = truncate .
                                    targetAdjust atk' classes nTarget .
                                    userAdjust atk' classes nUser .
                                    fromIntegral
  where
    atk' = case atk of
        Attack.Damage | nUser `is` Pierce -> Attack.Pierce
        _                                 -> atk

-- | Internal combat engine. Performs an 'Attack.Afflict', 'Attack.Pierce',
-- 'Attack.Damage', or 'Attack.Demolish' attack.
attack :: ∀ m. MonadPlay m => Attack -> Int -> m ()
attack atk dmg = void $ runMaybeT do
    skill      <- P.skill
    user       <- P.user
    target     <- P.target
    nUser      <- P.nUser
    nTarget    <- P.nTarget
    let classes = insertSet atkClass $ Skill.classes skill
        dmgCalc = formula atk classes nUser nTarget dmg
        direct  = Direct ∈ classes
        (dmg'Barrier, barr) = absorbBarrier dmgCalc $ Ninja.barrier nUser
        handleDefense
          | nTarget `is` Undefend = (,)
          | otherwise             = absorbDefense
        (dmg'Def, defense)
          | direct    = handleDefense dmgCalc $ Ninja.defense nTarget
          | otherwise = handleDefense dmg'Barrier $ Ninja.defense nTarget

    guard . not $ classes `intersectsSet` Effects.invincible nTarget
               || dmg < Effects.threshold nTarget
               || not direct && nUser `is` Stun atkClass
               || dmgCalc <= 0

    if atk == Attack.Afflict then do
        P.modify target $ Ninja.adjustHealth (— dmgCalc)
    else do
        unless direct $ P.modify user \n -> n { Ninja.barrier = barr }
        if atk == Attack.Demolish || dmg'Def <= 0 then
            P.modify target \n -> n { Ninja.defense = defense }
        else if dmg'Def == 0 then return () else
            P.modify target $ Ninja.adjustHealth (— dmg'Def) .
                \n -> n { Ninja.defense = defense }

    damaged <- (Ninja.health nTarget -) . Ninja.health <$> P.nTarget
    when (damaged > 0) do
        P.trigger user [OnDamage]
        P.trigger target $ OnDamaged <$> toList classes
        P.modify target $ Traps.track PerDamaged damaged
        whenM P.new . P.modify user $ Traps.track PerDamage damaged

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
    else when (amount' > 0) $ P.modify target \n ->
        n { Ninja.defense = Classed.nonStack skill defense $ Ninja.defense n }

-- | Adds an amount to a 'Defense' that the target already has.
-- If the target does not have any 'Ninja.defense' with a matching
-- 'Defense.name', nothing happens.
addDefense :: ∀ m. MonadPlay m => Text -> Int -> m ()
addDefense name amount = P.unsilenced do
    user    <- P.user
    target  <- P.user
    nTarget <- P.nTarget
    maybe (return ()) (P.modify target . Ninja.addDefense amount) .
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
        user    = Context.user context
        target  = Context.target context
        dur'    = Copy.maxDur (Skill.copying skill) $ sync dur
        save :: RunConstraint () -> Runnable Context
        save f  = Runnable.To
                      { Runnable.target = context { Context.new = False }
                      , Runnable.run    = f
                      }
        finish' amt
          | dur' < sync dur = save $ return ()
          | otherwise       = save $
                              Execute.wrap (singletonSet Trapped) (finish amt)
        barr = Barrier
            { Barrier.amount = amount'
            , Barrier.user = user
            , Barrier.name   = Skill.name skill
            , Barrier.while  = save $ Execute.wrap (singletonSet Trapped) while
            , Barrier.finish = finish'
            , Barrier.dur    = dur'
            }
    if amount' < 0 then do
        P.with Context.reflect do
            target' <- P.target
            nTarget <- P.nTarget
            damage (-amount')
            damaged <- (Ninja.health nTarget -) . Ninja.health <$> P.nTarget
            P.modify target' $ Traps.track PerDamaged damaged
    else when (amount' > 0) $ P.modify target \n ->
        n { Ninja.barrier = Classed.nonStack skill barr $ Ninja.barrier n }

-- | Kills the target. The target can survive if it has the 'Endure' effect.
-- Uses 'Ninja.kill' internally.
kill :: ∀ m. MonadPlay m => m ()
kill = P.toTarget $ Ninja.kill True

-- | Kills the target. The target cannot survive by any means.
-- Uses 'Ninja.kill' internally.
killHard :: ∀ m. MonadPlay m => m ()
killHard = P.toTarget $ Ninja.kill False

-- | Adjusts 'Ninja.health'. Uses 'Ninja.setHealth' internally.
setHealth :: ∀ m. MonadPlay m => Int -> m ()
setHealth = P.toTarget . Ninja.setHealth

-- | Adds a flat amount of 'Ninja.health'.
heal :: ∀ m. MonadPlay m => Int -> m ()
heal hp = P.unsilenced do
    nTarget <- P.nTarget
    unless (nTarget `is` Plague) do
        user   <- P.user
        target <- P.target
        nUser  <- P.nUser
        let hp'  = Effects.boost user nTarget * hp + Effects.bless nUser
        P.modify target $ Ninja.adjustHealth (+ hp')
        healed <- (— Ninja.health nTarget) . Ninja.health <$> P.nTarget
        if healed > 0 then do
            P.trigger target [OnHealed]
            P.modify target $ Traps.track PerHealed healed
        else if healed < 0 then
            P.modify target $ Traps.track PerDamaged (-healed)
        else
            return ()

-- | Restores a percentage of missing 'Ninja.health'.
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
        P.modify target $ Ninja.adjustHealth (+ hp')
        healed <- (— Ninja.health nTarget) . Ninja.health <$> P.nTarget
        if healed > 0 then do
            P.trigger target [OnHealed]
            P.modify target $ Traps.track PerHealed healed
        else if healed < 0 then
            P.modify target $ Traps.track PerDamaged (-healed)
        else
            return ()

-- | Damages the target and passes the amount of damage dealt to another action.
-- Typically paired with @self . 'heal'@ to effectively drain the target's
-- 'Ninja.health' into that of the user.
leech :: ∀ m. MonadPlay m => Int -> (Int -> m ()) -> m ()
leech hp f = do
    user     <- P.user
    target   <- P.target
    classes  <- Skill.classes <$> P.skill
    hpBefore <- Ninja.health <$> P.nTarget
    P.modify target $ Ninja.adjustHealth (— hp)
    damaged <- (hpBefore -) . Ninja.health <$> P.nTarget
    when (damaged > 0) do
        f damaged
        P.trigger user [OnDamage]
        P.trigger target $ OnDamaged <$> toList classes
        P.modify target $ Traps.track PerDamaged damaged
        whenM P.new .
            P.modify user $ Traps.track PerDamage damaged

-- | Sacrifices some amount of the target's 'Ninja.health' down to a minimum.
-- If the target is the user and has the 'ImmuneSelf' effect, nothing happens.
-- Uses 'Ninja.sacrifice' internally.
sacrifice :: ∀ m. MonadPlay m
          => Int  -- ^ Minimum 'Ninja.health'.
          -> Int  -- ^ Amount of 'Ninja.health' to sacrifice.
          -> m ()
sacrifice minhp hp = do
    user    <- P.user
    target  <- P.target
    nTarget <- P.nTarget
    unless (user == target && nTarget `is` ImmuneSelf) .
        P.modify target $ Ninja.sacrifice minhp hp

-- | Resets a 'Ninja.Ninja' to their initial state.
-- Uses 'Ninja.reset' internally.
factory :: ∀ m. MonadPlay m => m ()
factory = P.toTarget Ninja.factory
