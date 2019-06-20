{-# LANGUAGE ImpredicativeTypes    #-}
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

import ClassyPrelude.Yesod
import           Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.List as List

import           Core.Util ((—), (∈), intersects)
import qualified Class.Classed as Classed
import qualified Class.Labeled as Labeled
import qualified Class.Play as P
import           Class.Play (Play(..), PlayConstraint, MonadPlay)
import qualified Model.Attack as Attack
import           Model.Attack (Attack)
import qualified Model.Barrier as Barrier
import           Model.Barrier (Barrier)
import           Model.Class (Class(..))
import qualified Model.Context as Context
import qualified Model.Copy as Copy
import qualified Model.Defense as Defense
import           Model.Defense (Defense)
import           Model.Duration (Duration(..), Turns, incr, sync)
import qualified Model.Effect as Effect
import           Model.Effect (Amount(..), Effect(..))
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Model.Skill as Skill
import qualified Engine.Effects as Effects
import qualified Engine.Execute as Execute
import           Engine.Execute (Affected(..))

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
    P.modify $ Game.adjust user Ninja.clearBarrier
    P.modify $ Game.adjust target Ninja.clearDefense

-- | Internal combat engine. Performs an 'Attack.Afflict', 'Attack.Pierce',
-- 'Attack.Damage', or 'Attack.Demolish' attack.
attack :: ∀ m. MonadPlay m => Attack -> Int -> m ()
attack _ 0 = return ()
attack atk dmg = void $ runMaybeT do
    skill      <- P.skill
    user       <- P.user
    target     <- P.target
    nUser      <- P.nUser
    nTarget    <- P.nTarget
    let classes = atkClass : Skill.classes skill
        direct  = Direct ∈ classes
        -- | Damage modified by the user's 'Effect's.
        dmg'User
          | direct = toRational dmg
          | otherwise = (* Effects.strengthen classes nUser Percent) .
                        (+ Effects.strengthen classes nUser Flat) $
                        toRational dmg
        reduceAfflic = Effects.reduce [Affliction] nTarget
        reduce amount
          | atk /= Attack.Damage    = Effect.identity amount
          | Ninja.is Pierce nUser   = Effect.identity amount
          | Ninja.is Expose nTarget = Effect.identity amount
          | otherwise               = Effects.reduce classes nTarget amount
        weaken amount
          | atk == Attack.Afflict = Effect.identity amount
          | direct                = Effect.identity amount
          | otherwise             = Effects.weaken classes nUser amount
        bleed = Effects.bleed classes nTarget
        -- | Damage modified by the target's 'Effect's.
        dmg'Target = truncate .
                    (— reduceAfflic Flat) .
                    (— reduce Flat) .
                    (— weaken Flat) .
                    (+ bleed Flat) .
                    (* reduceAfflic Percent) .
                    (* reduce Percent) .
                    (* weaken Percent) .
                    (* bleed Percent) $
                    toRational dmg'User
        (dmg'Barrier, barr) = absorbBarrier dmg'Target $ Ninja.barrier nUser
        handleDefense
          | Ninja.is Undefend nTarget = (,)
          | otherwise            = absorbDefense
        (dmg'Def, defense)
          | direct    = handleDefense dmg'Target $ Ninja.defense nTarget
          | otherwise = handleDefense dmg'Barrier $ Ninja.defense nTarget

    guard . not $ classes `intersects` Effects.invincible nTarget
               || dmg < Effects.threshold nTarget
               || not direct && Ninja.is (Stun atkClass) nUser
               || dmg'Target <= 0

    if atk == Attack.Afflict then
        P.modify . Game.adjust target $ Ninja.adjustHealth (— dmg'Target)
    else do
        unless direct .
            P.modify $ Game.adjust user \n -> n { Ninja.barrier = barr }
        if atk == Attack.Demolish || dmg'Def <= 0 then
            P.modify $ Game.adjust target \n -> n { Ninja.defense = defense }
        else if dmg'Def == 0 then return () else
            P.modify . Game.adjust target $ Ninja.adjustHealth (— dmg'Def) .
                \n -> n { Ninja.defense = defense }
  where
    atkClass = case atk of
        Attack.Afflict -> Affliction
        _              -> NonAffliction

-- | Adds new destructible 'Defense'.
-- Destructible defense acts as an extra bar in front of the 'Ninja.health'
-- of a 'Ninja'. All attacks except for 'afflict' attacks must damage and
-- destroy the target's 'Ninja.defense' before they can damage the target.
-- Destructible defense can be temporary or permanent.
defend :: ∀ m. MonadPlay m => Turns -> Int -> m ()
defend (Duration -> dur) amount = do
    skill      <- P.skill
    user       <- P.user
    target     <- P.target
    nUser      <- P.nUser
    nTarget    <- P.nTarget
    let amount' = Effects.boost user nTarget * amount + Effects.build nUser
        defense = Defense.Defense
                      { Defense.amount = amount'
                      , Defense.user = user
                      , Defense.name   = Skill.name skill
                      , Defense.dur    = Copy.maxDur (Skill.copying skill) .
                                         incr $ sync dur
                      }
    if amount' < 0 then
        damage (-amount')
    else when (amount' > 0) . P.modify $ Game.adjust target \n ->
        n { Ninja.defense = Classed.nonStack skill defense $ Ninja.defense n }

-- | Adds an amount to a 'Defense' that the target already has.
-- If the target does not have any 'Ninja.defense' with a matching
-- 'Defense.name', nothing happens.
addDefense :: ∀ m. MonadPlay m => Text -> Int -> m ()
addDefense name amount = do
    user    <- P.user
    target  <- P.user
    nTarget <- P.nTarget
    case find (Labeled.match name user) $ Ninja.defense nTarget of
        Nothing      -> return ()
        Just defense -> P.modify $ Game.adjust target \n ->
            n { Ninja.defense =
                    defense { Defense.amount = Defense.amount defense + amount }
                    : List.deleteBy Labeled.eq defense (Ninja.defense n) }

-- | Adds new destructible 'Barrier'.
-- Destructible barrier acts as an extra bar in front of the 'Ninja.health'
-- of a 'Ninja'. All attacks except for 'afflict' attacks must damage and
-- destroy the user's 'Ninja.barrier' before they can damage the target.
-- Destructible barrier can be temporary or permanent.
barrier :: ∀ m. MonadPlay m => Turns -> Int -> m ()
barrier dur = barrierDoes dur (const $ return ()) (return ())

-- | Adds a 'Barrier' with an effect that occurs when its duration
-- 'Barrier.finish'es, which is passed as an argument the 'Barrier.amount' of
-- barrier remaining, and an effect that occurs each turn 'Barrier.while' it
-- exists.
barrierDoes :: ∀ m. MonadPlay m => Turns -> (Int -> PlayConstraint ())
            -> PlayConstraint () -> Int -> m ()
barrierDoes (Duration -> dur) finish while amount = do
    context    <- P.context
    amount'    <- (amount +) . Effects.build <$> P.nUser
    let skill   = Context.skill context
        user  = Context.user context
        target  = Context.target context
        dur'    = Copy.maxDur (Skill.copying skill) $ sync dur
        finish' amt
          | dur' < sync dur = (context, Play $ return ())
          | otherwise       = (context, Play $ Execute.wrap [Trapped] (finish amt))
        barr = Barrier.Barrier
            { Barrier.amount = amount'
            , Barrier.user = user
            , Barrier.name   = Skill.name skill
            , Barrier.while  = (context, Play $ Execute.wrap [Channeled, Trapped] while)
            , Barrier.finish = finish'
            , Barrier.dur    = dur'
            }
    if amount' < 0 then
        P.with Context.reflect $ damage (-amount')
    else when (amount' > 0) . P.modify $ Game.adjust target \n ->
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
heal hp = do
    nTarget <- P.nTarget
    unless (Ninja.is Plague nTarget) do
        user   <- P.user
        target <- P.target
        nUser  <- P.nUser
        let hp'  = Effects.boost user nTarget * hp + Effects.bless nUser
        P.modify . Game.adjust target $ Ninja.adjustHealth (+ hp')

-- | Restores a percentage of missing 'Ninja.health'.
restore :: ∀ m. MonadPlay m => Int -> m ()
restore percent = do
    nTarget <- P.nTarget
    unless (Ninja.is Plague nTarget) do
        user   <- P.user
        target <- P.target
        nUser  <- P.nUser
        let hp'  = Effects.boost user nTarget * percent
                   * (100 - Ninja.health nTarget) `quot` 100
                   + Effects.bless nUser
        P.modify . Game.adjust target $ Ninja.adjustHealth (+ hp')

-- | Damages the target and passes the amount of damage dealt to another action.
-- Typically paired with @'self' . 'heal'@ to effectively drain the target's
-- 'Ninja.health' into that of the user.
leech :: ∀ m. MonadPlay m => Int -> (Int -> m ()) -> m ()
leech hp f = do
    target   <- P.target
    hpBefore <- Ninja.health <$> P.nTarget
    P.modify . Game.adjust target $ Ninja.adjustHealth (— hp)
    hpAfter  <- Ninja.health <$> P.nTarget
    f $ hpBefore - hpAfter

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
    unless (user == target && Ninja.is ImmuneSelf nTarget) .
        P.modify . Game.adjust target $ Ninja.sacrifice minhp hp

-- | Resets a 'Ninja' to their initial state. Uses 'Ninja.reset' internally.
factory :: ∀ m. MonadPlay m => m ()
factory = P.toTarget Ninja.factory
