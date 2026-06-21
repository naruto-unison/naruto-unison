-- | Actions that characters can use to affect
-- 'N.health', 'N.barrier', and 'N.defense'.
module Game.Action.Combat
  ( -- * Attacking
    afflict, pierce, damage, demolish, demolishAll
    -- * Defending
  , defend, defend', increaseDefense, removeDefense
  , barricade, barricade'
    -- * Healing
  , heal, setHealth, resurrect
  , leech, leech'
    -- * Special effects
  , sacrifice
  , executeAt
  , kill, killHard
  ) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..))

import qualified Class.Parity as Parity
import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Engine.Combat as Combat
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Model.Attack as Attack
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Duration (Duration(..))
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.ID as ID
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import qualified Game.Model.Skill as Skill
import           Game.Model.Trigger (Trigger(..))

-- | Deals damage that ignores 'Reduce' effects, 'N.barrier',
-- and 'N.defense'.
afflict :: ∀ m. MonadPlay m => Int -> m ()
afflict = Combat.attack Attack.Afflict

-- | Deals damage that ignores 'Reduce' effects.
pierce :: ∀ m. MonadPlay m => Int -> m ()
pierce = Combat.attack Attack.Pierce

-- | Deals damage.
damage :: ∀ m. MonadPlay m => Int -> m ()
damage = Combat.attack Attack.Damage

-- | Deals damage to the user's 'N.barrier' and the target's 'N.defense'
-- without affecting the target's 'N.health'.
demolish :: ∀ m. MonadPlay m => Int -> m ()
demolish = Combat.attack Attack.Demolish

-- | Removes all 'N.barrier' from the user and 'N.defense' from the
-- target.
demolishAll :: ∀ m. MonadPlay m => m ()
demolishAll = do
    Ninja{barrier, slot = user}   <- P.nUser
    Ninja{defense, slot = target} <- P.nTarget
    P.modify user   Ninjas.clearBarrier
    P.modify target Ninjas.clearDefense
    P.trigger user   $ OnBreak . ID.from <$> barrier
    P.trigger target $ OnBreak . ID.from <$> defense

-- | Adds an amount to a 'Destructible' 'N.defense' that the target already has.
-- If the target does not have any 'N.defense' with a matching
-- 'Destructible.name', nothing happens.
-- Uses 'Ninjas.increaseDefense' internally.
increaseDefense :: ∀ m. MonadPlay m => Text -> Int -> m ()
increaseDefense name amount = P.unsilenced
    $ (P.toTargetFromUser $ Ninjas.increaseDefense amount) name

-- | Clears all 'Destructible' 'N.defense' with matching name and user.
-- Uses 'Ninjas.removeDefense' internally.
removeDefense :: ∀ m. MonadPlay m => Text -> m ()
removeDefense name = P.unsilenced do
    P.toTargetFromUser Ninjas.removeDefense name
    Context{user} <- P.context
    triggerID     <- P.createID name
    P.trigger user [OnBreak triggerID]

-- | Adds new 'Destructible' 'N.barrier'.
-- Destructible barrier acts as an extra bar in front of the 'N.health'
-- of a 'Ninja'. All attacks except for 'afflict' attacks must damage and
-- destroy the user's 'N.barrier' before they can damage the target.
-- Destructible barrier can be temporary or permanent.
barricade :: ∀ m. MonadPlay m => Duration -> Int -> m ()
barricade dur amount = barricade' dur amount []

-- | Adds new 'Destructible' 'N.barrier'.
-- Destructible barrier acts as an extra bar in front of the 'N.health'
-- of a 'Ninja'. All attacks except for 'afflict' attacks must damage and
-- destroy the user's 'N.barrier' before they can damage the target.
-- Destructible barrier can be temporary or permanent.
barricade' :: ∀ m. MonadPlay m => Duration -> Int -> [Effect] -> m ()
barricade' dur amount effects = P.toTarget
    . Ninjas.addBarrier =<< Combat.build dur amount effects

-- | Adds new 'Destructible' 'N.defense'.
-- Destructible defense acts as an extra bar in front of the 'N.health'
-- of a 'Ninja'. All attacks except for 'afflict' attacks must damage and
-- destroy the target's 'N.defense' before they can damage the target.
-- Destructible defense can be temporary or permanent.
defend :: ∀ m. MonadPlay m => Duration -> Int -> m ()
defend dur amount = defend' dur amount []

-- | Adds new 'Destructible' 'N.defense'.
-- Destructible defense acts as an extra bar in front of the 'N.health'
-- of a 'Ninja'. All attacks except for 'afflict' attacks must damage and
-- destroy the target's 'N.defense' before they can damage the target.
-- Destructible defense can be temporary or permanent.
defend' :: ∀ m. MonadPlay m => Duration -> Int -> [Effect] -> m ()
defend' dur amount effects = do
    Context{user, target} <- P.context
    P.modify target . Ninjas.addDefense =<< Combat.build dur amount effects
    when (amount > 0)
        $ P.trigger user [OnDefend]

-- | Kills the target if their health is below a threshold.
-- The target can survive if it has the 'Endure' effect.
-- Uses 'Ninjas.kill' internally.
executeAt :: ∀ m. MonadPlay m => Int -> m ()
executeAt threshold = whenM (shouldExecute <$> P.nTarget) kill
  where
    shouldExecute Ninja{health} = health > 0 && health <= threshold

killFull :: ∀ m. MonadPlay m => Bool -> m ()
killFull endure = void $ runMaybeT do
    guard . N.alive =<< P.nTarget
    Context{target, user} <- P.context
    P.modify target $ Ninjas.kill endure
    guard . not $ Parity.allied user target
    guard . not . N.alive =<< P.nTarget
    P.trigger user [OnExecute]

-- | Kills the target. The target can survive if it has the 'Endure' effect.
-- Uses 'Ninjas.kill' internally.
kill :: ∀ m. MonadPlay m => m ()
kill = killFull True

-- | Kills the target. The target cannot survive by any means.
-- It's a good day for it!
-- Uses 'Ninjas.kill' internally.
killHard :: ∀ m. MonadPlay m => m ()
killHard = killFull False

-- | Adjusts 'N.health'.
-- Uses 'Ninjas.setHealth' internally.
setHealth :: ∀ m. MonadPlay m => Int -> m ()
setHealth amount = Combat.adjustHealth $ const amount

-- | Adds a flat amount of 'N.health'.
-- Uses 'Ninjas.adjustHealth' internally.
heal :: ∀ m. MonadPlay m => Int -> m ()
heal hp
  | hp <= 0 = return ()
  | otherwise = P.unsilenced do
    nTarget <- P.nTarget
    unless (nTarget `is` Plague || not (N.alive nTarget)) do
        nUser <- P.nUser
        let hp' = Effects.boost nUser.slot nTarget * hp + Effects.bless nUser
        Combat.adjustHealth (+ hp')

resurrect :: ∀ m. MonadPlay m => Int -> m ()
resurrect (min 100 -> hp) = P.unsilenced do
    Context{target, user} <- P.context
    Ninja{health} <- P.nTarget
    when (health < hp) do
        P.modify target \n -> n { N.health = hp }
        P.trigger user [OnHeal]
        when (health == 0)
            $ P.trigger target [OnResurrected]

-- | Damages the target and passes the amount of damage dealt to another action,
-- retargeted toward the user. Typically paired with @'heal'@ to effectively
-- drain the target's 'N.health' into that of the user.
-- Uses 'afflict' internally.
leech :: ∀ m. MonadPlay m => Int -> (Int -> m ()) -> m ()
leech hp f = leech' hp $ P.with Context.reflect . f

-- | Like @'leech'@, but does not retarget the effect toward the user.
leech' :: ∀ m. MonadPlay m => Int -> (Int -> m ()) -> m ()
leech' hp f
  | hp <= 0   = return ()
  | otherwise = do
    Context{target, user, skill} <- P.context
    hpBefore <- N.health <$> P.nTarget
    afflict hp
    damaged <- (hpBefore -) . N.health <$> P.nTarget
    when (damaged > 0) do
        f damaged
        P.trigger user [OnDamage]
        P.trigger target $ OnDamaged <$> NonAffliction : toList skill.classes

-- | Sacrifices some amount of the target's 'N.health' down to a minimum.
-- Uses 'Ninjas.sacrifice' internally.
sacrifice :: ∀ m. MonadPlay m
          => Int  -- ^ Minimum 'N.health'.
          -> Int  -- ^ Amount of 'N.health' to sacrifice.
          -> m ()
sacrifice minhp hp
  | hp <= 0   = return ()
  | otherwise = do
    Context{target, user} <- P.context
    when (user == target)
        $ P.trigger user [OnSacrifice]
    P.modify target $ Ninjas.sacrifice minhp hp
