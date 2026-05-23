-- | Actions that characters can use to affect
-- 'N.health', 'N.barrier', and 'N.defense'.
module Game.Action.Combat
  ( -- * Attacking
    afflict, pierce, damage, demolish, demolishAll
    -- * Defending
  , build
  , defend, increaseDefense, decreaseDefense
  , barricade
    -- * Healing
  , heal, setHealth
  , leech, leech'
    -- * Special effects
  , sacrifice
  , executeAt
  , kill, killHard
  ) where

import ClassyPrelude

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Engine.Combat as Combat
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Traps as Traps
import qualified Game.Model.Attack as Attack
import           Game.Model.Destructible (Destructible(Destructible))
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import qualified Game.Model.Destructible as Destructible
import           Game.Model.Duration (Duration(..))
import           Game.Model.Effect (Effect(..))
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Game.Model.Status (Status(Status))
import qualified Game.Model.Status
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
    Context{target, user} <- P.context
    P.modify user   \n -> n { N.barrier = [] }
    P.modify target \n -> n { N.defense = [] }

-- | Adds an amount to a 'Destructible' 'N.defense' that the target already has.
-- If the target does not have any 'N.defense' with a matching
-- 'Destructible.name', nothing happens.
-- Uses 'Ninjas.increaseDefense' internally.
increaseDefense :: ∀ m. MonadPlay m => Text -> Int -> m ()
increaseDefense name amount = P.unsilenced . P.fromUser
    $ Ninjas.increaseDefense amount name

-- | Clears all 'Destructible' 'N.defense' with matching name and user.
-- Uses 'Ninjas.decreaseDefense' internally.
decreaseDefense :: ∀ m. MonadPlay m => Text -> m ()
decreaseDefense name = P.unsilenced . P.fromUser $ Ninjas.decreaseDefense name

-- | Adds new 'Destructible' 'N.barrier'.
-- Destructible barrier acts as an extra bar in front of the 'N.health'
-- of a 'Ninja'. All attacks except for 'afflict' attacks must damage and
-- destroy the user's 'N.barrier' before they can damage the target.
-- Destructible barrier can be temporary or permanent.
barricade :: ∀ m. MonadPlay m => Duration -> Destructible -> m ()
barricade dur barrier = do
    Context{target} <- P.context
    P.modify target $ Ninjas.addBarrier $ Destructible.setDur dur barrier

-- | Adds new 'Destructible' 'N.defense'.
-- Destructible defense acts as an extra bar in front of the 'N.health'
-- of a 'Ninja'. All attacks except for 'afflict' attacks must damage and
-- destroy the target's 'N.defense' before they can damage the target.
-- Destructible defense can be temporary or permanent.
defend :: ∀ m. MonadPlay m => Duration -> Destructible -> m ()
defend dur defense@Destructible{amount} = do
    Context{target, user} <- P.context
    when (amount > 0) $
        P.trigger user [OnDefend]
    P.modify target $ Ninjas.addDefense $ Destructible.setDur dur defense

build :: ∀ m. MonadPlay m => Int -> m Destructible
build amount = create <$> P.context <*> P.nUser
  where
    create context n = Destructible.new context Permanent
                     $ amount + Effects.build n

-- | Kills the target if their health is below a threshold.
-- The target can survive if it has the 'Endure' effect.
-- Uses 'Ninjas.kill' internally.
executeAt :: ∀ m. MonadPlay m => Int -> m ()
executeAt threshold = whenM (shouldExecute <$> P.nTarget) kill
  where
    shouldExecute Ninja{health} = health > 0 && health <= threshold

killFull :: ∀ m. MonadPlay m => Bool -> m ()
killFull endure = whenM (N.alive <$> P.nTarget) do
    P.toTarget $ Ninjas.kill endure
    unlessM (N.alive <$> P.nTarget) do
        Context{user, skill} <- P.context
        P.toTarget . Ninjas.addStatus $ executed user skill
  where
    executed user skill = Status
        { amount = 1
        , name   = "executed"
        , user
        , skill
        , effects = mempty
        , classes = setFromList [Unremovable, Hidden]
        , bombs   = []
        , maxDur  = 1
        , dur     = 1
        }

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
setHealth amt = do
    nHealth <- N.health <$> P.nTarget
    P.toTarget $ Ninjas.setHealth amt
    nHealth' <- N.health <$> P.nTarget
    Context{target, user, skill = Skill{classes}} <- P.context
    case nHealth' `compare` nHealth of
        EQ -> return ()
        GT -> P.trigger user [OnHeal]
        LT -> P.trigger target $ OnDamaged <$> toList classes

-- | Adds a flat amount of 'N.health'.
-- Uses 'Ninjas.adjustHealth' internally.
heal :: ∀ m. MonadPlay m => Int -> m ()
heal hp = P.unsilenced do
    nTarget <- P.nTarget
    unless (nTarget `is` Plague || not (N.alive nTarget)) do
        Context{target, user} <- P.context
        nUser <- P.nUser
        let hp' = Effects.boost user nTarget * hp + Effects.bless nUser
        P.modify target $ Ninjas.adjustHealth (+ hp')
        damaged <- (N.health nTarget -) . N.health <$> P.nTarget
        case damaged `compare` 0 of
            EQ -> return ()
            GT -> P.modify target $ Traps.track PerDamaged damaged
            LT -> P.trigger user [OnHeal]


-- | Damages the target and passes the amount of damage dealt to another action,
-- retargeted toward the user. Typically paired with @'heal'@ to effectively
-- drain the target's 'N.health' into that of the user.
-- Uses 'afflict' internally.
leech :: ∀ m. MonadPlay m => Int -> (Int -> m ()) -> m ()
leech hp f = leech' hp $ P.with Context.reflect . f

-- | Like @'leech'@, but does not retarget the effect toward the user.
leech' :: ∀ m. MonadPlay m => Int -> (Int -> m ()) -> m ()
leech' hp f = do
    Context{target, user, skill = Skill{classes}} <- P.context
    hpBefore <- N.health <$> P.nTarget
    afflict hp
    damaged <- (hpBefore -) . N.health <$> P.nTarget
    when (damaged > 0) do
        f damaged
        P.trigger user [OnDamage]
        P.trigger target $ OnDamaged <$> NonAffliction : toList classes
        P.modify target $ Traps.track PerDamaged damaged

-- | Sacrifices some amount of the target's 'N.health' down to a minimum.
-- Uses 'Ninjas.sacrifice' internally.
sacrifice :: ∀ m. MonadPlay m
          => Int  -- ^ Minimum 'N.health'.
          -> Int  -- ^ Amount of 'N.health' to sacrifice.
          -> m ()
sacrifice _     0  = return ()
sacrifice minhp hp = do
    Context{target, user} <- P.context
    when (user == target)
        $ P.trigger user [OnSacrifice]
    P.toTarget $ Ninjas.sacrifice minhp hp
