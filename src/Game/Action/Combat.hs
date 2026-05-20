-- | Actions that characters can use to affect
-- 'N.health', 'N.barrier', and 'N.defense'.
module Game.Action.Combat
  ( -- * Attacking
    afflict, pierce, damage, demolish, demolishAll
    -- * Defending
  , defend, addDefense, removeDefense
  , barricade, barricade'
    -- * Healing
  , heal, setHealth
  , leech, leech'
    -- * Special effects
  , sacrifice
  , executeAt
  , kill, killHard
  ) where

import ClassyPrelude

import qualified Class.Classed as Classed
import           Class.Labeled (Labeled)
import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Action as Action
import qualified Game.Engine.Combat as Combat
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Traps as Traps
import qualified Game.Model.Attack as Attack
import qualified Game.Model.Barrier as Barrier
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Defense (Defense(Defense))
import qualified Game.Model.Defense as Defense
import           Game.Model.Duration (Duration)
import           Game.Model.Effect (Effect(..))
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Runnable (IntRunConstraint, RunConstraint)
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

-- | Adds new destructible 'Defense'.
-- Destructible defense acts as an extra bar in front of the 'N.health'
-- of a 'N.Ninja'. All attacks except for 'afflict' attacks must damage and
-- destroy the target's 'N.defense' before they can damage the target.
-- Destructible defense can be temporary or permanent.
defend :: ∀ m. MonadPlay m => Duration -> Int -> m ()
defend (succ -> dur) amount = P.unsilenced do
    Context{skill, target, user} <- P.context
    nUser   <- P.nUser
    nTarget <- P.nTarget
    let amount' = Effects.boost user nTarget * amount + Effects.build nUser
        addNonStack :: ∀ a. Labeled a => a -> [a] -> [a]
        addNonStack = Classed.nonStack skill
    case amount' `compare` 0 of
        EQ -> return ()
        LT -> do
            context <- P.context
            let barr = Barrier.new context dur
                      (const $ return ()) (return ()) (-amount')
            P.modify target \n ->
                n { N.barrier = addNonStack barr $ N.barrier n }
        GT -> do
            P.trigger user [OnDefend]
            let defense = Defense
                    { user
                    , dur
                    , amount = amount'
                    , name   = Skill.name skill
                    }
            P.modify target \n ->
                n { N.defense = addNonStack defense $ N.defense n }

-- | Adds an amount to a 'Defense' that the target already has.
-- If the target does not have any 'N.defense' with a matching
-- 'Defense.name', nothing happens.
-- Uses 'Ninjas.addDefense' internally.
addDefense :: ∀ m. MonadPlay m => Text -> Int -> m ()
addDefense name amount = P.unsilenced . P.fromUser
    $ Ninjas.addDefense amount name

-- | Clears all 'Defense' with matching name and user.
-- Uses 'Ninjas.removeDefense' internally.
removeDefense :: ∀ m. MonadPlay m => Text -> m ()
removeDefense name = P.unsilenced . P.fromUser $ Ninjas.removeDefense name

-- | Adds new destructible 'Barrier'.
-- Destructible barrier acts as an extra bar in front of the 'N.health'
-- of a 'N.Ninja'. All attacks except for 'afflict' attacks must damage and
-- destroy the user's 'N.barrier' before they can damage the target.
-- Destructible barrier can be temporary or permanent.
barricade :: ∀ m. MonadPlay m => Duration -> Int -> m ()
barricade dur = barricade' dur (const $ return ()) (return ())

-- | Adds a 'Barrier' with an effect that occurs when its duration
-- 'Barrier.finish'es, which is passed as an argument the 'Barrier.amount' of
-- barrier remaining, and an effect that occurs each turn 'Barrier.while' it
-- exists.
barricade' :: ∀ m. MonadPlay m => Duration -> IntRunConstraint ()
            -> RunConstraint () -> Int -> m ()
barricade' dur finish while amount = P.unsilenced do
    context@Context{skill, target} <- P.context
    amount' <- (+ amount) . Effects.build <$> P.nUser
    let barr = Barrier.new context dur
               (Action.wrap . finish) (Action.wrap while) amount'
        addNonStack :: ∀ a. Labeled a => a -> [a] -> [a]
        addNonStack = Classed.nonStack skill
    case amount' `compare` 0 of
        EQ -> return ()
        LT -> do
            Context{user} <- P.context
            let defense = Defense
                    { user
                    , dur
                    , amount = -amount'
                    , name   = Skill.name skill
                    }
            P.trigger user [OnDefend]
            P.modify target \n ->
              n { N.defense = addNonStack defense $ N.defense n }
        GT -> P.modify target \n ->
            n { N.barrier = addNonStack barr $ N.barrier n }

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
