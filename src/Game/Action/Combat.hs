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
  , leech
    -- * Special effects
  , sacrifice
  , executeAt
  , kill, killHard
    -- * Internals
  , formula, attack
  ) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Enum.Set (EnumSet)

import qualified Class.Classed as Classed
import           Class.Labeled (Labeled)
import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Action as Action
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Traps as Traps
import           Game.Model.Attack (Attack)
import qualified Game.Model.Attack as Attack
import           Game.Model.Barrier (Barrier(Barrier))
import qualified Game.Model.Barrier as Barrier
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import           Game.Model.Defense (Defense(Defense))
import qualified Game.Model.Defense as Defense
import           Game.Model.Duration (Duration)
import           Game.Model.Effect (Amount(..), Effect(..))
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Runnable (RunConstraint)
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Game.Model.Status (Status(Status))
import qualified Game.Model.Status
import           Game.Model.Trigger (Trigger(..))

-- | Reduces incoming damage by depleting the user's 'N.barrier'.
absorbBarrier :: Int -> [Barrier] -> (Int, [Barrier])
absorbBarrier hp [] = (hp, [])
absorbBarrier hp (x@Barrier{amount}:xs)
  | amount <= hp = absorbBarrier (hp - amount) xs
  | otherwise    = (0, x { Barrier.amount = amount - hp } : xs)

-- | Reduces incoming damage by depleting the target's 'N.defense'.
absorbDefense :: Int -> [Defense] -> (Int, [Defense])
absorbDefense hp [] = (hp, [])
absorbDefense hp (x@Defense{amount}:xs)
  | amount <= hp = absorbDefense (hp - amount) xs
  | otherwise    = (0, x { Defense.amount = amount - hp } : xs)

-- | Deals damage that ignores 'Reduce' effects, 'N.barrier',
-- and 'N.defense'.
afflict :: ∀ m. MonadPlay m => Int -> m ()
afflict = attack Attack.Afflict

-- | Deals damage that ignores 'Reduce' effects.
pierce :: ∀ m. MonadPlay m => Int -> m ()
pierce = attack Attack.Pierce

-- | Deals damage.
damage :: ∀ m. MonadPlay m => Int -> m ()
damage = attack Attack.Damage

-- | Deals damage to the user's 'N.barrier' and the target's 'N.defense'
-- without affecting the target's 'N.health'.
demolish :: ∀ m. MonadPlay m => Int -> m ()
demolish = attack Attack.Demolish

-- | Removes all 'N.barrier' from the user and 'N.defense' from the
-- target.
demolishAll :: ∀ m. MonadPlay m => m ()
demolishAll = do
    Context{target, user} <- P.context
    P.modify user   \n -> n { N.barrier = [] }
    P.modify target \n -> n { N.defense = [] }

userAdjust :: Attack -> EnumSet Class -> Ninja -> Float -> Float
userAdjust atk classes nUser x = x
    * max 0 (1 + strengthen Percent - weaken Percent)
    + strengthen Flat
    - weaken Flat
  where
    strengthen = Effects.strengthen classes nUser
    weaken
      | atk == Attack.Afflict = const 0
      | otherwise             = Effects.weaken classes nUser

targetAdjust :: Attack -> EnumSet Class -> Ninja -> Float -> Float
targetAdjust atk classes nTarget x = x
    * max 0 (1 + bleed Percent - reduceAfflic Percent - reduce Percent)
    + bleed Flat
    - reduceAfflic Flat
    - reduce Flat
  where
    bleed        = Effects.bleed classes nTarget
    reduceAfflic = Effects.reduce (singletonSet Affliction) nTarget
    reduce amt
      | atk == Attack.Damage = Effects.reduce classes nTarget amt
      | otherwise            = 0

-- | Damage formula.
formula :: Attack -- ^ Attack type.
        -> EnumSet Class -- ^ 'Skill.classes'.
        -> Ninja -- ^ User.
        -> Ninja -- ^ Target.
        -> Int -- ^ Base damage.
        -> Int
formula atk classes nUser nTarget = limit . round
    . targetAdjust atk' classes nTarget
    . userAdjust atk' classes nUser
    . fromIntegral
  where
    atk'
      | atk == Attack.Damage && nUser `is` Pierce = Attack.Pierce
      | otherwise = atk
    limit i
      | atk == Attack.Afflict = i
      | otherwise = case Effects.limit nTarget of
        Just x  -> min x i
        Nothing -> i

-- | Internal combat engine. Performs an 'Attack.Afflict', 'Attack.Pierce',
-- 'Attack.Damage', or 'Attack.Demolish' attack.
-- Uses 'Ninjas.adjustHealth' internally.
attack :: ∀ m. MonadPlay m => Attack -> Int -> m ()
attack atk dmg = void $ runMaybeT do
    nTarget <- P.nTarget
    guard . not $ nTarget `is` Invulnerable atkClass

    channeled <- isChanneled <$> P.context
    guard . not $ channeled && nTarget `is` AntiChannel

    Context{target, user, skill = Skill{classes, name}} <- P.context
    nUser <- P.nUser
    let classes'            = insertSet atkClass classes
        dmgCalc             = formula atk classes' nUser nTarget dmg
        (dmg'Barrier, barr) = absorbBarrier dmgCalc $ N.barrier nUser
        handleDefense
          | nTarget `is` Undefend = (,)
          | otherwise             = absorbDefense
        (dmg'Def, defense) = handleDefense dmg'Barrier $ N.defense nTarget

    guard $ dmgCalc > Effects.threshold nTarget -- Always 0 or higher

    if atk > Attack.Afflict && nTarget `is` DamageToDefense then
        let damageDefense = Defense
                { amount = dmgCalc
                , user
                , name
                , dur    = 0
                }
        in
        P.modify target \n -> n { N.defense = damageDefense : N.defense n }

    else if atk == Attack.Afflict then
        P.modify target $ Ninjas.adjustHealth (- dmgCalc)

    else do
        P.modify user \n -> n { N.barrier = barr }
        if atk == Attack.Demolish || dmg'Def <= 0 then
            P.modify target \n -> n { N.defense = defense }
        else
            P.modify target $ Ninjas.adjustHealth (- dmg'Def) . \n ->
                n { N.defense = defense }

    damaged <- (N.health nTarget -) . N.health <$> P.nTarget
    when (damaged > 0) do
        P.trigger user [OnDamage]
        P.trigger target $ OnDamaged <$> toList classes'
        P.modify target $ Traps.track PerDamaged damaged

  where
    isChanneled Context{continues, new} = continues && not new
    atkClass
      | atk == Attack.Afflict = Affliction
      | otherwise             = NonAffliction

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
barricade' :: ∀ m. MonadPlay m => Duration -> (Int -> RunConstraint ())
            -> RunConstraint () -> Int -> m ()
barricade' dur finish while amount = P.unsilenced do
    context@Context{skill, target} <- P.context
    amount' <- (+ amount) . Effects.build <$> P.nUser
    let barr = Barrier.new context dur
               (\n -> Action.wrap $ finish n) (Action.wrap while) amount'
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

-- | Damages the target and passes the amount of damage dealt to another action.
-- Typically paired with @self . 'heal'@ to effectively drain the target's
-- 'N.health' into that of the user.
-- Uses 'afflict' internally.
leech :: ∀ m. MonadPlay m => Int -> (Int -> m ()) -> m ()
leech hp f = do
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
