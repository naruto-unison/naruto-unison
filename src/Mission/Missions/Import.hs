{-# OPTIONS_HADDOCK prune #-}

-- Base import for modules in @Mission.Missions@.
module Mission.Missions.Import
  ( module Import
  , win, winConsecutive
  -- * Action hooks
  , check
  , cure
  , damage, damageDuringStacks, damageWithStacks
  , defend
  , demolish
  , execute
  , heal
  , kill, killAffected, killDuring
  , interrupt
  , use, useDuring, useDuringStacks
  -- * Chakra hooks
  , deplete
  -- * Store hooks
  , checkUnique
  , affectUniqueEnemy
  , damageUnique
  , killUnique, killUniqueDuring
  , stunUnique
  , useUnique
  -- * Trap hooks
  , trapUnique, trapUniqueAlly, trapUniqueEnemy
  -- * Turn hooks
  , checkEnemyStatus
  , killWith
  , maintain, maintainOnAlly
  ) where

import ClassyPrelude as Import
import Class.Parity as Import (allied)
import Data.IntSet as Import (size)
import Game.Model.Ninja as Import (alive, hasOwn, health)
import Game.Model.Trigger as Import (Trigger(..))
import Game.Engine.Effects as Import (stunned)
import Mission.Goal as Import

import           Class.Display (Display(..))
import qualified Game.Model.Chakra as Chakra
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Slot as Slot
import           Util ((∈), (∉), commas)

-- | Add this to mission progress in order to reset it to 0.
resetToZero :: Int
resetToZero = -10000

--  | True if target has a 'Status' from the user with matching 'Status.name'.
hasFrom :: Ninja -> Text -> Ninja -> Bool
hasFrom user name = Ninja.has name $ Ninja.slot user

winFull :: WinType -> Int -> [Text] -> Goal
winFull winType reach chars = Reach reach Career desc $ Win winType chars
  where
    desc = toStrict . builderToLazy $
           "Win " ++ display reach ++ " matches with "
           ++ commas "and" (display <$> chars) ++ " together."

-- | 'WinTotal' objective, given a goal and a team of Characters.
win :: Int -> [Text] -> Goal
win = winFull WinTotal

-- 'WinConsecutive' objective, given a goal and a team of Characters.
winConsecutive :: Int -> [Text] -> Goal
winConsecutive = winFull WinConsecutive

-- ACTION HOOKS

-- | 1 if the condition holds true, otherwise 0.
check :: (Ninja -> Ninja -> Ninja -> Bool) -> ActionHook
check f _ x y z = fromEnum $ f x y z

-- | 1 if the user cured the target, otherwise 0.
cure :: ActionHook
cure _ user target target' = fromEnum $
    allied user target
    && Ninja.numHelpful target' < Ninja.numHelpful target

-- | Damage received by the target after an action.
damage :: ActionHook
damage _ user target target'
  | allied user target = 0
  | otherwise = max 0 $ health target - health target'

-- | Damage received by the target after an action while the user has some
-- number of stacks of a @Status@.
damageDuringStacks :: Text -> ActionHook
damageDuringStacks name _ user target target'
  | allied user target              = 0
  | health target' >= health target = 0
  | otherwise = Ninja.numStacks name (Ninja.slot user) user

-- | Damage received by the target after an action while the target has some
-- number of stacks of a @Status@.
damageWithStacks :: Text -> ActionHook
damageWithStacks name _ user target target'
  | allied user target              = 0
  | health target' >= health target = 0
  | otherwise = Ninja.numStacks name (Ninja.slot user) target

-- | 'Ninja.defense' added to the target after an action.
defend :: ActionHook
defend name user target target'
  | alive target = max 0 addedDefense
  | otherwise    = 0
  where
    userSlot     = Ninja.slot user
    addedDefense = Ninja.defenseAmount name userSlot target'
                   - Ninja.defenseAmount name userSlot target

-- | 'Ninja.defense' destroyed after an action.
demolish :: ActionHook
demolish _ user target target'
  | allied user target = 0
  | otherwise = max 0 $ Ninja.totalDefense target - Ninja.totalDefense target'

-- | 1 if the user killed the target with an instant-kill effect, otherwise 0.
execute :: ActionHook
execute _ user target target' = fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && hasFrom user "executed" target'

-- | Healing received by a target after an action.
heal :: ActionHook
heal _ user target target'
  | not $ alive target = 0
  | not $ allied user target = 0
  | otherwise = max 0 $ health target' - health target

-- | 1 if the target died after an action, otherwise 0.
kill :: ActionHook
kill _ user target target' = fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')

-- | 1 if the target died after an action while affected by a @Status@,
-- otherwise 0.
killAffected :: Text -> ActionHook
killAffected name _ user target target' = fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && hasFrom user name target

-- | 1 if the target died after an action while the user had a @Status@,
-- otherwise 0.
killDuring :: Text -> ActionHook
killDuring name _ user target target' = fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && Ninja.numActive name user /= 0

-- | Number of target's 'Ninja.channels' canceled due to an action.
interrupt :: ActionHook
interrupt _ user target target'
  | allied user target = 0
  | otherwise          = max 0 interrupted
  where
    numChannels n = length (Ninja.channels n) + length (Ninja.newChans n)
    interrupted   = numChannels target - numChannels target'

-- Always 1.
use :: ActionHook
use _ _ _ _ = 1

-- | 1 if the action was used while the user was affected by a @Status@,
-- otherwise 0.
useDuring :: Text -> ActionHook
useDuring name _ user _ _ = fromEnum $ Ninja.numActive name user /= 0

-- | Number of user's stacks of a @Status@ after an action.
useDuringStacks :: Text -> ActionHook
useDuringStacks name _ user _ _ = Ninja.numStacks name (Ninja.slot user) user

-- CHAKRA HOOKS

-- | Number of 'Chakra.Chakra's depleted.
deplete :: ChakraHook
deplete (_, chak) (_, chak') = max 0 $ Chakra.total chak' - Chakra.total chak

-- STORE HOOKS

-- | Like 'check', but once it succeeds, it cannot succeed again for that
-- target.
checkUnique :: (Text -> Ninja -> Ninja -> Bool) -> StoreHook
checkUnique f name user _ target' store
  | targetSlot ∈ store  = (store, 0)
  | f name user target' = (targetSlot `insertSet` store, 1)
  | otherwise           = (store, 0)
  where
    targetSlot = Slot.toInt $ Ninja.slot target'

-- | Like 'checkUnique', but using the target's state before the action as well.
compareUnique :: (Text -> Ninja -> Ninja -> Ninja -> Bool) -> StoreHook
compareUnique f name user target target' store
  | targetSlot ∈ store         = (store, 0)
  | f name user target target' = (targetSlot `insertSet` store, 1)
  | otherwise                  = (store, 0)
  where
    targetSlot = Slot.toInt $ Ninja.slot target'

-- | Apply a @Status@ to an enemy.
affectUniqueEnemy :: StoreHook
affectUniqueEnemy = checkUnique \name user target ->
    alive target
    && not (allied user target)
    && hasFrom user name target

-- | Damage an enemy.
damageUnique :: StoreHook
damageUnique = compareUnique \_ user target target' ->
    not (allied user target)
    && health target' < health target

-- | Kill an enemy.
killUnique :: StoreHook
killUnique = compareUnique \_ user target target' ->
    not (allied user target)
    && alive target
    && not (alive target')

-- | Kill an enemy while the user is affected by a @Status@.
killUniqueDuring :: Text -> StoreHook
killUniqueDuring name = compareUnique \_ user target target' ->
    not (allied user target)
    && alive target
    && not (alive target')
    && Ninja.numActive name user /= 0

-- | Stun an enemy.
stunUnique :: StoreHook
stunUnique = checkUnique \name user target ->
    not (allied user target)
    && hasFrom user name target
    && stunned target

-- | Use an action on a target.
useUnique :: StoreHook
useUnique _ _ target _ store =
    (targetSlot `insertSet` store, fromEnum $ targetSlot ∉ store)
  where
    targetSlot = Slot.toInt $ Ninja.slot target

-- TRAP HOOKS

-- | Tallies the number of unique targets who trigger a trap.
trapUnique :: TrapHook
trapUnique _ target store =
    (targetSlot `insertSet` store, fromEnum $ targetSlot ∉ store)
  where
    targetSlot = Slot.toInt $ Ninja.slot target

-- | 'trapUnique' restricted to the user's team.
trapUniqueAlly :: TrapHook
trapUniqueAlly user target store
  | not $ allied user target = (store, 0)
  | otherwise                = trapUnique user target store

-- | 'trapUnique' restricted to the enemy's team.
trapUniqueEnemy :: TrapHook
trapUniqueEnemy user target store
  | allied user target = (store, 0)
  | otherwise          = trapUnique user target store

-- TURN HOOKS

-- | 1 if an enemy has a @Status@ at the end of the turn, otherwise 0.
checkEnemyStatus :: Text -> TurnHook
checkEnemyStatus name player user _ target store
  | not $ allied player user = (store, 0)
  | allied user target       = (store, 0)
  | alive target             = (store, fromEnum $ hasFrom user name target)
  | otherwise                = (store, 0)

-- | 1 if an enemy dies with a @Status@ at the end of the turn, otherwise 0.
killWith :: Text -> TurnHook
killWith name player user target target' store = (store, ) . fromEnum $
    allied player user
    && not (allied user target)
    && alive target
    && not (alive target')
    && hasFrom user name target

-- | Increases while the target maintains a @Status@.
-- Resets to 0 if they lose the @Status@.
maintain :: Text -> TurnHook
maintain name player user _ target store
  | Ninja.slot user /= Ninja.slot target = (store, 0)
  | not $ alive target                   = (store, resetToZero)
  | Ninja.numActive name user == 0       = (store, resetToZero)
  | allied player user                   = (store, 1)
  | otherwise                            = (store, 0)

-- | 'maintain' restricted to the user's team.
maintainOnAlly :: Text -> TurnHook
maintainOnAlly name player user _ target store
  | not $ allied user target       = (store, 0)
  | not $ alive target             = (targetSlot `deleteSet` store, reset)
  | not $ hasFrom user name target = (targetSlot `deleteSet` store, reset)
  | otherwise = (targetSlot `insertSet` store, fromEnum $ allied player user)
  where
    targetSlot = Slot.toInt $ Ninja.slot target
    reset
      | targetSlot ∈ store = resetToZero
      | otherwise          = 0
