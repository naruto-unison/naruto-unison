module Mission.Missions.Base
  ( module Import
  , hasFrom
  , stunned
  -- * Action hooks
  , check
  , cure
  , damage
  , damageDuringStacks
  , damageWithStacks
  , defend
  , execute
  , heal
  , kill
  , killAffected
  , killDuring
  , interrupt
  , useDuring
  , useDuringStacks
  -- * Chakra hooks
  , deplete
  -- * Store hooks
  , checkUnique
  , damageUnique
  , killUnique
  -- * Trap hooks
  , trapUnique
  -- * Turn hooks
  , checkEnemyStatus
  , killWith
  , maintain
  ) where

import ClassyPrelude as Import
import Class.Parity as Import (allied)
import Data.IntSet as Import (size)
import Game.Model.Ninja as Import (alive, hasOwn, health)
import Mission.Goal as Import

import qualified Game.Engine.Effects as Effects
import qualified Game.Model.Chakra as Chakra
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Slot as Slot
import           Util ((∈))

resetToZero :: Int
resetToZero = -1000

hasFrom :: Ninja -> Text -> Ninja -> Bool
hasFrom user name = Ninja.has name $ Ninja.slot user

stunned :: Ninja -> Bool
stunned n = not . null $ Effects.stun n

-- ACTION HOOKS

check :: (Ninja -> Ninja -> Ninja -> Bool) -> ActionHook
check f x y z = fromEnum $ f x y z

cure :: ActionHook
cure user target target' = fromEnum $
    allied user target
    && Ninja.numHelpful target' < Ninja.numHelpful target

damage :: ActionHook
damage user target target'
  | allied user target = 0
  | otherwise          = max 0 $ health target - health target'

damageDuringStacks :: Text -> ActionHook
damageDuringStacks name user target target'
  | allied user target              = 0
  | health target' >= health target = 0
  | otherwise = Ninja.numStacks name (Ninja.slot user) user

damageWithStacks :: Text -> ActionHook
damageWithStacks name user target target'
  | allied user target              = 0
  | health target' >= health target = 0
  | otherwise = Ninja.numStacks name (Ninja.slot user) target

defend :: Text -> ActionHook
defend name user target target'
  | alive target = max 0 addedDefense
  | otherwise    = 0
  where
    userSlot     = Ninja.slot user
    addedDefense = Ninja.defenseAmount name userSlot target'
                   - Ninja.defenseAmount name userSlot target

execute :: ActionHook
execute user target target' = fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && hasFrom user "executed" target'

heal :: ActionHook
heal user target target'
  | not $ alive target = 0
  | not $ allied user target = 0
  | otherwise = max 0 $ health target' - health target

kill :: ActionHook
kill user target target' = fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')

killAffected :: Text -> ActionHook
killAffected name user target target' = fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && hasFrom user name target

killDuring :: Text -> ActionHook
killDuring name user target target' = fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && hasOwn name user

interrupt :: ActionHook
interrupt user target target'
  | allied user target = 0
  | otherwise          = max 0 interrupted
  where
    numChannels n = length (Ninja.channels n) + length (Ninja.newChans n)
    interrupted   = numChannels target - numChannels target'

useDuring :: Text -> ActionHook
useDuring name user _ _ = fromEnum $ hasOwn name user

useDuringStacks :: Text -> ActionHook
useDuringStacks name user _ _ = Ninja.numStacks name (Ninja.slot user) user

-- CHAKRA HOOKS

deplete :: ChakraHook
deplete (_, chak) (_, chak') = max 0 $ Chakra.total chak' - Chakra.total chak

-- STORE HOOKS

checkUnique :: (Ninja -> Ninja -> Bool) -> StoreHook
checkUnique f user _ target' store
  | targetSlot ∈ store = (store, 0)
  | f user target'     = (targetSlot `insertSet` store, 1)
  | otherwise          = (store, 0)
  where
    targetSlot = Slot.toInt $ Ninja.slot target'

compareUnique :: (Ninja -> Ninja -> Ninja -> Bool) -> StoreHook
compareUnique f user target target' store
  | targetSlot ∈ store    = (store, 0)
  | f user target target' = (targetSlot `insertSet` store, 1)
  | otherwise             = (store, 0)
  where
    targetSlot = Slot.toInt $ Ninja.slot target'

damageUnique :: StoreHook
damageUnique = compareUnique \user target target' ->
    not (allied user target)
    && health target' < health target

killUnique :: StoreHook
killUnique = compareUnique \user target target' ->
    not (allied user target)
    && alive target
    && not (alive target')

-- TRAP HOOKS

trapUnique :: TrapHook
trapUnique target store
  | targetSlot ∈ store = (store, 0)
  | otherwise          = (targetSlot `insertSet` store, 1)
  where
    targetSlot = Slot.toInt $ Ninja.slot target

-- TURN HOOKS

checkEnemyStatus :: Text -> TurnHook
checkEnemyStatus name user _ target
  | allied user target = 0
  | alive target       = fromEnum $ hasFrom user name target
  | otherwise          = 0

killWith :: Text -> TurnHook
killWith name user target target' = fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && hasFrom user name target

maintain :: Text -> TurnHook
maintain name user _ target
  | Ninja.slot user /= Ninja.slot target = 0
  | not $ alive target                   = resetToZero
  | hasOwn name user                     = 1
  | otherwise                            = resetToZero
