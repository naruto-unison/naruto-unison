module Mission.Missions.Base
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
  , affectUniqueEnemy
  , checkUnique
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

import qualified Game.Model.Chakra as Chakra
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Slot as Slot
import           Util ((∈), (∉), commas)

resetToZero :: Int
resetToZero = -1000

hasFrom :: Ninja -> Text -> Ninja -> Bool
hasFrom user name = Ninja.has name $ Ninja.slot user

winFull :: WinType -> Int -> [Text] -> Goal
winFull winType reach chars = Reach reach Career desc $ Win winType chars
  where
    desc = toStrict . builderToLazy $
           "Win 10 matches with " ++ commas "and" chars ++ " together."

win :: Int -> [Text] -> Goal
win = winFull WinTotal

winConsecutive :: Int -> [Text] -> Goal
winConsecutive = winFull WinConsecutive

-- ACTION HOOKS

check :: (Ninja -> Ninja -> Ninja -> Bool) -> ActionHook
check f _ x y z = fromEnum $ f x y z

cure :: ActionHook
cure _ user target target' = fromEnum $
    allied user target
    && Ninja.numHelpful target' < Ninja.numHelpful target

damage :: ActionHook
damage _ user target target'
  | allied user target = 0
  | otherwise = max 0 $ health target - health target'

damageDuringStacks :: Text -> ActionHook
damageDuringStacks name _ user target target'
  | allied user target              = 0
  | health target' >= health target = 0
  | otherwise = Ninja.numStacks name (Ninja.slot user) user

damageWithStacks :: Text -> ActionHook
damageWithStacks name _ user target target'
  | allied user target              = 0
  | health target' >= health target = 0
  | otherwise = Ninja.numStacks name (Ninja.slot user) target

defend :: ActionHook
defend name user target target'
  | alive target = max 0 addedDefense
  | otherwise    = 0
  where
    userSlot     = Ninja.slot user
    addedDefense = Ninja.defenseAmount name userSlot target'
                   - Ninja.defenseAmount name userSlot target

demolish :: ActionHook
demolish _ user target target'
  | allied user target = 0
  | otherwise = max 0 $ Ninja.totalDefense target - Ninja.totalDefense target'

execute :: ActionHook
execute _ user target target' = fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && hasFrom user "executed" target'

heal :: ActionHook
heal _ user target target'
  | not $ alive target = 0
  | not $ allied user target = 0
  | otherwise = max 0 $ health target' - health target

kill :: ActionHook
kill _ user target target' = fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')

killAffected :: Text -> ActionHook
killAffected name _ user target target' = fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && hasFrom user name target

killDuring :: Text -> ActionHook
killDuring name _ user target target' = fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && hasOwn name user

interrupt :: ActionHook
interrupt _ user target target'
  | allied user target = 0
  | otherwise          = max 0 interrupted
  where
    numChannels n = length (Ninja.channels n) + length (Ninja.newChans n)
    interrupted   = numChannels target - numChannels target'

use :: ActionHook
use _ _ _ _ = 1

useDuring :: Text -> ActionHook
useDuring name _ user _ _ = fromEnum $ hasOwn name user

useDuringStacks :: Text -> ActionHook
useDuringStacks name _ user _ _ = Ninja.numStacks name (Ninja.slot user) user

-- CHAKRA HOOKS

deplete :: ChakraHook
deplete (_, chak) (_, chak') = max 0 $ Chakra.total chak' - Chakra.total chak

-- STORE HOOKS

affectUniqueEnemy :: StoreHook
affectUniqueEnemy = checkUnique \name user target ->
    alive target
    && not (allied user target)
    && hasFrom user name target

checkUnique :: (Text -> Ninja -> Ninja -> Bool) -> StoreHook
checkUnique f name user _ target' store
  | targetSlot ∈ store  = (store, 0)
  | f name user target' = (targetSlot `insertSet` store, 1)
  | otherwise           = (store, 0)
  where
    targetSlot = Slot.toInt $ Ninja.slot target'

compareUnique :: (Text -> Ninja -> Ninja -> Ninja -> Bool) -> StoreHook
compareUnique f name user target target' store
  | targetSlot ∈ store         = (store, 0)
  | f name user target target' = (targetSlot `insertSet` store, 1)
  | otherwise                  = (store, 0)
  where
    targetSlot = Slot.toInt $ Ninja.slot target'

damageUnique :: StoreHook
damageUnique = compareUnique \_ user target target' ->
    not (allied user target)
    && health target' < health target

killUnique :: StoreHook
killUnique = compareUnique \_ user target target' ->
    not (allied user target)
    && alive target
    && not (alive target')

killUniqueDuring :: Text -> StoreHook
killUniqueDuring name = compareUnique \_ user target target' ->
    not (allied user target)
    && alive target
    && not (alive target')
    && hasOwn name user

stunUnique :: StoreHook
stunUnique = checkUnique \name user target ->
    not (allied user target)
    && hasFrom user name target
    && stunned target

useUnique :: StoreHook
useUnique _ _ target _ store =
    (targetSlot `insertSet` store, fromEnum $ targetSlot ∉ store)
  where
    targetSlot = Slot.toInt $ Ninja.slot target

-- TRAP HOOKS

trapUnique :: TrapHook
trapUnique _ target store =
    (targetSlot `insertSet` store, fromEnum $ targetSlot ∉ store)
  where
    targetSlot = Slot.toInt $ Ninja.slot target

trapUniqueAlly :: TrapHook
trapUniqueAlly user target store
  | not $ allied user target = (store, 0)
  | otherwise                = trapUnique user target store

trapUniqueEnemy :: TrapHook
trapUniqueEnemy user target store
  | allied user target = (store, 0)
  | otherwise          = trapUnique user target store

-- TURN HOOKS

checkEnemyStatus :: Text -> TurnHook
checkEnemyStatus name player user _ target store
  | not $ allied player user = (store, 0)
  | allied user target       = (store, 0)
  | alive target             = (store, fromEnum $ hasFrom user name target)
  | otherwise                = (store, 0)

killWith :: Text -> TurnHook
killWith name player user target target' store = (store, ) . fromEnum $
    allied player user
    && not (allied user target)
    && alive target
    && not (alive target')
    && hasFrom user name target

maintain :: Text -> TurnHook
maintain name player user _ target store
  | Ninja.slot user /= Ninja.slot target = (store, 0)
  | not $ alive target                   = (store, resetToZero)
  | not $ hasOwn name user               = (store, resetToZero)
  | allied player user                   = (store, 1)
  | otherwise                            = (store, 0)

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
