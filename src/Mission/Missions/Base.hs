module Mission.Missions.Base
  ( module Import
  , check
  , checkEnemyStatus
  , checkUnique
  , damageWith
  , defend
  , killAffected
  , killDuring
  , hasFrom
  , maintain
  , stunned
  ) where

import ClassyPrelude as Import
import Class.Parity as Import (allied)
import Data.IntSet as Import (size)
import Game.Model.Ninja as Import (alive, hasOwn, health)
import Mission.Goal as Import

import qualified Game.Engine.Effects as Effects
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Slot as Slot

resetToZero :: Int
resetToZero = -1000

hasFrom :: Ninja -> Text -> Ninja -> Bool
hasFrom user name = Ninja.has name $ Ninja.slot user

check :: (Ninja -> Ninja -> Ninja -> Bool) -> HookFunc
check f x y z store = (store, fromEnum $ f x y z)

checkUnique :: (Ninja -> Ninja -> Bool) -> HookFunc
checkUnique f user _ target' store
  | targetSlot `elem` store = (store, 0)
  | f user target'          = (targetSlot `insertSet` store, 1)
  | otherwise               = (store, 0)
  where
    targetSlot = Slot.toInt $ Ninja.slot target'

stunned :: Ninja -> Bool
stunned n = not . null $ Effects.stun n

checkEnemyStatus :: Text -> TurnFunc
checkEnemyStatus name user target store
  | allied user target = (store, 0)
  | otherwise          = (store, fromEnum $ hasFrom user name target)

killAffected :: Text -> HookFunc
killAffected name user target target' store = (store, ) . fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && hasFrom user name target

defend :: Text -> HookFunc
defend name user target target' store = (store, max 0 addedDefense)
  where
    userSlot     = Ninja.slot user
    addedDefense = Ninja.defenseAmount name userSlot target'
                   - Ninja.defenseAmount name userSlot target

killDuring :: Text -> HookFunc
killDuring name user target target' store = (store, ) . fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && hasOwn name user

maintain :: Text -> TurnFunc
maintain name user target store
  | Ninja.slot user /= Ninja.slot target = (store, 0)
  | hasOwn name user                     = (store, 1)
  | otherwise                            = (store, resetToZero)

damageWith :: Int -> Text -> HookFunc
damageWith amount name user target target' store
  | Ninja.health target' >= Ninja.health target             = (store, 0)
  | Ninja.numStacks name (Ninja.slot user) target >= amount = (store, 1)
  | otherwise                                               = (store, 0)
