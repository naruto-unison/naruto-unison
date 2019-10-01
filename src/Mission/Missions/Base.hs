module Mission.Missions.Base
  ( module Import
  , check
  , maintain
  , affectAll
  , killAffected
  , killDuring
  , hasFrom
  , stunned
  ) where

import ClassyPrelude as Import
import Class.Parity as Import (allied)
import Game.Model.Ninja as Import (alive, hasOwn, health)
import Mission.Goal as Import
import Data.IntSet as Import (size)

import qualified Game.Engine.Effects as Effects
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Slot as Slot

hasFrom :: Ninja -> Text -> Ninja -> Bool
hasFrom user name = Ninja.has name $ Ninja.slot user

check :: (Ninja -> Ninja -> Ninja -> Bool) -> HookFunc
check f x y z store = (store, fromEnum $ f x y z)

stunned :: Ninja -> Bool
stunned n = not . null $ Effects.stun n

killAffected :: Text -> HookFunc
killAffected name user target target' store = (store, ) . fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && hasFrom user name target

killDuring :: Text -> HookFunc
killDuring name user target target' store = (store, ) . fromEnum $
    not (allied user target)
    && alive target
    && not (alive target')
    && hasOwn name user

maintain :: Int -> Text -> TurnFunc
maintain goal name user target store
  | Ninja.slot user == Ninja.slot target =
      (singleton count, fromEnum $ count >= goal)
  | otherwise = (store, 0)
  where
    count
      | hasOwn name user = maybe 1 (+1) $ headMay store
      | otherwise        = 0

affectAll :: (Ninja -> Ninja -> Bool) -> HookFunc
affectAll f user _ target' store = (store', fromEnum $ size store' >= 3)
  where
    targetSlot = Slot.toInt $ Ninja.slot target'
    store'
      | f user target' = insertSet targetSlot store
      | otherwise      = store
