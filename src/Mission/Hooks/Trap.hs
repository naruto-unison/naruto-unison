{-# OPTIONS_HADDOCK prune #-}

module Mission.Hooks.Trap
  ( TrapHook, TriggerHook
  , trapUnique, trapUniqueAlly, trapUniqueEnemy
  ) where

import ClassyPrelude

import           Class.Parity (allied)
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as N
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Mission.Store (Store)
import           Util ((∉))

type TrapHook = Slot  -- ^ User.
             -> Ninja -- ^ Target after triggering trap.
             -> Store
             -> (Store, Int)

-- | Used in 'HookTrigger'.
type TriggerHook = Ninja -- ^ User.
                -> Bool

-- | Tallies the number of unique targets who trigger a trap.
trapUnique :: TrapHook
trapUnique _ target store = ( insertSet targetSlot store
                            , fromEnum $ targetSlot ∉ store
                            )
  where
    targetSlot = Slot.toInt $ N.slot target

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
