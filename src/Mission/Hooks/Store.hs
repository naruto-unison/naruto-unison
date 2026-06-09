{-# OPTIONS_HADDOCK prune #-}

module Mission.Hooks.Store
  ( StoreHook
  , checkUnique
  , affectUniqueEnemy
  , damageUnique
  , killUnique, killUniqueDuring
  , stunUnique
  , useUnique
  ) where

import ClassyPrelude

import           Class.Parity (allied)
import qualified Game.Engine.Effects as Effects
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import qualified Game.Model.Slot as Slot
import           Mission.Hooks.Util (hasFrom, toID)
import           Mission.Store (Store)
import           Util ((∈), (∉))

type StoreHook = Text  -- ^ Skill name.
              -> Ninja -- ^ User.
              -> Ninja -- ^ Target before action.
              -> Ninja -- ^ Target after action.
              -> Store
              -> (Store, Int)
-- | Like 'check', but once it succeeds, it cannot succeed again for that
-- target.
checkUnique :: (Text -> Ninja -> Ninja -> Bool) -> StoreHook
checkUnique f name user _ target' store
  | targetSlot ∈ store  = (store, 0)
  | f name user target' = (insertSet targetSlot store, 1)
  | otherwise           = (store, 0)
  where
    targetSlot = Slot.toInt $ N.slot target'

-- | Like 'checkUnique', but using the target's state before the action as well.
compareUnique :: (Text -> Ninja -> Ninja -> Ninja -> Bool) -> StoreHook
compareUnique f name user target target' store
  | targetSlot ∈ store         = (store, 0)
  | f name user target target' = (insertSet targetSlot store, 1)
  | otherwise                  = (store, 0)
  where
    targetSlot = Slot.toInt $ N.slot target'

-- | Apply a @Status@ to an enemy.
affectUniqueEnemy :: StoreHook
affectUniqueEnemy = checkUnique \name user target ->
    N.alive target
    && not (allied user target)
    && hasFrom user name target

-- | Damage an enemy.
damageUnique :: StoreHook
damageUnique = compareUnique \_ user target target' ->
    not (allied user target)
    && N.health target' < N.health target

-- | Kill an enemy.
killUnique :: StoreHook
killUnique = compareUnique \_ user target target' ->
    not (allied user target)
    && N.alive target
    && not (N.alive target')

-- | Kill an enemy while the user is affected by a @Status@.
killUniqueDuring :: Text -> StoreHook
killUniqueDuring name = compareUnique \_ user@Ninja{slot} target target' ->
    not (allied user target)
    && N.alive target
    && not (N.alive target')
    && N.numStacks (toID name slot) user /= 0

-- | Stun an enemy.
stunUnique :: StoreHook
stunUnique = checkUnique \name user target ->
    not (allied user target)
    && hasFrom user name target
    && Effects.stunned target

-- | Use an action on a target.
useUnique :: StoreHook
useUnique _ _ target _ store = ( insertSet targetSlot store
                               , fromEnum $ targetSlot ∉ store
                               )
  where
    targetSlot = Slot.toInt $ N.slot target
