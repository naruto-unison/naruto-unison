{-# OPTIONS_HADDOCK prune #-}

module Mission.Hooks.Turn
  ( TurnHook
  , checkEnemyStatus
  , killWith
  , maintain, maintainOnAlly
  ) where

import ClassyPrelude
import Class.Parity (allied)

import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import           Game.Model.Player (Player)
import           Mission.Hooks.Util (boolean, hasFrom, toID)
import           Mission.Progress (Store)
import qualified Mission.Progress as Progress
import           Util ((∈))

-- | Used in 'HookTurn'.
type TurnHook = Player -- ^ Whose turn it is.
             -> Ninja  -- User.
             -> Ninja  -- ^ Target at beginning of turn.
             -> Ninja  -- ^ Target at end of turn.
             -> Store
             -> (Store, Int)

-- | 1 if an enemy has a @Status@ at the end of the turn, otherwise 0.
checkEnemyStatus :: Text -> TurnHook
checkEnemyStatus name player user _ target store
  | not $ allied player user = (store, 0)
  | allied user target       = (store, 0)
  | N.alive target           = (store, boolean $ hasFrom user name target)
  | otherwise                = (store, 0)

-- | 1 if an enemy dies with a @Status@ at the end of the turn, otherwise 0.
killWith :: Text -> TurnHook
killWith name player user target target' store = (store, ) . boolean
    $ allied player user
    && not (allied user target)
    && N.alive target
    && not (N.alive target')
    && hasFrom user name target

-- | Increases while the target maintains a @Status@.
-- Resets to 0 if they lose the @Status@.
maintain :: Text -> TurnHook
maintain name player user@Ninja{slot} _ target@Ninja{slot = targetSlot} store
  | slot /= targetSlot        = (store, 0)
  | not $ N.alive target      = (store, Progress.resetToZero)
  | not $ N.has statusID user = (store, Progress.resetToZero)
  | allied player user        = (store, 1)
  | otherwise                 = (store, 0)
  where
    statusID = toID name slot

-- | 'maintain' restricted to the user's team.
maintainOnAlly :: Text -> TurnHook
maintainOnAlly name player user _ target@Ninja{slot} store
  | not $ allied user target       = (store, 0)
  | not $ N.alive target           = (deleteSet slot store, reset)
  | not $ hasFrom user name target = (deleteSet slot store, reset)
  | otherwise                      = ( insertSet slot store
                                     , boolean $ allied player user
                                     )
  where
    reset
      | slot ∈ store = Progress.resetToZero
      | otherwise    = 0
