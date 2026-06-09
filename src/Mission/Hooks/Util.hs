{-# OPTIONS_HADDOCK prune #-}

module Mission.Hooks.Util
  ( hasFrom
  , hasOwn
  , toID
  ) where

import ClassyPrelude

import           Game.Model.ID (ID(ID))
import qualified Game.Model.ID
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import           Game.Model.Slot (Slot)

--  | True if target has a 'Status' from the user with matching 'Status.name'.
hasFrom :: Ninja -> Text -> Ninja -> Bool
hasFrom Ninja{slot} name = N.has $ toID name slot

--  | True if user has a 'Status' from the user with matching 'Status.name'.
hasOwn :: Text -> Ninja -> Bool
hasOwn name n@Ninja{slot} = N.has (toID name slot) n

toID :: Text -> Slot -> ID
toID name user = ID { user, owner = user, name }
