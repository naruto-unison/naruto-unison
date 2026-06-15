{-# OPTIONS_HADDOCK prune #-}

module Mission.Hooks.Util
  ( hasFrom
  , hasOwn
  , toID
  , used
  ) where

import ClassyPrelude

import           Game.Model.ID (ID(ID))
import qualified Game.Model.ID
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)

--  | True if target has a 'Status' from the user with matching 'Status.name'.
hasFrom :: Ninja -> Text -> Ninja -> Bool
hasFrom Ninja{slot} name = N.has $ toID name slot

--  | True if user has a 'Status' from the user with matching 'Status.name'.
hasOwn :: Text -> Ninja -> Bool
hasOwn name n@Ninja{slot} = N.has (toID name slot) n

used :: Text -> Ninja -> Bool
used skillName Ninja{N.lastSkill = Just Skill{name}} = skillName == name
used _ _ = False

toID :: Text -> Slot -> ID
toID name user = ID { user, owner = user, name }
