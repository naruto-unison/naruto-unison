{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass       #-}

-- | 'Ninja' is defined in "Game.Model.Internal" as the basis for the majority
-- of functions in other @Game.Model@ modules, but its JSON encoding requires
-- the use of those functions. Unfortunately, an orphan instance is the least
-- convoluted way to achieve this.
module OrphanInstances.Ninja (Face(..)) where

import ClassyPrelude

import Data.Aeson ((.=), ToJSON(..), object)

import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import qualified Game.Model.Effect as Effect
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Requirement (Requirement(..))
import qualified Game.Model.Requirement as Requirement
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Game.Model.Status (Status)
import qualified Game.Model.Status as Status
import qualified Game.Model.Trap as Trap
import           Util ((∈), (∉))

-- | From 'Effect.Face'. Used only as an encoding intermediary.
data Face = Face { icon :: Text
                 , user :: Slot
                 } deriving (Eq, Show, Read, Generic, ToJSON)

-- | Generates a 'Face' from the most recent 'Effect.Face' in 'Ninja.statuses'.
statusFace :: Status -> Face
statusFace x = Face (toLower $ Status.name x) $ Status.user x

instance ToJSON Ninja where
    toJSON n = object
        [ "slot"      .= Ninja.slot n
        , "character" .= Character.ident (Ninja.character n)
        , "health"    .= Ninja.health n
        , "cooldowns" .= Ninja.cooldowns n
        , "charges"   .= Ninja.charges n
        , "defense"   .= Ninja.defense n
        , "barrier"   .= Ninja.barrier n
        , "statuses"  .= filter ((Hidden ∉) . Status.classes) (Ninja.statuses n)
        , "copies"    .= Ninja.copies n
        , "channels"  .= Ninja.channels n
        , "traps"     .= filter ((Hidden ∉) . Trap.classes) (Ninja.traps n)
        , "face"      .= (statusFace <$> mFace)
        , "lastSkill" .= Ninja.lastSkill n
        , "skills"    .= (usable <$> Ninjas.skills n)
        ]
      where
        mFace = find ((Effect.Face ∈) . Status.effects) $ Ninja.statuses n
        usable skill = skill { Skill.require = fulfill $ Skill.require skill }
        fulfill req@HasI{}
          | Requirement.succeed req (Ninja.slot n) n = Usable
          | otherwise                                = Unusable
        fulfill x = x
