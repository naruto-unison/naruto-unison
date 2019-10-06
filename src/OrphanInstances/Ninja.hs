{-# OPTIONS_GHC -fno-warn-orphans #-}

module OrphanInstances.Ninja () where

import ClassyPrelude

import Data.Aeson ((.=), ToJSON(..), object)

import qualified Game.Engine.Cooldown as Cooldown
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Requirement (Requirement(..))
import qualified Game.Model.Requirement as Requirement
import qualified Game.Model.Skill as Skill
import qualified Game.Model.Status as Status
import qualified Game.Model.Trap as Trap
import           Util ((∉))

instance ToJSON Ninja where
    toJSON n = object
        [ "slot"      .= Ninja.slot n
        , "character" .= Character.format (Ninja.character n)
        , "health"    .= Ninja.health n
        , "defense"   .= Ninja.defense n
        , "barrier"   .= Ninja.barrier n
        , "statuses"  .= filter ((Hidden ∉) . Status.classes) (Ninja.statuses n)
        , "charges"   .= Ninja.charges n
        , "cooldowns" .= Cooldown.active n
        , "variants"  .= Ninja.variants n
        , "copies"    .= Ninja.copies n
        , "channels"  .= Ninja.channels n
        , "traps"     .= filter ((Hidden ∉) . Trap.classes) (Ninja.traps n)
        , "face"      .= Ninja.face n
        , "lastSkill" .= Ninja.lastSkill n
        , "skills"    .= (usable <$> Ninjas.skills n)
        ]
      where
        usable skill = skill { Skill.require = fulfill $ Skill.require skill }
        fulfill req@HasI{}
          | Requirement.succeed req (Ninja.slot n) n = Usable
          | otherwise                                = Unusable
        fulfill x = x
