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
import           Game.Model.Ninja (Ninja(..))
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

-- | Generates a 'Face' from the most recent 'Effect.Face' in 'statuses'.
statusFace :: Status -> Face
statusFace x = Face (toLower $ Status.name x) $ Status.user x

instance ToJSON Ninja where
    toJSON n = object
        [ "slot"      .= slot n
        , "character" .= Character.ident (character n)
        , "health"    .= health n
        , "cooldowns" .= cooldowns n
        , "charges"   .= charges n
        , "defense"   .= defense n
        , "barrier"   .= barrier n
        , "statuses"  .= foldStats
                         (filter ((Hidden ∉) . Status.classes) (statuses n))
        , "copies"    .= copies n
        , "channels"  .= channels n
        , "traps"     .= filter ((Hidden ∉) . Trap.classes) (traps n)
        , "face"      .= (statusFace <$> mFace)
        , "lastSkill" .= lastSkill n
        , "skills"    .= (usable <$> Ninjas.skills n)
        ]
      where
        mFace = find ((Effect.Face ∈) . Status.effects) $ statuses n
        usable skill = skill { Skill.require = fulfill $ Skill.require skill }
        fulfill req@HasI{}
          | Requirement.succeed req (slot n) n = Usable
          | otherwise                          = Unusable
        fulfill x = x
        foldStats xs       = foldStat <$> group (sort xs)
        foldStat   (x:|[]) = x
        foldStat xs@(x:|_) = x { Status.amount = sum $ Status.amount <$> xs }
