{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja
import           Game.Model.Requirement (Requirement(..))
import qualified Game.Model.Requirement as Requirement
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Game.Model.Status (Status(Status))
import qualified Game.Model.Status as Status
import qualified Game.Model.Trap as Trap
import           Util ((∈), (∉))

-- | From 'Effect.Face'. Used only as an encoding intermediary.
data Face = Face
    { icon :: Text
    , user :: Slot
    } deriving (Eq, Show, Read, Generic)

instance ToJSON Face

-- | Generates a 'Face' from the most recent 'Effect.Face' in 'statuses'.
statusFace :: Status -> Face
statusFace Status{name, user} = Face (toLower name) user

instance ToJSON Ninja where
    toJSON n@Ninja { barrier
                   , channels
                   , character
                   , charges
                   , cooldowns
                   , copies
                   , defense
                   , health
                   , lastSkill
                   , slot
                   , statuses
                   , traps
                   } = object
        [ "slot"      .= slot
        , "character" .= Character.ident character
        , "health"    .= health
        , "cooldowns" .= cooldowns
        , "charges"   .= charges
        , "defense"   .= defense
        , "barrier"   .= barrier
        , "statuses"  .= foldStats
                         (filter ((Hidden ∉) . Status.classes) statuses)
        , "copies"    .= copies
        , "channels"  .= channels
        , "traps"     .= filter ((Hidden ∉) . Trap.classes) traps
        , "face"      .= (statusFace <$> mFace)
        , "lastSkill" .= lastSkill
        , "skills"    .= (usable <$> Ninjas.skills n)
        ]
      where
        mFace = find ((Effect.Face ∈) . Status.effects) statuses
        usable skill = skill { Skill.require = fulfill $ Skill.require skill }
        fulfill req@HasI{}
          | Requirement.succeed req slot n = Usable
          | otherwise                      = Unusable
        fulfill x = x
        foldStats xs       = foldStat <$> group (sort xs)
        foldStat   (x:|[]) = x
        foldStat xs@(x:|_) = x { Status.amount = sum $ Status.amount <$> xs }
