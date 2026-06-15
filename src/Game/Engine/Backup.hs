
-- | 'Ninja' processing.
module Game.Engine.Backup
    ( Backup(..), create, restore
    ) where

import ClassyPrelude

import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Copy (Copy)
import           Game.Model.Destructible (Destructible)
import           Game.Model.ID (HasID, ID(ID))
import qualified Game.Model.ID as ID
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import qualified Game.Model.Skill as Skill
import           Game.Model.Status (Status)
import           Game.Model.Trap (Trap)

data Backup = Backup
    { skillID    :: ID
    , health     :: Int
    , cooldowns  :: HashMap Skill.Key Int
    , charges    :: HashMap Skill.Key Int
    , copies     :: Vector (Maybe Copy)
    , defense    :: [Destructible]
    , barrier    :: [Destructible]
    , statuses   :: [Status]
    , traps      :: [Trap]
    }

matchID :: ID -> ID -> Bool
matchID = (==) `on` \ID{user, owner} -> (user, owner)

create :: ID -> Ninja -> Backup
create skillID Ninja { health
                     , cooldowns
                     , charges
                     , copies
                     , defense
                     , barrier
                     , statuses
                     , traps
                     } = Backup
    { skillID
    , health
    , cooldowns
    , charges
    , copies
    , defense  = getAffected defense
    , barrier  = getAffected barrier
    , statuses = getAffected statuses
    , traps    = getAffected traps
    }
  where
    getAffected :: ∀ o. (IsSequence o, HasID (Element o)) => o -> o
    getAffected = filter $ matchID skillID . ID.from

restore :: Backup -> Ninja -> Ninja
restore Backup
    { skillID
    , health
    , cooldowns
    , charges
    , copies
    , defense
    , barrier
    , statuses
    , traps
    } n = Ninjas.processEffects
    n { N.health    = health
      , N.cooldowns = cooldowns
      , N.charges   = charges
      , N.copies    = copies
      , N.defense   = overwrite defense $ N.defense n
      , N.barrier   = overwrite barrier $ N.barrier n
      , N.statuses  = overwrite statuses $ N.statuses n
      , N.traps     = overwrite traps $ N.traps n
      }
  where
    getAffected :: ∀ o. (IsSequence o, HasID (Element o)) => o -> o
    getAffected = filter $ not . matchID skillID . ID.from
    overwrite :: ∀ o. (IsSequence o, HasID (Element o)) => o -> o -> o
    overwrite backup current = backup ++ getAffected current
