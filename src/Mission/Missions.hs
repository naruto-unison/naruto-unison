-- | Character missions, which users complete in order to unlock new characters.
module Mission.Missions
  ( list
  , map
  , characterMissions
  , consecutiveWins
  ) where

import ClassyPrelude hiding ((\\), map)

import           Game.Model.Character (Character)
import qualified Game.Model.Character as Character
import           Mission.Goal (Goal(..), Mission(..), Objective(..), WinType(..))
import qualified Mission.Goal as Goal
import           Util (mapFromKeyed)

import qualified Mission.Missions.Shippuden

-- | Uses 'Character.clean' to turn character names in 'Objective's into
-- 'Character.ident' format.
clean :: Mission -> Mission
clean (Mission char goals) =
    Mission (Character.clean char) $ cleanGoal <$> goals
  where
    cleanGoal goal = goal { objective = f $ objective goal }
    f (Win winType names)        = Win winType $ Character.clean <$> names
    f (HookAction name skill fn) = HookAction (Character.clean name) skill fn
    f (HookChakra name skill fn) = HookChakra (Character.clean name) skill fn
    f (HookStore name skill fn)  = HookStore (Character.clean name) skill fn
    f (HookTrap name trap fn)    = HookTrap (Character.clean name) trap fn
    f (HookTrigger name trig fn) = HookTrigger (Character.clean name) trig fn
    f (HookTurn name fn)         = HookTurn (Character.clean name) fn
    f (Consecutive x skills)     = Consecutive (Character.clean x) $ sort skills

-- | All missions.
list :: [Mission]
list = clean <$> Mission.Missions.Shippuden.missions
{-# NOINLINE list #-}

-- | Map of all missions objectives, from 'Character.ident's to 'Goal.goal's.
map :: HashMap Text (Seq Goal)
map = mapFromKeyed (Goal.char, Goal.goals) list
{-# NOINLINE map #-}

-- | Obtains all of a character's missions from 'list'.
characterMissions :: Character -> [Mission]
characterMissions (Character.ident -> name) =
    filter (any (Goal.belongsTo name) . Goal.goals) list

-- | List of 'Character.ident's paired with 'WinConsecutive' indices within
-- their missions.
consecWins :: Mission -> [(Text, Int)]
consecWins x = (Goal.char x, ) . fst <$> filter consec indices
  where
    indices = zip [0..] . toList $ Goal.goals x
    consec (_, Reach{objective = Win WinConsecutive _}) = True
    consec _                                            = False

-- | All 'Character.ident's in 'list' paired with 'WinConsecutive' indices
-- within their missions.
consecutiveWins :: [(Text, Int)]
consecutiveWins = consecWins =<< list
{-# NOINLINE consecutiveWins #-}
