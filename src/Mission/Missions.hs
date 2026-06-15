-- | Character missions, which users complete in order to unlock new characters.
module Mission.Missions
  ( list
  , map
  , characterMissions
  , consecutiveWins
  ) where

import ClassyPrelude hiding ((\\), map)

import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character as Character
import           Mission.Goal (Goal(Reach), Mission(Mission))
import qualified Mission.Goal as Goal
import           Mission.Objective (Objective(..), WinType(..))
import           Util (lazyMapFromKeyed)

import qualified Mission.Missions.Shippuden

-- | Uses 'Character.clean' to turn character names in 'Objective's into
-- 'Character.ident' format.
clean :: Mission -> Mission
clean (Mission char goals) = Mission (Character.clean char) $ cleanup <$> goals
  where
    cleanup goal = goal { Goal.objective = f $ Goal.objective goal }
    f (Win winType idents)        = Win winType $ Character.clean <$> idents
    f (HookAction ident skill fn) = HookAction  (Character.clean ident) skill fn
    f (HookChakra ident skill fn) = HookChakra  (Character.clean ident) skill fn
    f (HookStore ident skill fn)  = HookStore   (Character.clean ident) skill fn
    f (HookTrap ident trap fn)    = HookTrap    (Character.clean ident) trap  fn
    f (HookTrigger ident trig fn) = HookTrigger (Character.clean ident) trig  fn
    f (HookTurn ident fn)         = HookTurn    (Character.clean ident)       fn
    f (Consecutive ident skills)  = Consecutive (Character.clean ident)
                                  $ sort skills

-- | All missions.
list :: [Mission]
list = clean <$> Mission.Missions.Shippuden.missions
{-# NOINLINE list #-}

-- | Map of all missions objectives, from 'Character.ident's to 'Goal.goal's.
map :: HashMap Text (Seq Goal)
map = lazyMapFromKeyed (Goal.char, Goal.goals) list
{-# NOINLINE map #-}

-- | Obtains all of a character's missions from 'list'.
characterMissions :: Character -> [Mission]
characterMissions Character{ident} =
    filter (any (Goal.belongsTo ident) . Goal.goals) list

-- | All 'Character.ident's in 'list' paired with 'WinConsecutive' indices
-- within their missions.
consecutiveWins :: [(Text, Int)]
consecutiveWins = [ (char, i) | Mission{char, goals} <- list,
                                (i, Reach{objective = Win WinConsecutive _})
                                    <- zip [0..] $ toList goals ]
{-# NOINLINE consecutiveWins #-}
