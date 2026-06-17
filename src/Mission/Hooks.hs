-- | Tracks progress on character missions during a game.
module Mission.Hooks
  ( Hooks(..)
  , forCharacter
  ) where

import ClassyPrelude hiding (empty)

import qualified Game.Characters as Characters
import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character as Character
import           Game.Model.Trigger (Trigger)
import           Mission.Goal (Goal, Mission(Mission))
import qualified Mission.Goal as Goal
import           Mission.Hooks.Action (ActionHook)
import           Mission.Hooks.Chakra (ChakraHook)
import           Mission.Hooks.Store (StoreHook)
import           Mission.Hooks.Trap (TrapHook, TriggerHook)
import           Mission.Hooks.Turn (TurnHook)
import qualified Mission.Missions as Missions
import           Mission.Objective (Objective(..))
import           Mission.Progress (Progress(Progress))
import           Util ((!), lazyMapFromKeyed)

data Hooks = Hooks
    { key      :: [(Int -> Progress)]
    , actions  :: HashMap Text [(Int, ActionHook)]
    , chakras  :: HashMap Text [(Int, ChakraHook)]
    , stores   :: HashMap Text [(Int, StoreHook)]
    , traps    :: HashMap Text [(Int, TrapHook)]
    , triggers :: HashMap Trigger [(Int, TriggerHook)]
    , turns    :: [(Int, TurnHook)]
    , consecs  :: [(Int, [Text])]
    , goals    :: Vector Goal
    }

forCharacter :: Text -> Hooks
forCharacter ident = forAllCharacters ! ident

forAllCharacters :: HashMap Text Hooks
forAllCharacters = lazyMapFromKeyed (Character.ident, new) Characters.list
{-# NOINLINE forAllCharacters #-}

new :: Character -> Hooks
new character@Character{ident} = addObjectives objectives Hooks
    { key      = missionKeys
    , actions  = mempty
    , chakras  = mempty
    , stores   = mempty
    , traps    = mempty
    , triggers = mempty
    , turns    = mempty
    , consecs  = mempty
    , goals    = fromList allGoals
    }
  where
    missions = Missions.characterMissions character
    missionKeys = [ Progress char i | Mission{char, goals} <- missions,
                                      (i, goal) <- zip [0..] $ toList goals,
                                      Goal.belongsTo ident goal ]
    allGoals = [ goal | Mission{goals} <- missions,
                        goal           <- toList goals,
                        Goal.belongsTo ident goal ]
    objectives = Goal.objective <$> allGoals

insertMultimap :: ∀ o. (IsMap o, IsSequence (MapValue o))
               => ContainerKey o -> Element (MapValue o) -> o -> o
insertMultimap key value = alterMap (Just . update) key
  where
    update :: Maybe (MapValue o) -> MapValue o
    update Nothing     = singleton value
    update (Just list) = value :< list

addObjectives :: [Objective] -> Hooks -> Hooks
addObjectives objectives track = foldl' go track $ zip [0..] objectives
  where
    go x (i, Consecutive _ skills) =
        x { consecs = (i, skills) : x.consecs }
    go x (i, HookAction _ skill func) =
        x { actions = insertMultimap skill (i, func) x.actions }
    go x (i, HookChakra _ skill func) =
        x { chakras = insertMultimap skill (i, func) x.chakras }
    go x (i, HookStore _ skill func) =
        x { stores = insertMultimap skill (i, func) x.stores }
    go x (i, HookTrap _ trap func) =
        x { traps = insertMultimap trap (i, func) x.traps }
    go x (i, HookTrigger _ trigger func) =
        x { triggers = insertMultimap trigger (i, func) x.triggers }
    go x (i, HookTurn _ func) =
        x { turns = (i, func) : x.turns }
    go x (_, Win{}) =
        x
