module Handler.Play.Tracker
  ( Tracker
  , fromInfo
  , empty
  , Progress(..)
  , unsafeFreeze
  , trackAction
  , trackTurn
  ) where

import ClassyPrelude hiding (empty)

import           Control.Monad.ST (ST)
import qualified Data.MultiMap as MultiMap
import           Data.MultiMap (MultiMap, (!))
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import           Data.Vector.Mutable (MVector)

import           Util ((!!))
import qualified Class.Parity as Parity
import qualified Game.Model.Character as Character
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Slot as Slot
import           Game.Model.Slot (Slot)
import qualified Handler.Play.GameInfo as GameInfo
import           Handler.Play.GameInfo (GameInfo)
import qualified Mission.Missions as Missions
import qualified Mission.Goal as Goal
import           Mission.Goal (Mission, TurnFunc, HookFunc, Objective(..), Store)
import           Mission.Progress (Progress(Progress))

missionKeys :: Text -> Mission -> [Int -> Progress]
missionKeys name mission =
    Progress (Goal.char mission) . fst <$> objectives
  where
    objectives = filter (Goal.belongsTo name . snd) .
                 zip [0..] . toList $ Goal.goals mission

data Track s = Track { slot     :: Slot
                     , key      :: Vector (Int -> Progress)
                     , hooks    :: MultiMap Text (Int, HookFunc)
                     , turns    :: Vector (Int, TurnFunc)
                     , store    :: MVector s IntSet
                     , progress :: MVector s Int
                     }

track :: ∀ s. Track s -> Int -> (Store -> (Store, Int)) -> ST s ()
track x i f = do
    (store', progress') <- f <$> MVector.unsafeRead (store x) i
    MVector.unsafeWrite (store x) i store'
    MVector.unsafeModify (progress x) (+ progress') i

trackTurn1 :: ∀ s. Vector Ninja -> Track s -> ST s ()
trackTurn1 ns x = sequence_ $ tracker <$> ns <*> turns x
  where
    user = ns !! Slot.toInt (slot x)
    tracker n (i, f) = track x i $ f user n

trackHooks1 :: ∀ s. Text -> [(Ninja, Ninja)] -> Track s -> ST s ()
trackHooks1 skill ns x = sequence_ $ tracker <$> ns <*> hooks x ! skill
  where
    user = snd $ ns !! Slot.toInt (slot x)
    tracker (n, n') (i, f) = track x i $ f user n n'

new :: ∀ s. Ninja -> ST s (Track s)
new n = do
    store    <- MVector.replicate Slot.teamSize mempty
    progress <- MVector.replicate Slot.teamSize 0
    return Track { slot  = Ninja.slot n
                 , key   = fromList $ missionKeys name =<< missions
                 , hooks = MultiMap.fromList $ mapMaybe hook objectives
                 , turns = fromList $ mapMaybe turn objectives
                 , store
                 , progress
                 }
  where
    name       = Character.format $ Ninja.character n
    missions   = Missions.characterMissions name
    objectives = [(i, Goal.objective x) | i <- [0..]
                                        | mission <- missions
                                        , x       <- toList $ Goal.goals mission
                                        , Goal.belongsTo name x]
    hook (i, Hook _ skill func) = Just (skill, (i, func))
    hook _ = Nothing
    turn (i, HookTurn _ func) = Just (i, func)
    turn _ = Nothing

newtype Tracker s = Tracker (Vector (Track s))

-- | The mutable elements of the Tracker may not be used after this operation.
unsafeFreeze :: ∀ s. Tracker s -> ST s (Vector Progress)
unsafeFreeze (Tracker xs) = concat <$> traverse freeze xs
  where
    freeze x = (key x <*>) <$> Vector.unsafeFreeze (progress x)

fromInfo :: ∀ s. GameInfo -> ST s (Tracker s)
fromInfo info = Tracker <$> mapM new ninjas
  where
    player = GameInfo.player info
    ninjas = Parity.half player $ GameInfo.ninjas info

trackTurn :: ∀ s. Tracker s -> Vector Ninja -> ST s ()
trackTurn (Tracker xs) ns = traverse_ (trackTurn1 ns) xs

trackAction :: ∀ s. Tracker s -> Text -> Vector Ninja -> Vector Ninja -> ST s ()
trackAction (Tracker xs) skill ns ns' = traverse_ (trackHooks1 skill zipped) xs
  where
    zipped = toList $ zip ns ns'

empty :: ∀ s. Tracker s
empty = Tracker mempty
