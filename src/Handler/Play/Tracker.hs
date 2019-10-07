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
import           Data.List (nub)
import           Data.MultiMap (MultiMap, (!))
import qualified Data.MultiMap as MultiMap
import qualified Data.Vector as Vector
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector

import qualified Class.Parity as Parity
import qualified Game.Model.Character as Character
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Handler.Play.GameInfo (GameInfo)
import qualified Handler.Play.GameInfo as GameInfo
import           Mission.Goal (Mission, TurnFunc, HookFunc, Objective(..), Span(..), Store)
import qualified Mission.Goal as Goal
import qualified Mission.Missions as Missions
import           Mission.Progress (Progress(Progress))
import           Util ((!!))

missionKeys :: Text -> Mission -> [Int -> Progress]
missionKeys name mission =
    Progress (Goal.char mission) . fst <$> objectives
  where
    objectives = filter (Goal.belongsTo name . snd) .
                 zip [0..] . toList $ Goal.goals mission

data Track s = Track { slot     :: Slot
                     , key      :: [(Int -> Progress)]
                     , hooks    :: MultiMap Text (Int, HookFunc)
                     , turns    :: [(Int, TurnFunc)]
                     , useAll   :: [Int]
                     , resets   :: [Bool]
                     , skills   :: STRef s [Text]
                     , store    :: MVector s IntSet
                     , progress :: MVector s Int
                     }

reset :: ∀ s. Track s -> ST s ()
reset x = traverse_ f . zip [0..] $ resets x
  where
    f (i, True) = MVector.unsafeWrite (progress x) i 0
    f _         = return ()

track :: ∀ s. Track s -> Int -> (Store -> (Store, Int)) -> ST s ()
track x i f = do
    (store', progress') <- f <$> MVector.unsafeRead (store x) i
    MVector.unsafeWrite (store x) i store'
    MVector.unsafeModify (progress x) (max 0 . (+ progress')) i

trackTurn1 :: ∀ s. [Ninja] -> Track s -> ST s ()
trackTurn1 ns x = do
    sequence_ $ tracker <$> ns <*> turns x
    reset x
    modifyRef' (skills x) $ drop 1
  where
    user = ns !! Slot.toInt (slot x)
    tracker n (i, f) = track x i $ f user n

trackHooks1 :: ∀ s. Text -> [(Ninja, Ninja)] -> Track s -> ST s ()
trackHooks1 skill ns x = do
    sequence_ $ tracker <$> ns <*> hooks x ! skill
    modifyRef' (skills x) (++ [skill])
    used <- readRef $ skills x
    when (length (nub used) >= Ninja.skillSize) .
        traverse_ (MVector.unsafeModify (progress x) (+1)) $ useAll x
  where
    user = snd $ ns !! Slot.toInt (slot x)
    tracker (n, n') (i, f) = track x i $ f user n n'

new :: ∀ s. Ninja -> ST s (Track s)
new n = do
    skills   <- newRef mempty
    store    <- MVector.replicate (length objectives) mempty
    progress <- MVector.replicate (length objectives) 0
    return Track { slot   = Ninja.slot n
                 , key    = missionKeys name =<< missions
                 , hooks  = MultiMap.fromList $ mapMaybe hook objectives
                 , turns  = mapMaybe turn objectives
                 , useAll = mapMaybe useAll objectives
                 , resets = (Turn ==) . Goal.spanning <$> goals
                 , skills
                 , store
                 , progress
                 }
  where
    char       = Ninja.character n
    name       = Character.ident $ Ninja.character n
    missions   = Missions.characterMissions char
    goals      = [x | mission <- missions
                    , x       <- toList $ Goal.goals mission
                    , Goal.belongsTo name x]
    objectives = zip [0..] $ Goal.objective <$> goals
    hook (i, Hook _ skill func) = Just (skill, (i, func))
    hook _                      = Nothing
    turn (i, HookTurn _ func) = Just (i, func)
    turn _                    = Nothing
    useAll (i, UseAllSkills _) = Just i
    useAll _                   = Nothing

newtype Tracker s = Tracker (Vector (Track s))

-- | The mutable elements of the Tracker may not be used after this operation.
unsafeFreeze :: ∀ s. Tracker s -> ST s [Progress]
unsafeFreeze (Tracker xs) = concat <$> traverse freeze xs
  where
    freeze x = (zipWith ($) $ key x) . toList <$> Vector.unsafeFreeze (progress x)

fromInfo :: ∀ s. GameInfo -> ST s (Tracker s)
fromInfo info = Tracker <$> mapM new ninjas
  where
    player = GameInfo.player info
    ninjas = fromList . Parity.half player $ GameInfo.ninjas info

trackTurn :: ∀ s. Tracker s -> [Ninja] -> ST s ()
trackTurn (Tracker xs) ns = traverse_ (trackTurn1 ns) xs

trackAction :: ∀ s. Tracker s -> Text -> [Ninja] -> [Ninja] -> ST s ()
trackAction (Tracker xs) skill ns ns' = traverse_ (trackHooks1 skill zipped) xs
  where
    zipped = toList $ zip ns ns'

empty :: ∀ s. Tracker s
empty = Tracker mempty
