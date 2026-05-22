-- | Tracks progress on character missions during a game.
module Handler.Play.Tracker
  ( Tracker
  , fromInfo
  , empty
  , Progress(..)
  , unsafeFreeze
  , trackAction
  , trackChakra
  , trackTrap
  , trackTrigger
  , trackTurn
  ) where

import ClassyPrelude hiding (empty)

import           Control.Monad.ST (ST)
import           Data.MultiMap (MultiMap, (!))
import qualified Data.MultiMap as MultiMap
import qualified Data.Vector as Vector
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector

import qualified Class.Parity as Parity
import           Game.Model.Chakras (Chakras)
import qualified Game.Model.Character as Character
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja
import           Game.Model.Player (Player)
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Game.Model.Trigger (Trigger)
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Handler.Play.GameInfo
import           Mission.Goal (Goal(Reach), Mission(Mission), Objective(..), Span(..), Store, ActionHook, ChakraHook, StoreHook, TrapHook, TriggerHook, TurnHook)
import qualified Mission.Goal as Goal
import qualified Mission.Missions as Missions
import           Mission.Progress (Progress(Progress))
import           Util ((!!))

missionKeys :: Text -> Mission -> [Int -> Progress]

missionKeys name Mission{char, goals} =
    [Progress char i | (i, goal) <- zip [0..] $ toList goals
                     , Goal.belongsTo name goal]

data Track s = Track
    { slot     :: Slot
    , key      :: [(Int -> Progress)]
    , actions  :: MultiMap Text (Int, ActionHook)
    , chakras  :: MultiMap Text (Int, ChakraHook)
    , stores   :: MultiMap Text (Int, StoreHook)
    , traps    :: MultiMap Text (Int, TrapHook)
    , triggers :: MultiMap Trigger (Int, TriggerHook)
    , turns    :: [(Int, TurnHook)]
    , consecs  :: [(Int, [Text])]
    , goals    :: Vector Goal
    , skills   :: STRef s [Text]
    , store    :: MVector s Store
    , progress :: MVector s Int
    }

resetGoal :: Goal -> Int -> Int
resetGoal Reach{reach} amt
  | amt < reach = 0
  | otherwise   = amt

reset :: ∀ s. Track s -> ST s ()
reset Track{goals, progress} = mapM_ f . zip [0..] $ toList goals
  where
    f (i, goal@(Reach Turn _ _ _)) = MVector.unsafeModify progress
                                         (resetGoal goal) i
    f _ = return ()

addProgress :: ∀ s. Track s -> Int -> Int -> ST s ()
addProgress _ _ 0   = return ()
addProgress Track{goals, progress} i amt = case goals !! i of
    Reach Moment amount _ _ | amt < amount -> return ()
    _ -> MVector.unsafeModify progress (max 0 . (+ amt)) i

trackStore :: ∀ s. Track s -> Int -> (Store -> (Store, Int)) -> ST s ()
trackStore x@Track{store} i f = do
    (store', progress') <- f <$> MVector.unsafeRead store i
    MVector.unsafeWrite store i store'
    addProgress x i progress'

trackAction1 :: ∀ s. Text -> [(Ninja, Ninja)] -> Track s -> ST s ()
trackAction1 skill ns track@Track { actions
                                  , consecs
                                  , progress
                                  , skills
                                  , slot
                                  , stores
                                  } = do
    sequence_ $ tracker <$> ns <*> actions ! skill
    sequence_ $ tracker' <$> ns <*> stores ! skill
    modifyRef' (skills) (skill :)
    used <- readRef $ skills
    mapM_ (consec used) consecs
  where
    user = snd $ ns !! Slot.toInt slot
    consec used (i, match)
      | match /= sort (zipWith const used match) = return ()
      | otherwise = MVector.unsafeModify progress (+ 1) i
    tracker (n, n') (i, f)  = addProgress track i $ f skill user n n'
    tracker' (n, n') (i, f) = trackStore track i $ f skill user n n'

trackChakra1 :: ∀ s. Text -> (Chakras, Chakras) -> (Chakras, Chakras) -> Track s
             -> ST s ()
trackChakra1 skill chaks chaks' x = sequence_ $ tracker <$> chakras x ! skill
  where
    tracker (i, f) = addProgress x i $ f (swapOwned chaks) (swapOwned chaks')
    swapOwned
      | Parity.even $ slot x = id
      | otherwise            = swap

trackTrap1 :: ∀ s. Text -> Slot -> Ninja -> Track s -> ST s ()
trackTrap1 trap user n x = sequence_ $ tracker <$> traps x ! trap
  where
    tracker (i, f) = trackStore x i $ f user n

trackTrigger1 :: ∀ s. Trigger -> Ninja -> Track s -> ST s ()
trackTrigger1 trigger n x = sequence_ $ tracker <$> triggers x ! trigger
  where
    tracker (i, f)
      | f n       = addProgress x i 1
      | otherwise = return ()

trackTurn1 :: ∀ s. Player -> [(Ninja, Ninja)] -> Track s -> ST s ()
trackTurn1 p ns x@Track{skills, slot, turns} = do
      sequence_ $ tracker <$> ns <*> turns
      unless (Parity.allied p user) $ modifyRef' skills safeInit
      reset x
  where
    user = snd $ ns !! Slot.toInt slot
    safeInit [] = []
    safeInit xs = unsafeInit xs
    tracker (n, n') (i, f) = trackStore x i $ f p user n n'

new :: ∀ s. Ninja -> ST s (Track s)
new Ninja{character, slot} = makeTrack
    <$> newRef mempty
    <*> MVector.replicate (length objectives) mempty
    <*> MVector.replicate (length objectives) 0
  where
    makeTrack skills store progress = foldl' go Track
        { slot
        , key      = missionKeys name =<< missions
        , actions  = MultiMap.empty
        , chakras  = MultiMap.empty
        , stores   = MultiMap.empty
        , traps    = MultiMap.empty
        , triggers = MultiMap.empty
        , turns    = mempty
        , consecs  = mempty
        , goals    = fromList goals
        , skills
        , store
        , progress
        } objectives
    name       = Character.ident character
    missions   = Missions.characterMissions character
    goals      = [x | mission <- missions
                    , x       <- toList $ Goal.goals mission
                    , Goal.belongsTo name x]
    objectives = zip [0..] $ Goal.objective <$> goals

    go x (i, Consecutive _ skills) =
        x { consecs = (i, skills) : consecs x }
    go x (i, HookAction _ skill func) =
        x { actions = MultiMap.insert skill (i, func) $ actions x }
    go x (i, HookChakra _ skill func) =
        x { chakras = MultiMap.insert skill (i, func) $ chakras x }
    go x (i, HookStore _ skill func) =
        x { stores = MultiMap.insert skill (i, func) $ stores x }
    go x (i, HookTrap _ trap func) =
        x { traps = MultiMap.insert trap (i, func) $ traps x }
    go x (i, HookTrigger _ trigger func) =
        x { triggers = MultiMap.insert trigger (i, func) $ triggers x }
    go x (i, HookTurn _ func) =
        x { turns = (i, func) : turns x }
    go x (_, Win{}) =
        x

newtype Tracker s = Tracker (Vector (Track s))

trackAll :: ∀ s. (Track s -> ST s ()) -> Tracker s -> ST s ()
trackAll f (Tracker xs) = mapM_ f xs

-- | The mutable elements of the Tracker may not be used after this operation.
unsafeFreeze :: ∀ s. Tracker s -> ST s [Progress]
unsafeFreeze (Tracker xs) = concat <$> mapM freeze xs
  where
    freeze Track{key, progress} = (zipWith ($) key) . toList
                                  <$> Vector.unsafeFreeze progress

-- | Initializes a @Tracker@.
fromInfo :: ∀ s. GameInfo -> ST s (Tracker s)
fromInfo GameInfo{ninjas, player} = Tracker
    <$> mapM new (fromList $ Parity.half player ninjas)

-- | 'HookAction'.
trackAction :: ∀ s. Text -> [Ninja] -> [Ninja] -> Tracker s -> ST s ()
trackAction skill ns ns' = trackAll . trackAction1 skill . toList $ zip ns ns'

-- | 'HookChakra'.
trackChakra :: ∀ s. Text -> (Chakras, Chakras) -> (Chakras, Chakras)
            -> Tracker s -> ST s ()
trackChakra skill chaks chaks' = trackAll $ trackChakra1 skill chaks chaks'

-- | 'HookTrap'.
trackTrap :: ∀ s. Text -> Slot -> Ninja -> Tracker s -> ST s ()
trackTrap trap user n = trackAll $ trackTrap1 trap user n

-- | 'HookTrigger'.
trackTrigger :: ∀ s. Trigger -> Ninja -> Tracker s -> ST s ()
trackTrigger trigger n = trackAll $ trackTrigger1 trigger n

-- | 'HookTurn'.
trackTurn :: ∀ s. Player -> [Ninja] -> [Ninja] -> Tracker s -> ST s ()
trackTurn p ns ns' = trackAll . trackTurn1 p $ zip ns ns'

empty :: ∀ s. Tracker s
empty = Tracker mempty
