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

import           Data.MultiMap (MultiMap, (!))
import qualified Data.MultiMap as MultiMap
import qualified Data.Vector as Vector
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector

import qualified Class.Parity as Parity
import           Game.Model.Chakras (Chakras)
import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character as Character
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja
import           Game.Model.Player (Player)
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Game.Model.Trigger (Trigger)
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Handler.Play.GameInfo
import           Mission.Goal (Goal(Reach), Mission(Mission))
import qualified Mission.Goal as Goal
import           Mission.Hooks.Action (ActionHook)
import           Mission.Hooks.Chakra (ChakraHook)
import           Mission.Hooks.Store (StoreHook)
import           Mission.Hooks.Trap (TrapHook, TriggerHook)
import           Mission.Hooks.Turn (TurnHook)
import qualified Mission.Missions as Missions
import           Mission.Objective (Objective(..), Span(..))
import           Mission.Progress (Progress(Progress))
import           Mission.Store (Store)
import           Util ((!!))

missionKeys :: Text -> Mission -> [Int -> Progress]

missionKeys name Mission{char, goals} =
    [ Progress char i | (i, goal) <- zip [0..] $ toList goals,
                        Goal.belongsTo name goal ]

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

reset :: ∀ m. PrimMonad m => Track (PrimState m) -> m ()
reset Track{goals, progress} = mapM_ f . zip [0..] $ toList goals
  where
    f (i, goal@(Reach Turn _ _ _)) = MVector.unsafeModify progress
                                         (resetGoal goal) i
    f _ = return ()

addProgress :: ∀ m. PrimMonad m => Track (PrimState m) -> Int -> Int -> m ()
addProgress _ _ 0   = return ()
addProgress Track{goals, progress} i amt = case goals !! i of
    Reach Moment amount _ _ | amt < amount -> return ()
    _ -> MVector.unsafeModify progress (max 0 . (+ amt)) i

trackStore :: ∀ m. PrimMonad m
           => Track (PrimState m) -> Int -> (Store -> (Store, Int)) -> m ()
trackStore x@Track{store} i f = do
    (store', progress') <- f <$> MVector.unsafeRead store i
    MVector.unsafeWrite store i store'
    addProgress x i progress'

trackAction1 :: ∀ m. PrimMonad m
             => Text -> [(Ninja, Ninja)] -> Track (PrimState m) -> m ()
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
    tracker  (n, n') (i, f) = addProgress track i $ f skill user n n'
    tracker' (n, n') (i, f) = trackStore  track i $ f skill user n n'

trackChakra1 :: ∀ m. PrimMonad m
             => Text -> (Chakras, Chakras) -> (Chakras, Chakras)
             -> Track (PrimState m) -> m ()
trackChakra1 skill chaks chaks' x = sequence_ $ tracker <$> chakras x ! skill
  where
    tracker (i, f) = addProgress x i $ f (swapOwned chaks) (swapOwned chaks')
    swapOwned = Parity.swap $ slot x

trackTrap1 :: ∀ m. PrimMonad m
           => Text -> Slot -> Ninja -> Track (PrimState m) -> m ()
trackTrap1 trap user n x = sequence_ $ tracker <$> traps x ! trap
  where
    tracker (i, f) = trackStore x i $ f user n

trackTrigger1 :: ∀ m. PrimMonad m
              => Trigger -> Ninja -> Track (PrimState m) -> m ()
trackTrigger1 trigger n x = sequence_ $ tracker <$> triggers x ! trigger
  where
    tracker (i, f)
      | f n       = addProgress x i 1
      | otherwise = return ()

trackTurn1 :: ∀ m. PrimMonad m
           => Player -> [(Ninja, Ninja)] -> Track (PrimState m) -> m ()
trackTurn1 p ns x@Track{skills, slot, turns} = do
      sequence_ $ tracker <$> ns <*> turns
      unless (Parity.allied p user) $ modifyRef' skills $ fromMaybe [] . initMay
      reset x
  where
    user = snd $ ns !! Slot.toInt slot
    tracker (n, n') (i, f) = trackStore x i $ f p user n n'

new :: ∀ m. PrimMonad m => Ninja -> m (Track (PrimState m))
new Ninja{character = character@Character{ident}, slot} = do
    skills   <- newRef mempty
    store    <- MVector.replicate (length objectives) mempty
    progress <- MVector.replicate (length objectives) 0
    return $ foldl' go Track
        { slot
        , key      = missionKeys ident =<< missions
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
  where
    missions   = Missions.characterMissions character
    goals      = [ x | mission <- missions,
                       x       <- toList $ Goal.goals mission,
                       Goal.belongsTo ident x ]
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

trackAll :: ∀ m. PrimMonad m
         => (Track (PrimState m) -> m ()) -> Tracker (PrimState m) -> m ()
trackAll f (Tracker xs) = mapM_ f xs

-- | The mutable elements of the Tracker may not be used after this operation.
unsafeFreeze :: ∀ m. PrimMonad m => Tracker (PrimState m) -> m [Progress]
unsafeFreeze (Tracker xs) = concat <$> mapM freeze xs
  where
    freeze Track{key, progress} = (zipWith ($) key) . toList
                                  <$> Vector.unsafeFreeze progress

-- | Initializes a @Tracker@.
fromInfo :: ∀ m. PrimMonad m => GameInfo -> m (Tracker (PrimState m))
fromInfo GameInfo{ninjas, player} = Tracker
    <$> mapM new (fromList $ Parity.half player ninjas)

-- | 'HookAction'.
trackAction :: ∀ m. PrimMonad m
            => Text -> [Ninja] -> [Ninja] -> Tracker (PrimState m) -> m ()
trackAction skill ns ns' = trackAll . trackAction1 skill $ zip ns ns'

-- | 'HookChakra'.
trackChakra :: ∀ m. PrimMonad m
            => Text -> (Chakras, Chakras) -> (Chakras, Chakras)
            -> Tracker (PrimState m) -> m ()
trackChakra skill chaks chaks' = trackAll $ trackChakra1 skill chaks chaks'

-- | 'HookTrap'.
trackTrap :: ∀ m. PrimMonad m
          => Text -> Slot -> Ninja -> Tracker (PrimState m) -> m ()
trackTrap trap user n = trackAll $ trackTrap1 trap user n

-- | 'HookTrigger'.
trackTrigger :: ∀ m. PrimMonad m
             => Trigger -> Ninja -> Tracker (PrimState m) -> m ()
trackTrigger trigger n = trackAll $ trackTrigger1 trigger n

-- | 'HookTurn'.
trackTurn :: ∀ m. PrimMonad m
          => Player -> [Ninja] -> [Ninja] -> Tracker (PrimState m) -> m ()
trackTurn p ns ns' = trackAll . trackTurn1 p $ zip ns ns'

empty :: ∀ s. Tracker s
empty = Tracker mempty
