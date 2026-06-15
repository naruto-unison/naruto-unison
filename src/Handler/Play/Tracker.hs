-- | Tracks progress on character missions during a game.
module Handler.Play.Tracker
  ( Tracker
  , fromInfo
  , empty
  , Progress(..)
  , freeze, unsafeFreeze
  , trackAction
  , trackChakra
  , trackTrap
  , trackTrigger
  , trackTurn
  ) where

import ClassyPrelude hiding (empty)

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
import           Mission.Goal (Goal(Reach))
import qualified Mission.Goal as Goal
import           Mission.Hooks (Hooks(Hooks))
import qualified Mission.Hooks as Hooks
import           Mission.Objective (Span(..))
import           Mission.Progress (Progress(Progress), Store)
import           Util ((!!), (!))

data Track s = Track
    { slot     :: Slot
    , hooks    :: Hooks
    , skills   :: STRef s [Text]
    , store    :: MVector s Store
    , progress :: MVector s Int
    }

resetGoal :: Goal -> Int -> Int
resetGoal Reach{reach} amt
  | amt < reach = 0
  | otherwise   = amt

reset :: ∀ m. PrimMonad m => Track (PrimState m) -> m ()
reset Track{hooks = Hooks{goals}, progress} = mapM_ f . zip [0..] $ toList goals
  where
    f (i, goal@(Reach Turn _ _ _)) = MVector.unsafeModify progress
                                         (resetGoal goal) i
    f _ = return ()

addProgress :: ∀ m. PrimMonad m => Track (PrimState m) -> Int -> Int -> m ()
addProgress _ _ 0   = return ()
addProgress Track{hooks = Hooks{goals}, progress} i amt = case goals !! i of
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
trackAction1 skill ns track@Track { hooks = Hooks { actions
                                                  , consecs
                                                  , stores
                                                  }
                                  , progress
                                  , skills
                                  , slot
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
trackChakra1 skill chaks chaks' x@Track{hooks = Hooks{chakras}} =
    sequence_ $ tracker <$> chakras ! skill
  where
    tracker (i, f) = addProgress x i $ f (swapOwned chaks) (swapOwned chaks')
    swapOwned = Parity.swap $ slot x

trackTrap1 :: ∀ m. PrimMonad m
           => Text -> Slot -> Ninja -> Track (PrimState m) -> m ()
trackTrap1 trap user n x@Track{hooks = Hooks{traps}} =
    sequence_ $ tracker <$> traps ! trap
  where
    tracker (i, f) = trackStore x i $ f user n

trackTrigger1 :: ∀ m. PrimMonad m
              => Trigger -> Ninja -> Track (PrimState m) -> m ()
trackTrigger1 trigger n x@Track{hooks = Hooks{triggers}} =
    sequence_ $ tracker <$> triggers ! trigger
  where
    tracker (i, f)
      | f n       = addProgress x i 1
      | otherwise = return ()

trackTurn1 :: ∀ m. PrimMonad m
           => Player -> [(Ninja, Ninja)] -> Track (PrimState m) -> m ()
trackTurn1 p ns x@Track{skills, slot, hooks = Hooks{turns}} = do
      sequence_ $ tracker <$> ns <*> turns
      unless (Parity.allied p user) $ modifyRef' skills $ fromMaybe [] . initMay
      reset x
  where
    user = snd $ ns !! Slot.toInt slot
    tracker (n, n') (i, f) = trackStore x i $ f p user n n'

new :: ∀ m. PrimMonad m => Ninja -> m (Track (PrimState m))
new Ninja{character = Character{ident}, slot} = do
    skills   <- newRef mempty
    store    <- MVector.replicate storeSize mempty
    progress <- MVector.replicate storeSize 0
    return Track
        { slot
        , hooks
        , skills
        , store
        , progress
        }
  where
    hooks = Hooks.forCharacter ident
    storeSize = length $ Hooks.goals hooks

newtype Tracker s = Tracker (Vector (Track s))

trackAll :: ∀ m. PrimMonad m
         => (Track (PrimState m) -> m ()) -> Tracker (PrimState m) -> m ()
trackAll f (Tracker xs) = mapM_ f xs

gFreeze :: ∀ m. PrimMonad m
        => (∀ a. MVector (PrimState m) a -> m (Vector a))
        -> Tracker (PrimState m) -> m [Progress]
gFreeze freezer (Tracker xs) = concat <$> mapM freezeTrack xs
  where
    freezeTrack Track{progress, hooks = Hooks{key}} =
        (zipWith ($) key) . toList <$> freezer progress

-- | The mutable elements of the Tracker may not be used after this operation.
freeze :: ∀ m. PrimMonad m => Tracker (PrimState m) -> m [Progress]
freeze = gFreeze Vector.freeze

-- | The mutable elements of the Tracker may not be used after this operation.
unsafeFreeze :: ∀ m. PrimMonad m => Tracker (PrimState m) -> m [Progress]
unsafeFreeze = gFreeze Vector.unsafeFreeze

-- | Initializes a @Tracker@.
fromInfo :: ∀ m. PrimMonad m => GameInfo -> m (Tracker (PrimState m))
fromInfo GameInfo{ninjas, player} = Tracker
    <$> mapM new (Parity.half player ninjas)

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
