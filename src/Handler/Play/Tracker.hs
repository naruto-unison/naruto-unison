module Handler.Play.Tracker
  ( Tracker
  , fromInfo
  , empty
  , Progress(..)
  , unsafeFreeze
  , trackAction
  , trackChakra
  , trackTrap
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
import           Game.Model.Chakra (Chakras)
import qualified Game.Model.Character as Character
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Player (Player)
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Handler.Play.GameInfo (GameInfo)
import qualified Handler.Play.GameInfo as GameInfo
import           Mission.Goal (ActionHook, ChakraHook, StoreHook, TrapHook, TurnHook, Goal, Mission, Objective(..), Span(..), Store)
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
                     , actions  :: MultiMap Text (Int, ActionHook)
                     , chakras  :: MultiMap Text (Int, ChakraHook)
                     , stores   :: MultiMap Text (Int, StoreHook)
                     , traps    :: MultiMap Text (Int, TrapHook)
                     , turns    :: [(Int, TurnHook)]
                     , consecs  :: [(Int, [Text])]
                     , goals    :: Vector Goal
                     , skills   :: STRef s [Text]
                     , store    :: MVector s Store
                     , progress :: MVector s Int
                     }

resetGoal :: Goal -> Int -> Int
resetGoal x amt
  | amt < Goal.reach x = 0
  | otherwise          = amt

reset :: ∀ s. Track s -> ST s ()
reset x = traverse_ f . zip [0..] . toList $ goals x
  where
    f (i, goal) = case Goal.spanning goal of
        Turn -> MVector.unsafeModify (progress x) (resetGoal goal) i
        _    -> return ()

addProgress :: ∀ s. Track s -> Int -> Int -> ST s ()
addProgress _ _ 0   = return ()
addProgress x i amt = case Goal.spanning goal of
    Moment | amt < Goal.reach goal -> return ()
    _ -> MVector.unsafeModify (progress x) (max 0 . (+ amt)) i
  where
    goal = goals x !! i

trackStore :: ∀ s. Track s -> Int -> (Store -> (Store, Int)) -> ST s ()
trackStore x i f = do
    (store', progress') <- f <$> MVector.unsafeRead (store x) i
    MVector.unsafeWrite (store x) i store'
    addProgress x i progress'

trackAction1 :: ∀ s. Text -> [(Ninja, Ninja)] -> Track s -> ST s ()
trackAction1 skill ns x = do
    sequence_ $ tracker <$> ns <*> actions x ! skill
    sequence_ $ tracker' <$> ns <*> stores x ! skill
    modifyRef' (skills x) (skill :)
    used <- readRef $ skills x
    traverse_ (consec used) $ consecs x
  where
    user = snd $ ns !! Slot.toInt (slot x)
    consec used (i, match)
      | match == sort (zipWith const used match) =
          MVector.unsafeModify (progress x) (+1) i
      | otherwise = return ()
    tracker (n, n') (i, f) = addProgress x i $ f user n n'
    tracker' (n, n') (i, f) = trackStore x i $ f user n n'

trackChakra1 :: ∀ s. Text -> (Chakras, Chakras) -> (Chakras, Chakras) -> Track s
             -> ST s ()
trackChakra1 skill chaks chaks' x = sequence_ $ tracker <$> chakras x ! skill
  where
    tracker (i, f) = addProgress x i $ f (swapOwned chaks) (swapOwned chaks')
    swapOwned
      | Parity.even $ slot x = id
      | otherwise            = swap

trackTrap1 :: ∀ s. Text -> Ninja -> Track s -> ST s ()
trackTrap1 trap n x = sequence_ $ tracker <$> traps x ! trap
  where
    tracker (i, f) = trackStore x i $ f n

trackTurn1 :: ∀ s. Player -> [(Ninja, Ninja)] -> Track s -> ST s ()
trackTurn1 p ns x
  | not . Parity.allied p $ slot x = return ()
  | otherwise = do
      sequence_ $ tracker <$> ns <*> turns x
      modifyRef' (skills x) safeInit
      reset x
  where
    user = snd $ ns !! Slot.toInt (slot x)
    safeInit [] = []
    safeInit xs = initEx xs
    tracker (n, n') (i, f) = addProgress x i $ f user n n'

new :: ∀ s. Ninja -> ST s (Track s)
new n = do
    skills   <- newRef mempty
    store    <- MVector.replicate (length objectives) mempty
    progress <- MVector.replicate (length objectives) 0
    return $ foldl' go
        Track { slot    = Ninja.slot n
              , key     = missionKeys name =<< missions
              , actions = MultiMap.empty
              , chakras = MultiMap.empty
              , stores  = MultiMap.empty
              , traps   = MultiMap.empty
              , turns   = mempty
              , consecs = mempty
              , goals   = fromList goals
              , skills
              , store
              , progress
              }
        objectives
  where
    char       = Ninja.character n
    name       = Character.ident $ Ninja.character n
    missions   = Missions.characterMissions char
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
    go x (i, HookTurn _ func) =
        x { turns = (i, func) : turns x }
    go x (_, Win{}) =
        x

newtype Tracker s = Tracker (Vector (Track s))

trackAll :: ∀ s. (Track s -> ST s ()) -> Tracker s -> ST s ()
trackAll f (Tracker xs) = traverse_ f xs

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

trackAction :: ∀ s. Text -> [Ninja] -> [Ninja] -> Tracker s -> ST s ()
trackAction skill ns ns' = trackAll . trackAction1 skill . toList $ zip ns ns'

trackChakra :: ∀ s. Text -> (Chakras, Chakras) -> (Chakras, Chakras)
            -> Tracker s -> ST s ()
trackChakra skill chaks chaks' = trackAll $ trackChakra1 skill chaks chaks'

trackTrap :: ∀ s. Text -> Ninja -> Tracker s -> ST s ()
trackTrap trap n = trackAll $ trackTrap1 trap n

trackTurn :: ∀ s. Player -> [Ninja] -> [Ninja] -> Tracker s -> ST s ()
trackTurn p ns ns' = trackAll . trackTurn1 p $ zip ns ns'

empty :: ∀ s. Tracker s
empty = Tracker mempty
