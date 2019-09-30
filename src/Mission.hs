module Mission
  ( initDB
  , progress
  , unlocked
  , teamMissions
  , userMission
  , processWin
  ) where

import ClassyPrelude hiding ((\\), map)
import Yesod

import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.Bimap as Bimap
import           Data.Bimap (Bimap)
import           Data.List ((\\), nub)
import qualified Data.Sequence as Seq
import           Database.Persist.Sql (Entity(..), SqlPersistT)
import qualified Yesod.Auth as Auth

import           Util ((∉), mapFromKeyed)
import qualified Application.App as App
import           Application.App (Handler)
import           Application.Model (Character(..), CharacterId, EntityField(..), Mission(..), Unlocked(..), User)
import qualified Game.Model.Character as Character
import qualified Game.Characters as Characters
import qualified Mission.Goal as Goal
import           Mission.Goal (Goal)

import qualified Mission.Shippuden

list :: [Goal.Mission]
list = Mission.Shippuden.missions
{-# NOINLINE list #-}

map :: HashMap Text Goal.Mission
map = mapFromKeyed (Goal.char, id) list
{-# NOINLINE map #-}

characterMissions :: Text -> [Goal.Mission]
characterMissions name =
    filter (any (Goal.involves name . Goal.objective) . Goal.goals) list

teamMissions :: [Text] -> [Goal.Mission]
teamMissions names = nub $ characterMissions =<< names

initDB :: ∀ m. MonadIO m => SqlPersistT m (Bimap CharacterId Text)
initDB = do
    chars    <- (entityVal <$>) <$> selectList [] []
    insertMany_ .
        filter (∉ chars) $ Character . Character.format <$> Characters.list
    newChars <- selectList [] []
    return $ makeMap newChars

makeMap :: [Entity Character] -> Bimap CharacterId Text
makeMap chars = Bimap.fromList . mapMaybe maybePair $ chars
  where
    maybePair (Entity charId Character{characterName}) =
        (charId, ) . Character.format <$> Characters.lookupName characterName

unlocked :: Handler (HashSet Text)
unlocked = do
    mwho <- Auth.maybeAuthId
    case mwho of
        Nothing  -> return mempty
        Just who -> do
            ids     <- getsYesod App.characterIDs
            unlocks <- runDB $ selectList [UnlockedUser ==. who] []
            return $ unlock ids unlocks

unlock :: Bimap CharacterId Text -> [Entity Unlocked] -> HashSet Text
unlock ids unlocks = union (setFromList $ mapMaybe look unlocks) $
                     keysSet Characters.map `difference` keysSet map
  where
    look (Entity _ Unlocked{unlockedCharacter}) =
        Bimap.lookup unlockedCharacter ids

userMission :: Text -> Handler (Seq (Goal, Int))
userMission name = fromMaybe mempty <$> runMaybeT do
    mission    <- MaybeT . return $ Goal.goals <$> lookup name map
    ids        <- getsYesod App.characterIDs
    char       <- Bimap.lookupR name ids
    mwho       <- Auth.maybeAuthId
    case mwho of
        Nothing  -> return $ unGoal <$> mission
        Just who -> do
            objectives <- lift . runDB $ selectList
                          [MissionUser ==. who, MissionCharacter ==. char] []
            return . zip mission $ setObjectives objectives mission
  where
    unGoal x = (x { Goal.reach = 0 }, 0)

updateProgress :: ∀ m. MonadIO m
               => Seq Goal
               -> Key User -> Key Character -> Int -> Int -> SqlPersistT m Bool
updateProgress mission who char i amount = do
    void $ upsert (Mission who char i amount) [MissionProgress +=. amount]
    objectives <- selectList [MissionUser ==. who, MissionCharacter ==. char] []
    if completed mission objectives then do
        deleteWhere [MissionUser ==. who, MissionCharacter ==. char]
        insertUnique $ Unlocked who char
        return True
    else
        return False

progress :: Text -> Int -> Int -> Handler Bool
progress name i amount = fromMaybe False <$> runMaybeT do
    Just who   <- Auth.maybeAuthId
    mission    <- MaybeT . return $ Goal.goals <$> lookup name map
    let len     = length mission
    guard $ i < len
    ids        <- getsYesod App.characterIDs
    char       <- Bimap.lookupR name ids
    lift . runDB $ updateProgress mission who char i amount

setObjectives :: [Entity Mission] -> Seq Goal -> Seq Int
setObjectives objectives xs = foldl' f (0 <$ xs) objectives
  where
    f acc (Entity _ x) = Seq.update (missionObjective x) (missionProgress x) acc

completed :: Seq Goal -> [Entity Mission] -> Bool
completed mission objectives = and . zipWith ((<=) . Goal.reach) mission $
                               setObjectives objectives mission

winners :: Bimap CharacterId Text
        -> [Text] -> [Entity Unlocked] -> [(Seq Goal, Key Character, Int)]
winners ids team unlocks = do
    Goal.Mission name mission <- list
    guard $ name ∉ names
    (i, Goal.Win team') <- zip [0..] $ Goal.objective <$> toList mission
    guard . null $ team' \\ team
    char <- Bimap.lookupR name ids
    return (mission, char, i)
  where
    names = unlock ids unlocks

processWin :: [Text] -> Handler ()
processWin team = do
    who <- Auth.requireAuthId
    ids <- getsYesod App.characterIDs
    runDB do
        unlocks <- selectList [UnlockedUser ==. who] []
        forM_ (winners ids team unlocks) \(mission, char, i) ->
            void $ updateProgress mission who char i 1
