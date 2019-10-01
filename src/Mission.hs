module Mission
  ( initDB
  , progress
  , unlocked
  , userMission
  , processWin
  ) where

import ClassyPrelude hiding ((\\))
import Yesod

import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.Bimap as Bimap
import           Data.Bimap (Bimap)
import           Data.List ((\\))
import qualified Data.Sequence as Seq
import           Database.Persist.Sql (Entity(..), SqlPersistT)
import qualified Yesod.Auth as Auth

import           Util ((∉))
import qualified Application.App as App
import           Application.App (Handler)
import           Application.Model (Character(..), CharacterId, EntityField(..), Mission(..), Unlocked(..), User)
import qualified Application.Settings as Settings
import qualified Game.Model.Character as Character
import qualified Game.Characters as Characters
import qualified Mission.Goal as Goal
import           Mission.Goal (Goal)
import qualified Mission.Missions as Missions
import           Mission.Progress (Progress(..))

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
    unlockAll <- getsYesod $ Settings.unlockAll . App.settings
    mwho <- Auth.maybeAuthId
    case mwho of
        Just who | not unlockAll -> do
            ids     <- getsYesod App.characterIDs
            unlocks <- runDB $ selectList [UnlockedUser ==. who] []
            return $ unlock ids unlocks
        _ -> return $ keysSet Characters.map

unlock :: Bimap CharacterId Text -> [Entity Unlocked] -> HashSet Text
unlock ids unlocks = union (setFromList $ mapMaybe look unlocks) $
                     keysSet Characters.map `difference` keysSet Missions.map
  where
    look (Entity _ Unlocked{unlockedCharacter}) =
        Bimap.lookup unlockedCharacter ids

userMission :: Text -> Handler (Maybe (Seq (Goal, Int)))
userMission name = fromMaybe mempty <$> runMaybeT do
    who     <- MaybeT Auth.maybeAuthId
    ids     <- getsYesod App.characterIDs
    char    <- Bimap.lookupR name ids
    mission <- MaybeT . return $ lookup name Missions.map
    (Just . zip mission <$>) . lift $ runDB do
        alreadyUnlocked <-
            selectFirst [UnlockedUser ==. who, UnlockedCharacter ==. char] []
        if isJust alreadyUnlocked then
            return $ Goal.reach <$> mission
        else
            setObjectives mission <$>
                selectList [MissionUser ==. who, MissionCharacter ==. char] []

updateProgress :: ∀ m. MonadIO m
               => Seq Goal
               -> Key User -> Key Character -> Int -> Int -> SqlPersistT m Bool
updateProgress mission who char i amount = do
    alreadyUnlocked <- isJust <$> selectFirst unlockedChar []
    if alreadyUnlocked then
        return True
    else do
        void $ upsert (Mission who char i amount) [MissionProgress +=. amount]
        objectives <- selectList missionChar []
        if completed mission objectives then do
            deleteWhere missionChar
            insertUnique $ Unlocked who char
            return True
        else
            return False
  where
    unlockedChar = [UnlockedUser ==. who, UnlockedCharacter ==. char]
    missionChar  = [MissionUser ==. who, MissionCharacter ==. char]

progress :: Progress -> Handler Bool
progress Progress{amount = 0} = return False
progress Progress{name, objective, amount} = fromMaybe False <$> runMaybeT do
    who        <- MaybeT Auth.maybeAuthId
    mission    <- MaybeT . return $ lookup name Missions.map
    let len     = length mission
    guard $ objective < len
    ids        <- getsYesod App.characterIDs
    char       <- Bimap.lookupR name ids
    lift . runDB $ updateProgress mission who char objective amount

setObjectives :: Seq Goal -> [Entity Mission] -> Seq Int
setObjectives xs objectives = foldl' f (0 <$ xs) objectives
  where
    f acc (Entity _ x) = Seq.update (missionObjective x) (missionProgress x) acc

completed :: Seq Goal -> [Entity Mission] -> Bool
completed mission objectives = and . zipWith ((<=) . Goal.reach) mission $
                               setObjectives mission objectives

winners :: Bimap CharacterId Text
        -> [Text] -> [Entity Unlocked] -> [(Seq Goal, Key Character, Int)]
winners ids team unlocks = do
    Goal.Mission name mission <- Missions.list
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
