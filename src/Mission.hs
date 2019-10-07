module Mission
  ( initDB
  , progress
  , unlocked
  , characterID
  , userMission
  , processWin
  , awardDNA
  ) where

import ClassyPrelude hiding ((\\))
import Yesod

import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.List ((\\))
import qualified Data.Sequence as Seq
import           Database.Persist.Sql (Entity(..), SqlPersistT)
import qualified Database.Persist.Sql as Sql
import qualified Yesod.Auth as Auth

import           Application.App (Handler)
import qualified Application.App as App
import           Application.Model (Character(..), CharacterId, EntityField(..), Mission(..), Unlocked(..), User(..))
import qualified Application.Settings as Settings
import qualified Game.Characters as Characters
import qualified Game.Model.Character as Character
import           Handler.Play.Match (Outcome(..))
import qualified Handler.Play.Queue as Queue
import           Mission.Goal (Goal, Span(..))
import qualified Mission.Goal as Goal
import qualified Mission.Missions as Missions
import           Mission.Progress (Progress(..))
import           Util ((∉))

initDB :: ∀ m. MonadIO m => SqlPersistT m (Bimap CharacterId Text)
initDB = do
    chars    <- (entityVal <$>) <$> selectList [] []
    insertMany_ .
        filter (∉ chars) $ Character . Character.format <$> Characters.list
    newChars <- selectList [] []
    return $ makeMap newChars

characterID :: Text -> MaybeT Handler CharacterId
characterID name = Bimap.lookupR name =<< getsYesod App.characterIDs

makeMap :: [Entity Character] -> Bimap CharacterId Text
makeMap chars = Bimap.fromList . mapMaybe maybePair $ chars
  where
    maybePair (Entity charId Character{characterName}) =
        (charId, ) . Character.format <$> Characters.lookup characterName

unlocked :: Handler (HashSet Text)
unlocked = do
    unlockAll <- getsYesod $ Settings.unlockAll . App.settings
    mwho <- Auth.maybeAuthId
    case mwho of
        Just who | not unlockAll -> do
            ids     <- getsYesod App.characterIDs
            unlocks <- runDB $ selectList [UnlockedUser ==. who] []
            return $ getUnlocked ids unlocks
        _ -> return $ keysSet Characters.map

freeChars :: HashSet Text
freeChars = setFromList dna `difference` keysSet Missions.map
  where
    dna = Character.format <$> filter ((== 0) . Character.price) Characters.list
{-# NOINLINE freeChars #-}

getUnlocked :: Bimap CharacterId Text -> [Entity Unlocked] -> HashSet Text
getUnlocked ids unlocks = freeChars `union` setFromList (mapMaybe look unlocks)
  where
    look (Entity _ Unlocked{unlockedCharacter}) =
        Bimap.lookup unlockedCharacter ids

userMission :: Character.Character -> Handler (Maybe (Seq (Goal, Int)))
userMission char = fromMaybe mempty <$> runMaybeT do
    who     <- MaybeT Auth.maybeAuthId
    charID  <- characterID name
    mission <- MaybeT . return $ lookup name Missions.map
    (Just . zip mission <$>) . lift $ runDB do
        alreadyUnlocked <-
            selectFirst [UnlockedUser ==. who, UnlockedCharacter ==. charID] []
        if isJust alreadyUnlocked then
            return $ Goal.reach <$> mission
        else
            setObjectives mission <$>
                selectList [MissionUser ==. who, MissionCharacter ==. charID] []
  where
    name = Character.format char

-- Invariant: @i < length mission@.
updateProgress :: ∀ m. MonadIO m
               => Seq Goal
               -> Key User -> Key Character -> Int -> Int -> SqlPersistT m Bool
updateProgress mission who char i amount = case mission `index` i of
    Nothing   -> return False
    Just goal
      | Goal.spanning goal /= Career && Goal.reach goal < amount -> return False
      | otherwise -> do
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
progress Progress{character, objective, amount} =
    fromMaybe False <$> runMaybeT do
        who     <- MaybeT Auth.maybeAuthId
        mission <- MaybeT . return $ lookup character Missions.map
        let len  = length mission
        guard $ objective < len
        charID  <- characterID character
        lift . runDB $ updateProgress mission who charID objective amount

setObjectives :: Seq Goal -> [Entity Mission] -> Seq Int
setObjectives xs objectives = foldl' f (0 <$ xs) objectives
  where
    f acc (Entity _ x) = Seq.update (missionObjective x) (missionProgress x) acc

completed :: Seq Goal -> [Entity Mission] -> Bool
completed mission objectives = and . zipWith ((<=) . Goal.reach) mission $
                               setObjectives mission objectives

winners :: Bimap CharacterId Text
        -> [Character.Character] -> [Entity Unlocked]
        -> [(Seq Goal, Key Character, Int)]
winners ids chars unlocks = do
    Goal.Mission{char, goals} <- Missions.list
    guard $ char ∉ names
    (i, Goal.Win team') <- zip [0..] $ Goal.objective <$> toList goals
    guard . null $ team' \\ team
    charID <- Bimap.lookupR char ids
    return (goals, charID, i)
  where
    team  = Character.format <$> chars
    names = getUnlocked ids unlocks

processWin :: [Character.Character] -> Handler ()
processWin team = do
    who <- Auth.requireAuthId
    ids <- getsYesod App.characterIDs
    runDB do
        unlocks <- selectList [UnlockedUser ==. who] []
        forM_ (winners ids team unlocks) \(mission, char, i) ->
            void $ updateProgress mission who char i 1

-- When ladder matches are introduced, these two will become more complicated.

awardDNA :: Queue.Section -> Outcome -> Handler [(Text, Int)]
awardDNA Queue.Private _ = return []
awardDNA Queue.Quick outcome = do
    (who, user)   <- Auth.requireAuthPair
    dnaConf       <- getsYesod $ Settings.dnaConf . App.settings
    UTCTime day _ <- liftIO getCurrentTime
    let jDay       = Just day
    let tallies    = tallyDNA Queue.Quick outcome dnaConf jDay user
    runDB . Sql.update who $ updateLatestWin outcome jDay
        [UserLatestGame =. jDay, UserDna +=. sum (snd <$> tallies)]
    return tallies

updateLatestWin :: Outcome -> Maybe Day -> [Update User] -> [Update User]
updateLatestWin Victory day xs = (UserLatestWin =. day) : xs
updateLatestWin _       _   xs = xs

tallyDNA :: Queue.Section -> Outcome -> Settings.DNA -> Maybe Day -> User
         -> [(Text, Int)]
tallyDNA section outcome dnaConf day user = filter ((> 0) . snd)
    [ (tshow outcome, outcomeDNA section outcome dnaConf)
    , ("First Game of the Day", dailyGame)
    , ("First Win of the Day", dailyWin)
    , ("Win Streak",  winStreak)
    ]
  where
    winStreak
      | outcome /= Victory         = 0
      | userStreak user < 1        = 0
      | Settings.useStreak dnaConf = floor . sqrt @Float . fromIntegral $
                                     userStreak user - 1
      | otherwise                  = 0
    dailyGame
      | userLatestGame user == day = 0
      | otherwise                  = Settings.dailyGame dnaConf
    dailyWin
      | outcome /= Victory        = 0
      | userLatestWin user == day = 0
      | otherwise                 = Settings.dailyWin dnaConf


outcomeDNA ::Queue.Section -> Outcome -> Settings.DNA -> Int
outcomeDNA Queue.Private _     = const 0
outcomeDNA Queue.Quick Victory = Settings.quickWin
outcomeDNA Queue.Quick Defeat  = Settings.quickLose
outcomeDNA Queue.Quick Tie     = Settings.quickTie
