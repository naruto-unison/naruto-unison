-- | Database handling for character missions, which users complete in order to
-- unlock new characters.
module Mission
  ( initDB
  , updateProgress
  , Unlocks, unlocked, freeChars
  , characterID
  , userMission
  , processWin, processDefeat, processUnpicked
  , awardDNA
  , getUsageRates
  ) where

import ClassyPrelude
import Database.Persist

import           Control.Monad.Trans.Maybe (MaybeT(..), hoistMaybe)
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Sequence as Seq
import           Database.Persist.Sql (SqlPersistT)
import           Yesod (cached, getsYesod, runDB)
import qualified Yesod.Auth as Auth

import           Application.App (liftDB)
import qualified Application.App as App
import           Application.Model (EntityField(..))
import           Application.Model.Character (Character(Character), CharacterId)
import           Application.Model.Mission (Mission(Mission))
import qualified Application.Model.Mission
import           Application.Model.Unlocked (Unlocked(Unlocked))
import qualified Application.Model.Unlocked
import           Application.Model.Usage (Usage(Usage))
import qualified Application.Model.Usage as Usage
import           Application.Model.User (Privilege(..), User(User))
import qualified Application.Model.User
import qualified Application.Model.Character
import qualified Application.Settings as Settings
import qualified Game.Characters as Characters
import qualified Game.Model.Character as Character
import           Handler.Client.Reward (Reward(Reward))
import qualified Handler.Client.Reward as Reward
import           Handler.Play.Match (Outcome(..))
import           Handler.Play.War (War)
import qualified Handler.Queue as Queue
import           Mission.Goal (Goal(Reach))
import qualified Mission.Goal as Goal
import qualified Mission.Missions as Missions
import           Mission.Objective (Objective(..), Span(..))
import           Mission.Progress (Progress(Progress))
import qualified Mission.Progress
import           Mission.UsageRate (UsageRate)
import qualified Mission.UsageRate as UsageRate
import           Util ((!?), (∈), (∉))

-- | Starts up the mission database by mapping every Character to a database
-- ID. Returns the map, which goes into 'App.characterIDs'.
-- 'Character.ident' is used as the key.
initDB :: ∀ m. MonadIO m => SqlPersistT m (Bimap CharacterId Text)
initDB = do
    chars <- (entityVal <$>) <$> selectList [] []
    insertMany_ $ filter (∉ chars) charList
    charEntities <- selectList [] []
    return $ makeMap charEntities
  where
    charList = Character . Character.ident <$> Characters.list

-- | Looks up a Character's ID in 'App.characterIDs' using 'Character.ident'.
characterID :: ∀ m. (App.MonadHandler m)
            => Text -> m (Maybe CharacterId)
characterID name = Bimap.lookupR name <$> getsYesod App.characterIDs

-- | Processes the database list of characters into a map between IDs and
-- 'Character.ident'.
makeMap :: [Entity Character] -> Bimap CharacterId Text
makeMap chars = Bimap.fromList $ mapMaybe maybePair chars
  where
    maybePair (Entity charId Character{name}) =
        (charId, ) . Character.ident <$> Characters.lookup name

type Unlocks = HashSet Text

allUnlocked :: Unlocks
allUnlocked = keysSet Characters.map

-- | 'Character.ident' collection of all Characters that the user has unlocked.
-- If not logged in, all Characters are returned.
-- If @unlock-all@ in [config/settings.yml](config/settings.yml) is set to true,
-- all Characters will always be returned.
unlocked :: App.Handler Unlocks
unlocked = cached $ fromMaybe allUnlocked <$> runMaybeT do
    unlockAll <- getsYesod $ Settings.unlockAll . App.settings
    guard unlockAll
    Just who  <- Auth.maybeAuthId
    privilege <- App.getPrivilege
    guard $ privilege < Moderator
    ids <- getsYesod App.characterIDs
    unlocks <- liftDB $ selectList [ UnlockedUser ==. who ] []
    return $ freeChars `union` setFromList (mapMaybe (look ids) unlocks)
  where
    look ids (Entity _ Unlocked{character}) =
        Bimap.lookup character ids

-- | 'Character.ident's of all Characters without missions or DNA
-- 'Character.price's.
freeChars :: HashSet Text
freeChars = setFromList dna \\ keysSet Missions.map
  where
    dna = [ident | Character.Character{price = 0, ident} <- Characters.list]
{-# NOINLINE freeChars #-}

-- | Returns the user's progress on a single Character's mission.
-- Returns @Nothing@ if the user is not logged in, the Character does not
-- have a mission, or the user has already completed their mission.
-- Otherwise, returns a list of goals paired with the user's progress on each.
userMission :: Text -> App.Handler (Maybe (Seq (Goal, Int)))
userMission char = runMaybeT do
    Just who    <- Auth.maybeAuthId
    Just charID <- characterID char
    mission     <- hoistMaybe $ lookup char Missions.map
    objectives  <- liftDB do
        alreadyUnlocked <- selectFirst [ UnlockedUser      ==. who
                                       , UnlockedCharacter ==. charID
                                       ] []
        if isJust alreadyUnlocked then
            return $ Goal.reach <$> mission
        else do
            missions <- selectList [ MissionUser      ==. who
                                   , MissionCharacter ==. charID
                                   ] []
            return $ setObjectives mission missions
    return $ zip mission objectives

-- | If @i >= length goals@, this will do nothing.
data GoalIndex = GoalIndex
    { goals :: Seq Goal
    , char  :: CharacterId
    , i     :: Int
    }

-- | Inserts progress on a mission into the database.
insertProgress :: ∀ m. MonadIO m
               => Key User
               -> Int -- ^ Progress to add.
               -> GoalIndex
               -> SqlPersistT m Bool -- ^ Returns True if the character unlocks.
insertProgress who amount GoalIndex{goals, char, i}
  | not canUpdate = return False
  | otherwise     = do
        alreadyUnlocked <- exists unlockedChar
        if alreadyUnlocked then
            return True
        else do
            upsert (Mission who char i amount)
                   [ MissionProgress +=. amount ]
            complete <- completed goals <$> selectList missionChar []
            when complete do
                deleteWhere missionChar
                void $ insertUnique $ Unlocked who char
            return complete
  where
    canUpdate = case goals !? i of
        Just Reach{spanning, reach} -> spanning == Career || amount >= reach
        Nothing                     -> False
    unlockedChar = [ UnlockedUser      ==. who
                   , UnlockedCharacter ==. char
                   ]
    missionChar  = [ MissionUser      ==. who
                   , MissionCharacter ==. char
                   ]

-- | Attempts to update the database with progress on a mission.
-- Fails if the user is not logged in. Also fails in the unlikely circumstances
-- of the mission not existing, the objective index exceeding the size of the
-- mission, or the Character not existing in the character ID database.
updateProgress :: Progress -> App.Handler Bool
updateProgress Progress{amount = 0} = return False
updateProgress Progress{character, objective, amount} = fromMaybe False <$> runMaybeT do
    Just who  <- Auth.maybeAuthId
    goals     <- hoistMaybe $ lookup character Missions.map
    guard $ objective < length goals
    Just char <- characterID character
    liftDB $ insertProgress who amount GoalIndex { goals, char, i = objective }

-- | Using a list of database mission entries for a user, maps goals onto the
-- user's progress toward those goals.
setObjectives :: Seq Goal -> [Entity Mission] -> Seq Int
setObjectives xs objectives = foldl' f (0 <$ xs) objectives
  where
    f acc (Entity _ Mission{objective, progress}) =
        Seq.update objective progress acc

-- | Returns true if a user has completed a given mission.
completed :: Seq Goal -> [Entity Mission] -> Bool
completed mission objectives = and . zipWith ((<=) . Goal.reach) mission
    $ setObjectives mission objectives

-- | Extracts 'Goal.Win' progress from a winning user's team.
winners :: Bimap CharacterId Text
        -> [Text] -> Unlocks
        -> [GoalIndex]
winners ids team unlocks = do
    Goal.Mission{char, goals} <- Missions.list
    guard $ char ∉ unlocks
    (i, Win _ team') <- zip [0..] $ Goal.objective <$> toList goals
    guard $ all (∈ team) team'
    charID <- Bimap.lookupR char ids
    return GoalIndex { goals, char = charID, i }

newUsage :: CharacterId -> Usage
newUsage x = Usage x 0 0 0 0

usageUpsert :: Usage -> [Update Usage]
usageUpsert Usage{wins, losses, picked, unpicked} = mapMaybe makeUpsert
    $ [ (UsageWins,     wins)
      , (UsageLosses,   losses)
      , (UsagePicked,   picked)
      , (UsageUnpicked, unpicked)
      ]
  where
    makeUpsert (_,     0) = Nothing
    makeUpsert (field, n) = Just $ field +=. n

upsertUsage :: ∀ backend m. ( PersistUniqueWrite backend
                            , MonadIO m
                            , PersistRecordBackend Usage backend
                            )
            => Usage -> ReaderT backend m (Entity Usage)
upsertUsage usage = upsert usage $ usageUpsert usage

-- | Updates 'Goal.Win' progress with the user's team.
-- This function should only be called when the user logged in wins a match.
processWin :: [Text] -> App.Handler ()
processWin team = do
    who      <- Auth.requireAuthId
    ids      <- getsYesod App.characterIDs
    unlocks  <- unlocked
    let chars = mapMaybe (`Bimap.lookupR` ids) team
    runDB do
        mapM_ (void . updateUsage) chars
        mapM_ (void . insertProgress who 1) $ winners ids team unlocks
  where
    updateUsage char = upsertUsage (newUsage char) { Usage.picked = 1
                                                   , Usage.wins   = 1
                                                   }

-- | Resets all 'Goal.WinConsecutive' win progress to 0.
-- This function should only be called when the user logged in loses a match or
-- ties.
processDefeat :: [Text] -> App.Handler ()
processDefeat team = do
    who <- Auth.requireAuthId
    ids <- getsYesod App.characterIDs
    runDB do
        mapM_ (resetGoal ids who) Missions.consecutiveWins
        mapM_ (void . updateUsage) $ mapMaybe (`Bimap.lookupR` ids) team
  where
    updateUsage char = upsertUsage (newUsage char) { Usage.picked = 1
                                                   , Usage.losses = 1
                                                   }

-- | Updates usage stats after a game.
-- This function should always be called at the end of a game.
processUnpicked :: [Text] -> App.Handler ()
processUnpicked team = do
    ids     <- getsYesod App.characterIDs
    unlocks <- unlocked
    runDB . mapM_ (void . updateUsage) . mapMaybe (`Bimap.lookupR` ids) . toList
        $ unlocks \\ setFromList team
  where
    updateUsage char = upsertUsage (newUsage char) { Usage.unpicked = 1 }

-- | Resets progress toward a goal to 0.
resetGoal :: ∀ m. MonadIO m
          => Bimap CharacterId Text -> Key User -> (Text, Int)
          -> SqlPersistT m ()
resetGoal ids who ((`Bimap.lookupR` ids) -> Just char, i) =
    deleteWhere
    [ MissionUser      ==. who
    , MissionCharacter ==. char
    , MissionObjective ==. i
    ]
resetGoal _ _ _ = return ()

-- When ladder matches are introduced, these two will become more complicated.

-- | Awards DNA upon completing a match and returns a list of DNA gains,
-- paired with textual descriptions of why each was awarded.
awardDNA :: Queue.Section -> Outcome -> Maybe War -> App.Handler [Reward]
awardDNA Queue.Private _     _   = return mempty
awardDNA Queue.Quick outcome war = do
    (who, user)   <- Auth.requireAuthPair
    dnaConf       <- getsYesod $ Settings.dnaConf . App.settings
    UTCTime day _ <- liftIO getCurrentTime
    let jDay       = Just day
        tallies    = tallyDNA Queue.Quick outcome war dnaConf jDay user
    runDB . update who $ updateLatestWin outcome jDay
        [ UserLatestGame =. jDay
        , UserDna       +=. sum (Reward.amount <$> tallies)
        ]
    return tallies

-- | Modifies 'UserLatestWin' to today if the user won.
-- This is used to calculate first-win-of-the-day bonuses.
updateLatestWin :: Outcome -> Maybe Day -> [Update User] -> [Update User]
updateLatestWin Victory day xs = (UserLatestWin =. day) : xs
updateLatestWin _       _   xs = xs

-- | Processes DNA gains for 'awardDNA'.
tallyDNA :: Queue.Section -> Outcome -> Maybe War -> Settings.DNA -> Maybe Day
         -> User -> [Reward]
tallyDNA section outcome war dnaConf day User { latestGame
                                              , latestWin
                                              , streak
                                              } = filter ((> 0) . Reward.amount)
    [ Reward (tshow outcome) $       outcomeDNA section outcome dnaConf
    , Reward "First Game of the Day" dailyGame
    , Reward "First Win of the Day " dailyWin
    , Reward "Win Streak"            winStreak
    , Reward "War Bonus"             warWin
    ]
  where
    dailyGame
      | latestGame == day  = 0
      | otherwise          = Settings.dailyGame dnaConf
    dailyWin
      | outcome /= Victory = 0
      | latestWin == day   = 0
      | otherwise          = Settings.dailyWin dnaConf
    winStreak
      | outcome /= Victory = 0
      | streak < 1         = 0
      | Settings.useStreak dnaConf = floor . sqrt @Float
                                   . fromIntegral $ streak - 1
      | otherwise          = 0
    warWin
      | outcome /= Victory = 0
      | isNothing war      = 0
      | otherwise          = Settings.warWin dnaConf

-- | DNA rewards for completing games, as configured in
--  [config/settings.yml](config.settings.yml).
outcomeDNA :: Queue.Section -> Outcome -> Settings.DNA -> Int
outcomeDNA Queue.Private _     = const 0
outcomeDNA Queue.Quick Victory = Settings.quickWin
outcomeDNA Queue.Quick Defeat  = Settings.quickLose
outcomeDNA Queue.Quick Tie     = Settings.quickTie

-- | Returns usage stats about all characters in the database.
getUsageRates :: App.Handler [UsageRate]
getUsageRates = do
    ids   <- getsYesod App.characterIDs
    chars <- runDB $ selectList [] []
    return $ mapMaybe (findUsage ids) chars

-- | Matches a @Usage@ with a 'Character' from 'Characters.map'.
findUsage :: Bimap CharacterId Text -> Entity Usage -> Maybe UsageRate
findUsage ids (Entity _ usage@Usage{character}) = do
    ident <- Bimap.lookup character ids
    char  <- Characters.lookup ident
    return $ UsageRate.new char usage
