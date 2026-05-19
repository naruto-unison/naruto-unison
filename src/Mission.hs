-- | Database handling for character missions, which users complete in order to
-- unlock new characters.
module Mission
  ( initDB
  , progress
  , Unlocks, allUnlocked, unlocked
  , characterID
  , userMission
  , processWin, processDefeat, processUnpicked
  , awardDNA
  , getUsageRates
  ) where

import ClassyPrelude hiding ((\\))
import Yesod

import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.List ((\\))
import qualified Data.Sequence as Seq
import           Database.Persist.Sql (SqlPersistT)
import qualified Database.Persist.Sql as Sql
import qualified Yesod.Auth as Auth

import           Application.App (Handler)
import qualified Application.App as App
import           Application.Fields (Privilege(..))
import           Application.Model (Character(Character), CharacterId, EntityField(..), Mission(Mission), Unlocked(Unlocked), Usage(..), User(User))
import qualified Application.Model
import qualified Application.Settings as Settings
import qualified Game.Characters as Characters
import qualified Game.Model.Character as Character
import           Handler.Client.Reward (Reward(Reward))
import qualified Handler.Client.Reward as Reward
import           Handler.Play.Match (Outcome(..))
import           Handler.Play.War (War)
import qualified Handler.Queue as Queue
import           Mission.Goal (Goal(Reach), Span(..))
import qualified Mission.Goal as Goal
import qualified Mission.Missions as Missions
import           Mission.Progress (Progress(Progress))
import qualified Mission.Progress
import           Mission.UsageRate (UsageRate)
import qualified Mission.UsageRate as UsageRate
import           Util ((!?), (∉))

-- | Starts up the mission database by mapping every Character to a database
-- ID. Returns the map, which goes into 'App.characterIDs'.
-- 'Character.ident' is used as the key.
initDB :: ∀ m. MonadIO m => SqlPersistT m (Bimap CharacterId Text)
initDB = do
    charEntities <- selectList [] []
    let chars = entityVal <$> charEntities
    insertMany_ $ filter (∉ chars) charList
    makeMap <$> selectList [] []
  where
    charList = Character . Character.ident <$> Characters.list

-- | Looks up a Character's ID in 'App.characterIDs' using 'Character.ident'.
characterID :: Text -> MaybeT Handler CharacterId
characterID name = Bimap.lookupR name =<< getsYesod App.characterIDs

-- | Processes the database list of characters into a map between IDs and
-- 'Character.ident'.
makeMap :: [Entity Character] -> Bimap CharacterId Text
makeMap chars = Bimap.fromList $ mapMaybe maybePair chars
  where
    maybePair (Entity charId Character{characterName}) =
        (charId, ) . Character.ident <$> Characters.lookup characterName

newtype Unlocks = Unlocks (HashSet Text)
    deriving (Show, Read, Semigroup, Monoid, MonoFoldable, ToJSON)

type instance Element Unlocks = Text

allUnlocked :: Unlocks
allUnlocked = Unlocks $ keysSet Characters.map

-- | 'Character.ident' collection of all Characters that the user has unlocked.
-- If not logged in, all Characters are returned.
-- If @unlock-all@ in [config/settings.yml](config/settings.yml) is set to true,
-- all Characters will always be returned.
unlocked :: Handler Unlocks
unlocked = cached $ maybe allUnlocked Unlocks <$> runMaybeT do
    unlockAll <- getsYesod $ Settings.unlockAll . App.settings
    guard unlockAll
    who       <- MaybeT Auth.maybeAuthId
    privilege <- App.getPrivilege
    guard $ privilege < Moderator
    getUnlocked <$> getsYesod App.characterIDs
                <*> lift (runDB (selectList [UnlockedUser ==. who] []))
  where
    getUnlocked ids unlocks = freeChars `union` setFromList
                              (mapMaybe (look ids) unlocks)
    look ids (Entity _ Unlocked{unlockedCharacter}) =
        Bimap.lookup unlockedCharacter ids

-- | 'Character.ident's of all Characters without DNA 'Character.price's.
freeChars :: HashSet Text
freeChars = setFromList dna `difference` keysSet Missions.map
  where
    dna = Character.ident <$> filter ((== 0) . Character.price) Characters.list
{-# NOINLINE freeChars #-}

-- | Returns the user's progress on a single Character's mission.
-- Returns @Nothing@ if the user is not logged in, the Character does not
-- have a mission, or the user has already completed their mission.
-- Otherwise, returns a list of goals paired with the user's progress on each.
userMission :: Text -> Handler (Maybe (Seq (Goal, Int)))
userMission char = fromMaybe mempty <$> runMaybeT do
    who        <- MaybeT Auth.maybeAuthId
    charID     <- characterID char
    mission    <- MaybeT . return $ lookup char Missions.map
    objectives <- lift $ runDB do
        alreadyUnlocked <-
            selectFirst [UnlockedUser ==. who, UnlockedCharacter ==. charID] []
        if isJust alreadyUnlocked then
            return $ Goal.reach <$> mission
        else
            setObjectives mission <$>
                selectList [MissionUser ==. who, MissionCharacter ==. charID] []
    return . Just $ zip mission objectives

-- | If @i >= length goals@, this will do nothing.
data GoalIndex = GoalIndex
    { goals :: Seq Goal
    , char  :: CharacterId
    , i     :: Int
    }

-- | Inserts progress on a mission into the database.
updateProgress :: ∀ m. MonadIO m
               => Key User
               -> Int -- ^ Progress to add.
               -> GoalIndex
               -> SqlPersistT m Bool -- ^ Returns True if the character unlocks.
updateProgress who amount GoalIndex{goals, char, i} =
    if not canUpdate then return False else do
        alreadyUnlocked <- isJust <$> selectFirst unlockedChar []
        if alreadyUnlocked then
            return True
        else do
            upsert (Mission who char i amount)
                   [MissionProgress +=. amount]
            complete <- completed goals <$> selectList missionChar []
            when complete $ void do
                deleteWhere missionChar
                insertUnique $ Unlocked who char
            return complete
  where
    canUpdate = case goals !? i of
        Just Reach{spanning, reach} -> spanning == Career || amount >= reach
        Nothing                     -> False
    unlockedChar = [UnlockedUser ==. who, UnlockedCharacter ==. char]
    missionChar  = [MissionUser ==. who, MissionCharacter ==. char]

-- | Attempts to update the database with progress on a mission.
-- Fails if the user is not logged in. Also fails in the unlikely circumstances
-- of the mission not existing, the objective index exceeding the size of the
-- mission, or the Character not existing in the character ID database.
progress :: Progress -> Handler Bool
progress Progress{amount = 0} = return False
progress Progress{character, objective, amount} =
    fromMaybe False <$> runMaybeT do
        who   <- MaybeT Auth.maybeAuthId
        goals <- MaybeT . return $ lookup character Missions.map
        guard $ objective < length goals
        char  <- characterID character
        lift . runDB
            $ updateProgress who amount GoalIndex { goals, char, i = objective }

-- | Using a list of database mission entries for a user, maps goals onto the
-- user's progress toward those goals.
setObjectives :: Seq Goal -> [Entity Mission] -> Seq Int
setObjectives xs objectives = foldl' f (0 <$ xs) objectives
  where
    f acc (Entity _ Mission{missionObjective, missionProgress}) =
        Seq.update missionObjective missionProgress acc

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
    (i, Goal.Win _ team') <- zip [0..] $ Goal.objective <$> toList goals
    guard . null $ team' \\ team
    charID <- Bimap.lookupR char ids
    return GoalIndex { goals, char = charID, i }

newUsage :: CharacterId -> Usage
newUsage x = Usage x 0 0 0 0

-- | Updates 'Goal.Win' progress with the user's team.
-- This function should only be called when the user logged in wins a match.
processWin :: [Text] -> Handler ()
processWin team = do
    who      <- Auth.requireAuthId
    ids      <- getsYesod App.characterIDs
    unlocks  <- unlocked
    let chars = mapMaybe (`Bimap.lookupR` ids) team
    runDB do
        traverse_ ups chars
        traverse_ (updateProgress who 1) $ winners ids team unlocks
  where
    ups char = upsert (newUsage char){ usagePicked = 1, usageWins = 1 }
               [UsagePicked +=. 1, UsageWins +=. 1]

-- | Resets all 'Goal.WinConsecutive' win progress to 0.
-- This function should only be called when the user logged in loses a match or
-- ties.
processDefeat :: [Text] -> Handler ()
processDefeat team = do
    who <- Auth.requireAuthId
    ids <- getsYesod App.characterIDs
    runDB do
        traverse_ (resetGoal ids who) Missions.consecutiveWins
        traverse_ ups $ mapMaybe (`Bimap.lookupR` ids) team
  where
    ups char = upsert (newUsage char){ usagePicked = 1, usageLosses = 1 }
               [UsagePicked +=. 1, UsageLosses +=. 1]

-- | Updates usage stats after a game.
-- This function should always be called at the end of a game.
processUnpicked :: [Text] -> Handler ()
processUnpicked team = do
    ids             <- getsYesod App.characterIDs
    Unlocks unlocks <- unlocked
    runDB . traverse_ ups . mapMaybe (`Bimap.lookupR` ids) . toList
        $ unlocks `difference` setFromList team
  where
    ups char = upsert (newUsage char){ usageUnpicked = 1 }
               [UsageUnpicked +=. 1]

-- | Resets progress toward a goal to 0.
resetGoal :: ∀ m. MonadIO m
          => Bimap CharacterId Text -> Key User -> (Text, Int)
          -> SqlPersistT m ()
resetGoal ids who ((`Bimap.lookupR` ids) -> Just char, i) =
    Sql.deleteWhere
    [MissionUser ==. who, MissionCharacter ==. char, MissionObjective ==. i]
resetGoal _ _ _ = return ()

-- When ladder matches are introduced, these two will become more complicated.

-- | Awards DNA upon completing a match and returns a list of DNA gains,
-- paired with textual descriptions of why each was awarded.
awardDNA :: Queue.Section -> Outcome -> Maybe War -> Handler [Reward]
awardDNA Queue.Private _     _   = return []
awardDNA Queue.Quick outcome war = do
    (who, user)   <- Auth.requireAuthPair
    dnaConf       <- getsYesod $ Settings.dnaConf . App.settings
    UTCTime day _ <- liftIO getCurrentTime
    let jDay       = Just day
        tallies    = tallyDNA Queue.Quick outcome war dnaConf jDay user
    runDB . Sql.update who $ updateLatestWin outcome jDay
        [UserLatestGame =. jDay, UserDna +=. sum (Reward.amount <$> tallies)]
    return tallies

-- | Modifies 'UserLatestWin' to today if the user won.
-- This is used to calculate first-win-of-the-day bonuses.
updateLatestWin :: Outcome -> Maybe Day -> [Update User] -> [Update User]
updateLatestWin Victory day xs = (UserLatestWin =. day) : xs
updateLatestWin _       _   xs = xs

-- | Processes DNA gains for 'awardDNA'.
tallyDNA :: Queue.Section -> Outcome -> Maybe War -> Settings.DNA -> Maybe Day
         -> User -> [Reward]
tallyDNA section outcome war dnaConf day User { userLatestGame
                                              , userLatestWin
                                              , userStreak
                                              } = filter ((> 0) . Reward.amount)
    [ Reward (tshow outcome) $       outcomeDNA section outcome dnaConf
    , Reward "First Game of the Day" dailyGame
    , Reward "First Win of the Day " dailyWin
    , Reward "Win Streak"            winStreak
    , Reward "War Bonus"             warWin
    ]
  where
    dailyGame
      | userLatestGame == day = 0
      | otherwise             = Settings.dailyGame dnaConf
    dailyWin
      | outcome /= Victory    = 0
      | userLatestWin == day  = 0
      | otherwise             = Settings.dailyWin dnaConf
    winStreak
      | outcome /= Victory    = 0
      | userStreak < 1        = 0
      | Settings.useStreak dnaConf = floor . (sqrt :: Float -> Float)
                                   . fromIntegral $ userStreak - 1
      | otherwise             = 0
    warWin
      | outcome /= Victory    = 0
      | isNothing war         = 0
      | otherwise             = Settings.warWin dnaConf

-- | DNA rewards for completing games, as configured in
--  [config/settings.yml](config.settings.yml).
outcomeDNA :: Queue.Section -> Outcome -> Settings.DNA -> Int
outcomeDNA Queue.Private _     = const 0
outcomeDNA Queue.Quick Victory = Settings.quickWin
outcomeDNA Queue.Quick Defeat  = Settings.quickLose
outcomeDNA Queue.Quick Tie     = Settings.quickTie

-- | Returns usage stats about all characters in the database.
getUsageRates :: Handler [UsageRate]
getUsageRates = mapMaybe . findUsage
    <$> getsYesod App.characterIDs
    <*> runDB (selectList [] [])

-- | Matches a @Usage@ with a 'Character' from 'Characters.map'.
findUsage :: Bimap CharacterId Text -> Entity Usage -> Maybe UsageRate
findUsage ids (Entity _ usage@Usage{usageCharacter}) = do
    name <- Bimap.lookup usageCharacter ids
    char <- Characters.lookup name
    return $ UsageRate.new char usage
