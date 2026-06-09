{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Yesod settings.
module Application.Settings
  ( Settings(..)
  , configSettingsYmlValue
  , widgetFile
  , DNA(..)
  , combineStylesheets
  , combineScripts
  ) where

import ClassyPrelude
import Yesod.Static

import qualified Control.Exception as Exception
import           Data.Aeson ((.!=), (.:), (.:?), FromJSON, Result(..), Value)
import qualified Data.Aeson as Aeson
import           Data.Default (def)
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Yaml as Yaml
import           Database.Persist.Postgresql (PostgresConf)
import qualified Language.Haskell.TH.Syntax as TH
import qualified Network.Wai.Handler.Warp as Warp
import qualified Yesod.Default.Config2 as DefaultConfig
import           Yesod.Default.Util (WidgetFileSettings)
import qualified Yesod.Default.Util as Util

-- | DNA rewards.
data DNA = DNA
    { dailyGame :: Int
    -- ^ When a player completes their first quick/ranked match of the day
    , dailyWin  :: Int
    -- ^ When a player earns their first quick/ranked victory of the day
    ,quickWin   :: Int
    -- ^ Whenever a player wins a quick match
    , quickLose :: Int
    -- ^ Whenever a player loses a quick match
    , quickTie  :: Int
    -- ^ Whenever a player ends a match in a tie
    , warWin    :: Int
    -- ^ Reward for victory in a 'Handler.Play.War.War'.
    , useStreak :: Bool
    -- ^ Add the square root of the user's win streak to the DNA reward
    } deriving (Eq, Ord, Show)

instance FromJSON DNA where
    parseJSON = Aeson.withObject "DNA" \o -> do
        dailyGame <- o.: "daily-game"
        dailyWin  <- o .: "daily-win"
        quickWin  <- o .: "quick-win"
        quickLose <- o .: "quick-lose"
        quickTie  <- o .: "quick-tie"
        warWin    <- o .: "war-win"
        useStreak <- o .: "use-streak"
        return DNA
            { dailyGame
            , dailyWin
            , quickWin
            , quickLose
            , quickTie
            , warWin
            , useStreak
            }

data Settings = Settings
    { unlockAll              :: ~Bool
    -- ^ Allow players to use characters they have not unlocked.
    , turnLength             :: ~Int
    -- ^ Duration of a game turn.
    , practiceCacheExpiry    :: ~Integer
    -- ^ Expiration duration of the cache that holds practice matches.
    , forfeitAfterSkips      :: ~Int
    -- ^ If a character goes past @turnLength@ this many times, they
    -- automatically forfeit.
    , queueTableSizeHint     :: ~Int
    -- ^ Initial size of the queue user info 'Data.HashTable.HashTable'.

    , staticDir              :: ~String
    -- ^ Directory from which to serve static files.
    , databaseConf           :: ~PostgresConf
    -- ^ Configuration settings for accessing the database.
    , dnaConf                :: ~DNA
    -- ^ Configuration settings for rewarding DNA.
    , root                   :: ~(Maybe Text)
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , host                   :: ~Warp.HostPreference
    -- ^ Host/interface the server should bind to.
    , port                   :: ~Int
    -- ^ Port to listen on.
    , ipFromHeader           :: ~Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , detailedRequestLogging :: ~Bool
    -- ^ Use detailed request logging system.
    , shouldLogAll           :: ~Bool
    -- ^ Should all log messages be displayed?
    , reloadTemplates        :: ~Bool
    -- ^ Use the reload version of templates.
    , mutableStatic          :: ~Bool
    -- ^ Assume that files in the static dir may change after compilation.
    , skipCombining          :: ~Bool
    -- ^ Perform no stylesheet/script combining.

    -- Example app-specific configuration values.
    , copyright              :: ~Text
    -- ^ Copyright text to appear in the footer of the page.
    , analytics              :: ~(Maybe Text)
    -- ^ Google Analytics code.

    , authDummyLogin         :: ~Bool
    -- ^ Indicate if auth dummy login should be enabled.
    } deriving (Show)

instance FromJSON Settings where
    parseJSON = Aeson.withObject "Settings" \o -> do
        staticDir              <- o .: "static-dir"
        databaseConf           <- o .: "database"
        dnaConf                <- o .: "dna"
        root                   <- o .:? "approot"
        host                   <- o .: "host"
        port                   <- o .: "port"
        ipFromHeader           <- o .: "ip-from-header"

        dev                    <- o .:? "development"      .!= defaultDev

        detailedRequestLogging <- o .:? "detailed-logging" .!= dev
        shouldLogAll           <- o .:? "should-log-all"   .!= dev
        reloadTemplates        <- o .:? "reload-templates" .!= dev
        mutableStatic          <- o .:? "mutable-static"   .!= dev
        skipCombining          <- o .:? "skip-combining"   .!= dev

        copyright              <- o .:  "copyright"
        analytics              <- o .:? "analytics"

        authDummyLogin         <- o .:? "auth-dummy-login" .!= dev

        unlockAll              <- o .:? "unlock-all"       .!= dev
        turnLength             <- o .: "turn-length"
        practiceCacheExpiry    <- o .: "practice-cache-expiry"
        forfeitAfterSkips      <- o .: "forfeit-after-skips"
        queueTableSizeHint     <- o .: "queue-table-size-hint"
        return Settings
            { unlockAll
            , turnLength          = 1_000_000 * turnLength
            , practiceCacheExpiry = 1_000_000_000 * practiceCacheExpiry
            , host                = fromString host
            , forfeitAfterSkips
            , queueTableSizeHint
            , staticDir
            , databaseConf
            , dnaConf
            , root
            , port
            , ipFromHeader
            , detailedRequestLogging
            , shouldLogAll
            , reloadTemplates
            , mutableStatic
            , skipCombining
            , copyright
            , analytics
            , authDummyLogin
            }
      where
#ifdef DEVELOPMENT
            defaultDev = True
#else
            defaultDev = False
#endif

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> TH.Q TH.Exp
widgetFile
    | reloadTemplates compileTimeAppSettings =
                  Util.widgetFileReload widgetFileSettings
    | otherwise = Util.widgetFileNoReload widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(FileEmbed.embedFile DefaultConfig.configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = case Yaml.decodeEither' configSettingsYmlBS of
    Left decodeError -> Exception.throw decodeError
    Right decoded    -> decoded

-- | A version of @Settings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: Settings
compileTimeAppSettings = case Aeson.fromJSON json of
    Error e          -> error e
    Success settings -> settings
  where
    json = DefaultConfig.applyEnvValue False mempty configSettingsYmlValue


-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

combineStylesheets :: TH.Name -> [Route Static] -> TH.Q TH.Exp
combineStylesheets = combineStylesheets'
    (skipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: TH.Name -> [Route Static] -> TH.Q TH.Exp
combineScripts = combineScripts'
    (skipCombining compileTimeAppSettings)
    combineSettings
