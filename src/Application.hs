{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This is the main module of the library.
-- It implements "Application.App" and holds the functions used by applications.
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import ClassyPrelude
import Yesod

import           Control.Concurrent (forkIO)
import qualified Control.Monad.Logger as Logger
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Cache as Cache
import qualified Data.HashTable as HashTable
import qualified Database.Persist.Postgresql as Sql
import           Database.Persist.Sql (SqlBackend, SqlPersistT)
import           Data.Time.Clock.System (SystemTime(..), getSystemTime)
import qualified Language.Haskell.TH.Syntax as TH
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Log.FastLogger as FastLogger
import qualified Yesod.Auth as Auth
import qualified Yesod.Default.Config2 as DefaultConfig
import qualified Yesod.Static as Static

import           Application.App (App(..), Route(..))
import qualified Application.App as App
import qualified Application.Logger as AppLogger
import           Application.Model (CharacterId)
import qualified Application.Model as Model
import           Application.Settings (Settings(Settings))
import qualified Application.Settings as Settings
import           Handler.Admin
import           Handler.Client
import           Handler.Client.Data (writeDataJS)
import           Handler.Embed
import           Handler.Play
import           Handler.Profile
import qualified Handler.Queue as Queue
import           Handler.Site
import qualified Mission

mkYesodDispatch "App" App.resourcesApp

-- | Initializes the database:
-- loads 'Application.Model',
-- runs migrations from [config/db.sql](db.sql),
-- initializes the character ID table with 'Mission.initDB',
-- and returns the table to be stored in 'characterIDs'.
initDB :: ∀ m. MonadIO m => SqlPersistT m (Bimap CharacterId Text)
initDB = do
    Sql.runMigration Model.migrateAll
    dbMigrationsSql <- readFile "config/db.sql"
    Sql.rawExecute (decodeUtf8 dbMigrationsSql) []
    Mission.initDB

-- | Initializes the core of the app with a logger and a database.
makeFoundation :: Settings -> IO App
makeFoundation settings@Settings { databaseConf
                                 , mutableStatic
                                 , practiceCacheExpiry
                                 , queueTableSizeHint
                                 , staticDir
                                 } = do
    writeDataJS staticDir
    httpManager <- TLS.getGlobalManager
    loggerSet   <- FastLogger.newStdoutLoggerSet FastLogger.defaultBufSize
    logger      <- DefaultConfig.makeYesodLogger loggerSet
    static      <- staticMode staticDir
    quick       <- HashTable.newWithDefaults queueTableSizeHint
    private     <- newBroadcastTChanIO
    practice    <- Cache.newCache . Just $ fromInteger practiceCacheExpiry

    startup                  <- getCurrentTime
    MkSystemTime timestamp _ <- getSystemTime

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation characterIDs connPool = App
            { startup
            , timestamp
            , settings
            , static
            , connPool
            , httpManager
            , logger
            , practice
            , private
            , quick
            , characterIDs
            }
        tempFoundation = mkFoundation Bimap.empty
            $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation logger

    pool <- flip Logger.runLoggingT logFunc $ Sql.createPostgresqlPool
        (Sql.pgConnStr  databaseConf)
        (Sql.pgPoolSize databaseConf)

    charIDs <- Logger.runLoggingT (Sql.runSqlPool initDB pool) logFunc
    let foundation = mkFoundation charIDs pool
    forkIO $ Queue.quickManager foundation
    return foundation
  where
    staticMode
      | mutableStatic = Static.staticDevel
      | otherwise     = Static.static

-- | Convert foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- AppLogger.makeLogWare foundation
    logWare . defaultMiddlewaresNoLogging <$> toWaiAppPlain foundation

-- | Warp settings from app settings.
warpSettings :: App -> Warp.Settings
warpSettings foundation@App{logger, settings = Settings{host, port}} =
    Warp.setPort port
        . Warp.setHost host
        . Warp.setOnException exceptionHandler
        $ Warp.defaultSettings
  where
    exceptionHandler _req e = when (Warp.defaultShouldDisplayException e)
        $ messageLoggerSource foundation logger
              $(Logger.liftLoc =<< TH.qLocation)
              "yesod"
              LevelError
              (FastLogger.toLogStr $ "Exception from Warp: " ++ show e)

-- | Warp settings and WAI Application for @yesod devel@.
getApplicationDev :: IO (Warp.Settings, Application)
getApplicationDev = do
    settings   <- getAppSettings
    foundation <- makeFoundation settings
    wsettings  <- DefaultConfig.getDevSettings $ warpSettings foundation
    app        <- makeApplication foundation
    return (wsettings, app)

-- | Loads config settings from environment variables and config files.
getAppSettings :: IO Settings
getAppSettings = DefaultConfig.loadYamlSettings
    [DefaultConfig.configSettingsYml] [] DefaultConfig.useEnv

-- | Main function for use by @yesod devel@.
develMain :: IO ()
develMain = DefaultConfig.develMainHelper getApplicationDev

-- | The @main@ function for an executable running the site.
appMain :: IO ()
appMain = do
    settings <- DefaultConfig.loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [Settings.configSettingsYmlValue]

        -- allow environment variables to override
        DefaultConfig.useEnv

    foundation <- makeFoundation settings
    app        <- makeApplication foundation
    Warp.runSettings (warpSettings foundation) app

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------

-- | Initializes the application for use in GHCi.
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings   <- getAppSettings
    foundation <- makeFoundation settings
    wsettings  <- DefaultConfig.getDevSettings $ warpSettings foundation
    app1       <- makeApplication foundation
    return (Warp.getPort wsettings, foundation, app1)

-- | This doesn't actually do anything.
shutdownApp :: App -> IO ()
shutdownApp _ = return ()

---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler.
handler :: App.Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip App.unsafeHandler h

-- | Run DB queries.
db :: ReaderT SqlBackend App.Handler a -> IO a
db = handler . runDB
