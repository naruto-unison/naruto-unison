{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import ClassyPrelude hiding (Handler)
import Yesod

import qualified Control.Monad.Logger as Logger
import qualified Data.Cache as Cache
import           Data.Default (def)
import qualified Database.Persist.Postgresql as Sql
import           Database.Persist.Postgresql (SqlBackend)
import qualified Language.Haskell.TH.Syntax as TH
import qualified Network.HTTP.Client.TLS as TLS
import           Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           Network.Wai.Middleware.RequestLogger (Destination (Logger), IPAddrSource(..), OutputFormat(..))
import           System.Clock (TimeSpec(..))
import qualified System.Log.FastLogger as FastLogger
import qualified Yesod.Static as Static
import qualified Yesod.Auth as Auth
import qualified Yesod.Core.Types as YesodTypes
import qualified Yesod.Default.Config2 as DefaultConfig


import qualified Core.App as App
import           Core.App (App(..), Handler, Route(..))
import qualified Core.AppSettings as AppSettings
import           Core.AppSettings (AppSettings)
import qualified Core.Settings as Settings
import qualified Core.Model as Model

import Handler.Admin
import Handler.Client
import Handler.Embed
import Handler.Forum
import Handler.Play
import Handler.Site

mkYesodDispatch "App" App.resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation settings = do
    httpManager <- TLS.getGlobalManager
    logger      <- DefaultConfig.makeYesodLogger
                   =<< FastLogger.newStdoutLoggerSet FastLogger.defaultBufSize
    static      <- staticMode $ AppSettings.staticDir settings
    queue       <- newTChanIO
    practice    <- Cache.newCache . Just $ TimeSpec 3600 0

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation connPool = App {..}
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation logger

    pool <- flip Logger.runLoggingT logFunc .
            Logger.filterLogger (const (/= Logger.LevelDebug)) $
            Sql.createPostgresqlPool
                (Sql.pgConnStr  $ AppSettings.databaseConf settings)
                (Sql.pgPoolSize $ AppSettings.databaseConf settings)

    Logger.runLoggingT
        (Sql.runSqlPool (Sql.runMigration Model.migrateAll) pool) logFunc

    dbMigrationsSql <- readFile "config/db.sql"
    Logger.runLoggingT
      (Sql.runSqlPool (Sql.rawExecute (decodeUtf8 dbMigrationsSql) []) pool)
      logFunc

    return $ mkFoundation pool
  where
    staticMode
      | AppSettings.mutableStatic settings = Static.staticDevel
      | otherwise                          = Static.static

-- | Convert foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    RequestLogger.mkRequestLogger def
        { RequestLogger.outputFormat =
            if AppSettings.detailedRequestLogging $ App.settings foundation
                then Detailed True
                else Apache $
                        if AppSettings.ipFromHeader $ App.settings foundation
                            then FromFallback
                            else FromSocket
        , RequestLogger.destination =
            Logger . YesodTypes.loggerSet $ App.logger foundation
        }

warpSettings :: App -> Warp.Settings
warpSettings foundation =
      Warp.setPort (AppSettings.port $ App.settings foundation)
    $ Warp.setHost (AppSettings.host $ App.settings foundation)
    $ Warp.setOnException (\_req e ->
        when (Warp.defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (App.logger foundation)
            $(Logger.liftLoc =<< TH.qLocation)
            "yesod"
            LevelError
            (FastLogger.toLogStr $ "Exception from Warp: " ++ show e))
      Warp.defaultSettings

-- | Warp settings and WAI Application for @yesod devel@.
getApplicationDev :: IO (Warp.Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- DefaultConfig.getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
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

getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings   <- getAppSettings
    foundation <- makeFoundation settings
    wsettings  <- DefaultConfig.getDevSettings $ warpSettings foundation
    app1       <- makeApplication foundation
    return (Warp.getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip App.unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB
