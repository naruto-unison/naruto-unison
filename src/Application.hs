{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Yesod-provided implementation of 'Foundation'.
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

import StandardLibrary

import qualified System.Log.FastLogger                as FastLogger
import qualified Control.Monad.Logger                 as Logger
import qualified STMContainers.Map                    as STMMap
import qualified Database.Persist.Postgresql          as SQL
import qualified Language.Haskell.TH.Syntax           as Syntax
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Middleware.RequestLogger as WaiLogger

import Network.Wai.Middleware.RequestLogger (IPAddrSource (..), OutputFormat (..))

import Core.Import
import Handler.Embed
import Handler.Play
import Handler.Client
import Handler.Site
import Handler.Admin
import Handler.Forum

mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    appHttpManager <- newManager
    appLogger  <- FastLogger.newStdoutLoggerSet FastLogger.defaultBufSize
                  >>= makeYesodLogger
    appStatic  <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)
    appQueue    <- newTChanIO
    appPractice <- STMMap.newIO

    let mkFoundation appConnPool = App {..}
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    pool <- flip Logger.runLoggingT logFunc $ SQL.createPostgresqlPool
        (SQL.pgConnStr  $ appDatabaseConf appSettings)
        (SQL.pgPoolSize $ appDatabaseConf appSettings)

    Logger.runLoggingT (SQL.runSqlPool (runMigration migrateAll) pool) logFunc

    return $ mkFoundation pool

makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Wai.Middleware
makeLogWare foundation =
    WaiLogger.mkRequestLogger def
        { WaiLogger.outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , WaiLogger.destination = 
            WaiLogger.Logger $ loggerSet $ appLogger foundation
        }

warpSettings :: App -> Warp.Settings
warpSettings foundation =
    Warp.setPort (appPort $ appSettings foundation) $
    Warp.setHost (appHost $ appSettings foundation) $
    Warp.setOnException (\_req e ->
        when (Warp.defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(Syntax.qLocation >>= Logger.liftLoc)
            "yesod"
            LevelError
            (FastLogger.toLogStr $ "Exception from Warp: " ++ show e))
      Warp.defaultSettings

getApplicationDev :: IO (Warp.Settings, Application)
getApplicationDev = do
    settings <- loadYamlSettings [configSettingsYml] [] useEnv
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

develMain :: IO ()
develMain = develMainHelper getApplicationDev

appMain :: IO ()
appMain = do
    settings <- loadYamlSettingsArgs
        [configSettingsYmlValue]
        useEnv
    foundation <- makeFoundation settings
    app <- makeApplication foundation
    Warp.runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (Warp.getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

handler :: ∀ a. Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

db :: ∀ a. ReaderT SqlBackend (HandlerFor App) a -> IO a
db = handler . runDB
