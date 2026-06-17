-- | A slightly customized logging system for Warp that hides successful queries
-- if 'Settings.detailedRequestLogging' is disabled.
module Application.Logger (makeLogWare) where

import ClassyPrelude

import           Control.Concurrent (forkIO, threadDelay)
import           Network.HTTP.Types (Status(Status))
import qualified Network.Wai as Wai
import qualified Network.Wai.Header as WaiHeader
import qualified Network.Wai.Logger as WaiLogger
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified System.Log.FastLogger as FastLogger
import qualified Yesod.Core.Types

import           Application.App (App(App))
import qualified Application.App as App
import           Application.Settings (Settings(Settings))
import qualified Application.Settings as Settings

getDateGetter :: IO () -> IO (IO ByteString)
getDateGetter flusher = do
    (getter, updater) <- WaiLogger.clockDateCacher
    forkIO $ forever do
        threadDelay 1_000_000
        updater
        flusher
    return getter

makeLogWare :: App -> IO Wai.Middleware
makeLogWare App{logger, settings = Settings{detailedRequestLogging = True}} =
    RequestLogger.mkRequestLogger RequestLogger.defaultRequestLoggerSettings
        { RequestLogger.outputFormat = RequestLogger.Detailed True
        , RequestLogger.destination  = RequestLogger.Logger logger.loggerSet
        }

makeLogWare App{logger, settings = Settings{ipFromHeader}} = do
        dateGetter <- getDateGetter flusher
        loggerActions <- WaiLogger.initLogger ipSrc callback dateGetter
        return $ apacheMiddleware loggerActions
  where
    ipSrc
      | ipFromHeader = WaiLogger.FromFallback
      | otherwise    = WaiLogger.FromSocket
    logger'   = logger.loggerSet
    flusher  = FastLogger.flushLogStr logger'
    callback = FastLogger.LogCallback (FastLogger.pushLogStr logger') flusher

apacheMiddleware :: WaiLogger.ApacheLoggerActions -> Wai.Middleware
apacheMiddleware ala app req sendResponse = app req $ \res -> do
    let headers    = Wai.responseHeaders res
        status     = Wai.responseStatus res
        Status n _ = status
    when (n /= 200 && n /= 304)
        . WaiLogger.apacheLogger ala req status
        $ WaiHeader.contentLength headers
    sendResponse res
