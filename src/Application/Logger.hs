-- | A slightly customized logging system for Warp that hides successful queries
-- if 'Settings.detailedRequestLogging' is disabled.
module Application.Logger (makeLogWare) where

import ClassyPrelude

import           Control.Concurrent (forkIO, threadDelay)
import           Data.Default (def)
import           Network.HTTP.Types (Status(Status))
import           Network.Wai (Middleware)
import qualified Network.Wai as Wai
import qualified Network.Wai.Header as Header
import           Network.Wai.Logger (ApacheLoggerActions)
import qualified Network.Wai.Logger as WaiLogger
import           Network.Wai.Middleware.RequestLogger (Destination (Logger), IPAddrSource(..), OutputFormat(..))
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified System.Log.FastLogger as FastLogger
import qualified Yesod.Core.Types as YesodTypes

import           Application.App (App(App))
import qualified Application.App as App
import           Application.Settings (Settings(Settings))
import qualified Application.Settings as Settings

getDateGetter :: IO () -> IO (IO ByteString)
getDateGetter flusher = do
    (getter, updater) <- WaiLogger.clockDateCacher
    void . forkIO . forever $ do
        threadDelay 1_000_000
        updater
        flusher
    return getter

makeLogWare :: App -> IO Middleware
makeLogWare App{logger, settings = Settings{detailedRequestLogging = True}} =
    RequestLogger.mkRequestLogger def
        { RequestLogger.outputFormat = Detailed True
        , RequestLogger.destination  = Logger $ YesodTypes.loggerSet logger
        }

makeLogWare App{logger, settings = Settings{ipFromHeader}} = do
        dateGetter <- getDateGetter flusher
        apacheMiddleware <$> WaiLogger.initLogger ipSrc callback dateGetter
  where
    ipSrc
      | ipFromHeader = FromFallback
      | otherwise    = FromSocket
    logger'   = YesodTypes.loggerSet logger
    flusher  = FastLogger.flushLogStr logger'
    callback = FastLogger.LogCallback (FastLogger.pushLogStr logger') flusher

apacheMiddleware :: ApacheLoggerActions -> Middleware
apacheMiddleware ala app req sendResponse = app req $ \res -> do
    let headers    = Wai.responseHeaders res
        status     = Wai.responseStatus res
        Status n _ = status
    when (n /= 200 && n /= 304)
        . WaiLogger.apacheLogger ala req status $ Header.contentLength headers
    sendResponse res
