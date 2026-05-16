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

import           Application.App (App(..))
import qualified Application.App as App
import qualified Application.Settings as Settings
import           Util ((<$>.))

getDateGetter :: IO () -> IO (IO ByteString)
getDateGetter flusher = do
    (getter, updater) <- WaiLogger.clockDateCacher
    void . forkIO . forever $ do
        threadDelay 1e6
        updater
        flusher
    return getter

makeLogWare :: App -> IO Middleware
makeLogWare foundation
  | Settings.detailedRequestLogging $ App.settings foundation =
    RequestLogger.mkRequestLogger def
        { RequestLogger.outputFormat = Detailed True
        , RequestLogger.destination =
            Logger . YesodTypes.loggerSet $ App.logger foundation
        }
  | otherwise = apacheMiddleware <$>. WaiLogger.initLogger ipSrc callback
                                  =<< getDateGetter flusher
  where
    ipSrc
      | Settings.ipFromHeader $ App.settings foundation = FromFallback
      | otherwise                                          = FromSocket
    logger   = YesodTypes.loggerSet $ App.logger foundation
    flusher  = FastLogger.flushLogStr logger
    callback = FastLogger.LogCallback (FastLogger.pushLogStr logger) flusher

apacheMiddleware :: ApacheLoggerActions -> Middleware
apacheMiddleware ala app req sendResponse = app req $ \res -> do
    case Wai.responseStatus res of
        Status 200 _ -> return ()
        Status 304 _ -> return ()
        status       -> WaiLogger.apacheLogger ala req status .
                        Header.contentLength $ Wai.responseHeaders res
    sendResponse res
