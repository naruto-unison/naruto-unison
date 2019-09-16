module Core.Logger (makeLogWare) where

import ClassyPrelude

import qualified Control.Concurrent as Concurrent
import           Data.Default (def)
import           Network.HTTP.Types (Status(Status))
import qualified Network.Wai as Wai
import           Network.Wai (Middleware)
import qualified Network.Wai.Header as Header
import qualified Network.Wai.Logger as WaiLogger
import           Network.Wai.Logger (ApacheLoggerActions)
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           Network.Wai.Middleware.RequestLogger (Destination (Logger), IPAddrSource(..), OutputFormat(..))
import qualified System.Log.FastLogger as FastLogger
import qualified Yesod.Core.Types as YesodTypes


import qualified Core.App as App
import           Core.App (App(..))
import qualified Core.AppSettings as AppSettings

getDateGetter :: IO () -> IO (IO ByteString)
getDateGetter flusher = do
    (getter, updater) <- WaiLogger.clockDateCacher
    _ <- Concurrent.forkIO $ forever $ do
        Concurrent.threadDelay 1000000
        updater
        flusher
    return getter

makeLogWare :: App -> IO Middleware
makeLogWare foundation
  | AppSettings.detailedRequestLogging $ App.settings foundation =
    RequestLogger.mkRequestLogger def
        { RequestLogger.outputFormat = Detailed True
        , RequestLogger.destination =
            Logger . YesodTypes.loggerSet $ App.logger foundation
        }
  | otherwise = do
      dateGetter <- getDateGetter flusher
      apache     <- WaiLogger.initLogger ipSrc callback dateGetter
      return $ apacheMiddleware apache
  where
    ipSrc
      | AppSettings.ipFromHeader $ App.settings foundation = FromFallback
      | otherwise                                          = FromSocket
    logger   = YesodTypes.loggerSet $ App.logger foundation
    flusher  = FastLogger.flushLogStr logger
    callback = FastLogger.LogCallback (FastLogger.pushLogStr logger) flusher

apacheMiddleware :: ApacheLoggerActions -> Middleware
apacheMiddleware ala app req sendResponse = app req $ \res -> do
    case Wai.responseStatus res of
        Status 200 _ -> return ()
        Status 304 _ -> return ()
        status       -> WaiLogger.apacheLogger ala req status . Header.contentLength $ Wai.responseHeaders res
    sendResponse res
