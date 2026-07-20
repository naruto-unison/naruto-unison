{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Behind-the-scenes utility pages. Require sufficient 'Core.Field.Privilege'.
-- Privilege levels are handled in "Application.App".
module Handler.Admin
  ( getAdminR
  , getUsageR
  ) where

import ClassyPrelude
import Yesod

import System.Random.MWC (createSystemRandom)
import Text.Printf (printf)

import           Application.App (Route(..))
import qualified Application.App as App
import           Application.Settings (widgetFile)
import qualified Application.Settings as Settings
import qualified Handler.Site.Link as Link
import qualified Handler.Play as Play
import qualified Mission
import           Mission.UsageRate (UsageRate(UsageRate))
import qualified Mission.UsageRate as UsageRate

-- | Behind-the-scenes utilities for admin accounts. Requires authorization.
getAdminR :: App.Handler Html
getAdminR = do
    App.unchanged304
    port <- getsYesod \app -> app.settings.port
    liftIO createSystemRandom >>= runReaderT Play.gameSocket
    defaultLayout do
        $(widgetFile "admin/admin")
        $(widgetFile "admin/sockets")

-- | Displays 'Usage' stats of characters.
getUsageR :: App.Handler Html
getUsageR = do
    usageRates <- sortBy compareRates <$> Mission.getUsageRates
    defaultLayout $(widgetFile "admin/usage")
  where
    compareRates :: UsageRate -> UsageRate -> Ordering
    compareRates x y = comparing nanToNegInf y x
      where
        nanToNegInf rate
            | isNaN rate.winRate = rate { UsageRate.winRate = -1/0 }
            | otherwise          = rate

    showRate :: Float -> String
    showRate x
      |isNaN x    = "——"
      | otherwise = printf "%.2f%%" x
