{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Behind-the-scenes utility pages. Require sufficient 'Core.Field.Privilege'.
-- Privilege levels are handled in "Application.App".
module Handler.Admin
  ( getAdminR
  , postAdminR
  , getUsageR
  ) where

import ClassyPrelude
import Yesod

import           System.Random.MWC (createSystemRandom)
import           Text.Printf (printf)
import qualified Yesod.Auth as Auth

import           Application.App (Route(..))
import qualified Application.App as App
import           Application.Model (News(..))
import           Application.Settings (widgetFile)
import qualified Application.Settings as Settings
import qualified Handler.Link as Link
import qualified Handler.Play as Play
import qualified Mission
import           Mission.UsageRate (UsageRate)
import qualified Mission.UsageRate as UsageRate

-- | Behind-the-scenes utilities for admin accounts. Requires authorization.
getAdminR :: App.Handler Html
getAdminR = do
    App.unchanged304
    app <- getYesod
    (newsForm, enctype) <- generateFormPost =<< getNewsForm
    liftIO createSystemRandom >>= runReaderT Play.gameSocket
    defaultLayout do
        $(widgetFile "admin/admin")
        $(widgetFile "admin/sockets")

-- | 'getAdminR' for creating news posts.
postAdminR :: App.Handler Html
postAdminR = do
    app <- getYesod
    ((result, newsForm), enctype) <- runFormPost =<< getNewsForm
    case result of
        FormSuccess news -> do
            runDB $ insert400_ news
            defaultLayout [whamlet|<p>"News posted"|]
        _ -> defaultLayout [whamlet|<p>"Invalid post"|]
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
            | isNaN $ UsageRate.winRate rate = rate { UsageRate.winRate = -1/0 }
            | otherwise                      = rate

    showRate :: Float -> String
    showRate x
      |isNaN x    = "——"
      | otherwise = printf "%.2f%%" x

getNewsForm :: App.Handler (Html -> App.MForm News)
getNewsForm = return . renderDivs $ News
    <$> lift Auth.requireAuthId
    <*> lift (liftIO getCurrentTime)
    <*> areq textField "" Nothing
    <*> (unTextarea <$> areq textareaField "" Nothing)
