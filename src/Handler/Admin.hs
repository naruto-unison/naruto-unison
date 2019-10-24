{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Behind-the-scenes utility pages. Require sufficient 'Core.Field.Privilege'.
-- Privilege levels are handled in "Application.App".
module Handler.Admin
  ( getAdminR
  , postAdminR
  , getUsageR
  ) where

import ClassyPrelude
import Yesod

import qualified System.Random.MWC as Random
import           Text.Printf (printf)
import qualified Yesod.Auth as Auth

import           Application.App (Form, Handler, Route(..))
import qualified Application.App as App
import           Application.Model (News(..))
import           Application.Settings (widgetFile)
import qualified Application.Settings as Settings
import qualified Handler.Link as Link
import qualified Handler.Play as Play
import qualified Mission as Mission
import qualified Mission.UsageRate as UsageRate

-- | Behind-the-scenes utilities for admin accounts. Requires authorization.
getAdminR :: Handler Html
getAdminR = do
    app <- getYesod
    (newsForm, enctype) <- generateFormPost =<< getNewsForm
    liftIO Random.createSystemRandom >>= runReaderT Play.gameSocket
    defaultLayout do
        $(widgetFile "admin/admin")
        $(widgetFile "admin/sockets")

-- | 'getAdminR' for creating news posts.
postAdminR :: Handler Html
postAdminR = do
    app <- getYesod
    ((result, newsForm), enctype) <- runFormPost =<< getNewsForm
    case result of
        FormSuccess news -> do
            runDB $ insert400_ news
            defaultLayout [whamlet|<p>"News posted"|]
        _             -> defaultLayout [whamlet|<p>"Invalid post"|]
    liftIO Random.createSystemRandom >>= runReaderT Play.gameSocket
    defaultLayout do
        $(widgetFile "admin/admin")
        $(widgetFile "admin/sockets")

-- | Displays 'Usage' stats of characters.
getUsageR :: Handler Html
getUsageR = do
    usageRates <- sort <$> Mission.getUsageRates
    defaultLayout $(widgetFile "admin/usage")
  where
    showRate :: Float -> String
    showRate x
      |isNaN x    = "——"
      | otherwise = printf "%.2f%%" x

getNewsForm :: Handler (Form News)
getNewsForm = do
    author <- Auth.requireAuthId
    UTCTime date _ <- liftIO getCurrentTime
    return . renderDivs $ News author date
        <$> areq textField "" Nothing
        <*> (unTextarea <$> areq textareaField "" Nothing)
