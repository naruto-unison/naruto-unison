{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE QuasiQuotes                 #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- | Behind-the-scenes utility pages. Require sufficient 'Core.Field.Privilege'.
-- Privilege levels are handled in "Core.App".
module Handler.Admin (getAdminR, postAdminR) where

import ClassyPrelude
import Yesod

import qualified Yesod.Auth as Auth

import           Core.App (Form, Handler, Route(..))
import           Core.Model (News(..))
import           Core.Settings (widgetFile)
import qualified Class.Sockets as Sockets
import qualified Handler.Play as Play

getAdminR :: Handler Html
getAdminR = do
    (newsForm, enctype) <- generateFormPost =<< getNewsForm
    Sockets.run Play.gameSocket
    defaultLayout do
        setTitle "Admin"
        $(widgetFile "admin/admin")
        $(widgetFile "admin/sockets")

postAdminR :: Handler Html
postAdminR = do
    ((result, newsForm), enctype) <- runFormPost =<< getNewsForm
    case result of
        FormSuccess news -> do
            runDB $ insert400_ news
            defaultLayout [whamlet|<p>"News posted"|]
        _             -> defaultLayout [whamlet|<p>"Invalid post"|]
    Sockets.run Play.gameSocket
    defaultLayout do
        setTitle "Admin"
        $(widgetFile "admin/admin")
        $(widgetFile "admin/sockets")

getNewsForm :: Handler (Form News)
getNewsForm = do
    author <- Auth.requireAuthId
    (UTCTime date _) <- liftIO getCurrentTime
    return . renderDivs $ News author date
        <$> areq textField "" Nothing
        <*> (unTextarea <$> areq textareaField "" Nothing)
