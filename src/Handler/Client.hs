{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Interface for the Elm game client.
module Handler.Client
    ( getPlayR
    , getMuteR
    , getUpdateR
    ) where

import ClassyPrelude hiding (Handler, head)
import Yesod

import           Data.List.NonEmpty (head)
import qualified Yesod.Auth as Auth

import qualified Class.Sockets as Sockets
import           Core.App (Handler)
import           Core.Model (EntityField(..), User(..))
import           Core.Settings (widgetFile)
import           Core.Util ((∉), shorten)
import qualified Model.Character as Character
import           Model.Character (Character)
import qualified Model.Skill as Skill
import           Handler.Play (gameSocket)
import qualified Characters

-- | Updates a user's profile.
getUpdateR :: Text -> Bool -> Text -> Text -> Handler Value
getUpdateR updateName updateCondense updateBackground updateAvatar
  | not $ "/img/icon/" `isPrefixOf` updateAvatar =
      invalidArgs ["Invalid avatar"]
  | any (∉ legalChars) updateName =
      invalidArgs ["Name can only contain letters and numbers"]
  | otherwise = do
    (accId, _) <- Auth.requireAuthPair
    user <- runDB $ updateGet accId [ UserName      =. updateName
                                   , UserCondense   =. updateCondense
                                   , UserBackground =. updateBackground''
                                   , UserAvatar     =. updateAvatar
                                   ]
    returnJson user
  where
    updateBackground'  = fromMaybe "" $ tailMay updateBackground
    updateBackground''
      | null updateBackground' = Nothing
      | otherwise              = Just updateBackground'

-- | Updates a user's muted status.
getMuteR :: Bool -> Handler Value
getMuteR mute = do
    (who, _) <- Auth.requireAuthPair
    runDB $ update who [ UserMuted =. mute ]
    returnJson mute

-- | Renders the gameplay client.
getPlayR :: Handler Html
getPlayR = do
    Sockets.run gameSocket
    muser <- (entityVal <$>) <$> Auth.maybeAuth
    let team     = maybe [] (mapMaybe (`lookup` Characters.map)) $
                   muser >>= userTeam
        practice = maybe [] (mapMaybe (`lookup` Characters.map) . userPractice)
                   muser
        bg       = fromMaybe "/img/bg/valley2.jpg" $ muser >>= userBackground
        vol :: Text
        vol
          | isMuted muser = "click muted"
          | otherwise     = "click"
    setCsrfCookie
    defaultLayout do
        setTitle "Naruto Unison"
        addStylesheetRemote "/css/embeds.css"
        $(widgetFile "include/progressbar.min")
        $(widgetFile "include/soundjs.min")
        $(widgetFile "include/normalize")
        $(widgetFile "play/elm")
        $(widgetFile "play/play")

charAvatars :: Character -> [Text]
charAvatars char = toFile <$> "icon" : skills
  where
    skills      = Skill.name . head <$> toList (Character.skills char)
    toFile path = "/img/ninja/" ++ shorten (Character.format char) ++ "/"
                  ++ shorten path ++ ".jpg"

avatars :: [Text]
avatars = (("/img/icon/" ++) <$> icons) ++ concatMap charAvatars Characters.list
  where
    icons =
        [ "default.jpg"
        , "gaaraofthefunk.jpg"
        , "ninjainfocards.jpg"
        , "kabugrin.jpg"
        ]

isMuted :: Maybe User -> Bool
isMuted = maybe False userMuted

legalChars :: String
legalChars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'z']
