{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Interface for the PureScript game client.
module Handler.Client
    ( getPlayR
    , getMuteR
    , getUpdateR
    ) where

import StandardLibrary

import qualified Data.List.NonEmpty as NonEmpty

import Yesod.WebSockets (webSockets)

import Calculus
import Core.Import
import Game.Structure
import Game.Characters
import Handler.Play (gameSocket)

charAvatars :: Character -> [Text]
charAvatars char = toFile <$> "icon" : skills
  where 
    skills      = label . head <$> NonEmpty.take 4 (characterSkills char)
    toFile path = "/img/ninja/" ++ shorten (characterName char) ++ "/" 
                  ++ shorten path ++ ".jpg"

avatars :: [Text]
avatars = ("/img/icon/" ++)
        <$> [ "default.jpg"
          , "gaaraofthefunk.jpg"
          , "ninjainfocards.jpg"
          , "kabugrin.jpg"
          ]
       ++ concatMap charAvatars cs'

isMuted :: Maybe User -> Bool
isMuted = maybe False userMuted

legalChars :: String
legalChars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'z']

-- * HANDLERS

-- | Updates a user's profile.
getUpdateR :: Text -> Bool -> Text -> Text -> Handler Value
getUpdateR updateName updateCondense updateBackground updateAvatar
  | "/img/icon/" /= take 10 updateAvatar  = invalidArgs ["Invalid avatar"]
  | any (`notElem` legalChars) updateName = invalidArgs ["Invalid name"]
  | otherwise = do
    (accId, _) <- requireAuthPair
    user <- runDB $ updateGet accId [ UserName       =. updateName
                                   , UserCondense   =. updateCondense
                                   , UserBackground =. updateBackground''
                                   , UserAvatar     =. updateAvatar
                                   ]     
    returnJson user
  where 
    updateBackground'  = tTail updateBackground
    updateBackground'' 
      | null updateBackground' = Nothing
      | otherwise              = Just updateBackground'

-- | Updates a user's muted status.
getMuteR :: Bool -> Handler Value
getMuteR mute = do
    (who, _) <- requireAuthPair
    runDB $ update who [ UserMuted =. mute ]
    returnJson mute

-- | Renders the gameplay client.
getPlayR :: Handler Html
getPlayR = do
    webSockets gameSocket
    ma <- maybeAuth
    let (_, muser) = case ma of
          Just (Entity who user) -> (Just who, Just user)
          Nothing                -> (Nothing, Nothing)
    let team = maybe [] (mapMaybe (`lookup` cs)) $ muser >>= userTeam
    let bg   = fromMaybe "/img/bg/valley2.jpg" $ muser >>= userBackground
    defaultLayout $ do
        setTitle "Naruto Unison"
        addStylesheetRemote "/css/embeds.css"
        $(widgetFile "include/progressbar.min")
        $(widgetFile "include/soundjs.min")
        $(widgetFile "include/normalize")
        $(widgetFile "play/play")
        $(widgetFile "play/ps")
