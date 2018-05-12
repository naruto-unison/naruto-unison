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

import Preludesque

import qualified Data.Text           as T
import qualified Data.HashMap.Strict as M

import Data.Text  (Text)
import Text.Hamlet
import Yesod.WebSockets

import Calculus
import Core.Import
import Game.Structure
import Game.Characters
import Handler.Play (gameSocket)

charAvatars ∷ Character → [Text]
charAvatars char = (root ⧺ "icon.jpg")
                 : (((root ⧺) ∘ (⧺ ".jpg")) ↤ shorten ∘ label ∘ head)
                    ↤ take' 4 (characterSkills char)
  where root = "/img/ninja/" ⧺ shorten (characterName char) ⧺ "/"

avatars ∷ [Text]
avatars = ("/img/icon/" ⧺)
        ↤ [ "default.jpg"
          , "gaaraofthefunk.jpg"
          , "ninjainfocards.jpg"
          , "kabugrin.jpg"
          ]
       ⧺ concatMap charAvatars cs'

isMuted ∷ Maybe User → Bool
isMuted = maybe False userMuted

legalChars ∷ String
legalChars = ['0'..'9'] ⧺ ['a'..'z'] ⧺ ['A'..'z']

-- * HANDLERS

-- | Updates a user's profile.
getUpdateR ∷ Text → Text → Text → Handler Value
getUpdateR updateName updateBackground updateAvatar
  | "/img/icon/" ≠ T.take 10 updateAvatar = invalidArgs ["Invalid avatar"]
  | T.any (∉ legalChars) updateName = invalidArgs ["Invalid name"]
  | otherwise = do
    (accId, _) ← requireAuthPair
    user ← runDB $ updateGet accId [ UserName       =. updateName
                                   , UserBackground =. updateBackground''
                                   , UserAvatar     =. updateAvatar
                                   ]     
    returnJson user
  where updateBackground'  = tTail updateBackground
        updateBackground'' | T.null updateBackground' = Nothing
                           | otherwise                = Just updateBackground'

-- | Updates a user's muted status.
getMuteR ∷ Bool → Handler Value
getMuteR mute = do
    (who, _) ← requireAuthPair
    runDB $ update who [ UserMuted =. mute ]
    returnJson mute

-- | Renders the gameplay client.
getPlayR ∷ Handler Html
getPlayR = do
    webSockets gameSocket
    ma ← maybeAuth
    let (_, muser) = case ma of
          Just (Entity who user) → (Just who, Just user)
          Nothing                → (Nothing, Nothing)
    let team          = maybe [] (mapMaybe (`M.lookup` cs)) $ muser ≫= userTeam
    let bg = fromMaybe "/img/bg/valley2.jpg" $ muser ≫= userBackground
    defaultLayout $ do
        setTitle "Naruto Unison"
        addStylesheetRemote "/css/embeds.css"
        $(widgetFile "include/progressbar.min")
        $(widgetFile "include/soundjs.min")
        $(widgetFile "include/normalize")
        $(widgetFile "play/play")
        $(widgetFile "play/ps")
