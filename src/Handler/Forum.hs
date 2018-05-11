{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Interface for the PureScript game client.
module Handler.Forum
    ( getForumsR
    , getBoardR
    , getProfileR
    ) where

import Preludesque

import Data.Text       (Text)
import Text.Blaze.Html

import Calculus
import Core.Import
import Game.Characters (cs)

userRanks ∷ [Text]
userRanks = [ "Academy Student"
            , "Genin"
            , "Chūnin"
            , "Missing-Nin"
            , "Anbu"
            , "Jōnin"
            , "Sannin"
            , "Jinchūriki"
            , "Akatsuki"
            , "Kage"
            , "Hokage"
            ]

userRank ∷ User → Text
userRank User{..}
  | userPrivilege ≠ Normal = tshow userPrivilege
  | otherwise              = maybe "Hokage" fst ∘ uncons $ drop level userRanks
  where level = userXp ÷ 5000

-- | Renders the forums.
getForumsR ∷ Handler Html
getForumsR = defaultLayout $ do
    setTitle "Naruto Unison Forums"
    $(widgetFile "forum/browse")

-- | Renders a 'ForumBoard'.
getBoardR ∷ ForumBoard → Handler Html
getBoardR board = defaultLayout $ do
    setTitle ∘ toHtml $ boardName board
    $(widgetFile "forum/board")

getProfileR ∷ Key User → Handler Html
getProfileR who = do
    user@User{..} ← runDB $ get404 who
    let (level, xp) = quotRem userXp 5000
    defaultLayout $ do
        setTitle ∘ toHtml $ "User: " ⧺ userName
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "forum/profile")

