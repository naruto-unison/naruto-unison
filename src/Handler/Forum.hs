{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Interface for the PureScript game client.
module Handler.Forum
    ( getProfileR
    , getForumsR
    , getBoardR
    , getTopicR
    ) where

import Preludesque

import Data.Text       (Text)
import Text.Blaze.Html

import Calculus
import Core.Import
import Game.Characters (cs)

topicDesc ∷ Topic → Text
topicDesc Topic{topicPosts = Post{postBody = x:_}:_} = x
topicDesc _ = ""

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

getProfileR ∷ Text → Handler Html
getProfileR name = do
    muser                  ← runDB $ selectFirst [UserName ==. name] []
    Entity _ user@User{..} ← tryJust notFound muser
    let (level, xp)        = quotRem userXp 5000
    defaultLayout $ do
        setTitle $ toHtml userName
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "forum/profile")

-- | Renders the forums.
getForumsR ∷ Handler Html
getForumsR = defaultLayout $ do
    setTitle "Naruto Unison Forums"
    $(widgetFile "forum/browse")

-- | Renders a 'ForumBoard'.
getBoardR ∷ ForumBoard → Handler Html
getBoardR board = do
    topics ← runDB $ selectList [TopicBoard ==. board] []
    defaultLayout $ do
      setTitle ∘ toHtml $ boardName board
      $(widgetFile "forum/board")

getTopicR ∷ TopicId → Handler Html
getTopicR topicId = do
    Topic{..} ← runDB $ get404 topicId
    defaultLayout $ do
      setTitle $ toHtml topicTitle
      $(widgetFile "forum/topic")
