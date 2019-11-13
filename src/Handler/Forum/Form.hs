{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | User forum handler.
module Handler.Forum.Form
    ( topic, NewTopic(..)
    , post, PostForm(..)
    ) where

import ClassyPrelude hiding (delete)
import Yesod

import           Application.App (Handler)
import           Application.Fields (ForumBoard, Markdown(..), Privilege(..), TopicState(..))
import           Application.Model (ForumPost(..), ForumTopic(..), User(..))
import qualified Handler.Link as Link

data NewTopic = NewTopic ForumTopic (Key ForumTopic -> ForumPost)

toBody :: Textarea -> Markdown
toBody (Textarea area) = Markdown area

topic :: User -> ForumBoard -> UTCTime -> Key User
             -> AForm Handler NewTopic
topic User{..} forumTopicBoard forumPostTime forumPostAuthor =
    makeNewTopic
    <$> areq textField "Title" Nothing
    <*> areq textareaField "Post" Nothing
  where
    forumTopicAuthor   = forumPostAuthor
    forumTopicLatest   = forumPostAuthor
    forumTopicTime     = forumPostTime
    forumTopicStaff    = userPrivilege > Normal
    forumTopicState    = Open
    forumTopicPosts    = 1
    forumTopicModified = forumPostTime
    forumPostLikes     = 0
    forumPostDeleted   = False
    forumPostEdited    = Nothing
    makeNewTopic rawTitle area = NewTopic ForumTopic{..}
                                 \forumPostTopic -> ForumPost{..}
      where
        forumTopicTitle = filter (/= Link.staffTag) rawTitle
        forumPostBody = toBody area

data PostForm
    = NewPost ForumPost
    | EditPost (Key ForumPost) Markdown

post :: Key ForumTopic -> UTCTime -> Key User -> AForm Handler PostForm
post forumPostTopic forumPostTime forumPostAuthor =
    makePost
    <$> aopt hiddenField "" Nothing
    <*> areq textareaField "" Nothing
  where
    forumPostLikes   = 0
    forumPostDeleted = False
    forumPostEdited  = Nothing
    makePost mPostId (toBody -> forumPostBody) = case mPostId of
        Nothing     -> NewPost ForumPost{..}
        Just postId -> EditPost postId forumPostBody
