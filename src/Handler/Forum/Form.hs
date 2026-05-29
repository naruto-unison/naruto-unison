{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | User forum handler.
module Handler.Forum.Form
    ( topic, NewTopic(..)
    , post, PostForm(..)
    ) where

import ClassyPrelude hiding (delete)
import Yesod.Form

import Database.Persist (Key)

import qualified Application.App as App
import           Application.Fields (ForumBoard, Markdown(..), Privilege(..), TopicState(..))
import           Application.Model (ForumPost(..), ForumTopic(..), User(..))
import qualified Handler.Link as Link

data NewTopic = NewTopic ForumTopic (Key ForumTopic -> ForumPost)

toBody :: Textarea -> Markdown
toBody (Textarea area) = Markdown area

topic :: User -> ForumBoard -> UTCTime -> Key User -> App.AForm NewTopic
topic User{userPrivilege} forumTopicBoard forumPostTime forumPostAuthor =
    makeNewTopic <$> areq textField "Title" Nothing
                 <*> areq textareaField "Post" Nothing
  where
    makeTopic rawTitle = ForumTopic
        { forumTopicAuthor = forumPostAuthor
        , forumTopicBoard
        , forumTopicLatest = forumPostAuthor
        , forumTopicModified = forumPostTime
        , forumTopicPosts = 1
        , forumTopicStaff = userPrivilege > Normal
        , forumTopicState = Open
        , forumTopicTime = forumPostTime
        , forumTopicTitle = filter (/= Link.staffTag) rawTitle
        }
    makePost area forumPostTopic = ForumPost
        { forumPostAuthor
        , forumPostBody = toBody area
        , forumPostLikes = 0
        , forumPostDeleted = False
        , forumPostEdited = Nothing
        , forumPostTime
        , forumPostTopic
        }
    makeNewTopic rawTitle area = NewTopic (makeTopic rawTitle) $ makePost area

data PostForm
    = NewPost ForumPost
    | EditPost (Key ForumPost) Markdown

post :: Key ForumTopic -> UTCTime -> Key User -> App.AForm PostForm
post forumPostTopic forumPostTime forumPostAuthor = makePost
    <$> aopt hiddenField "" Nothing
    <*> areq textareaField "" Nothing
  where
    makePost mPostId area = case mPostId of
        Just postId -> EditPost postId $ toBody area
        Nothing     -> NewPost ForumPost
            { forumPostAuthor
            , forumPostTopic
            , forumPostTime
            , forumPostLikes = 0
            , forumPostDeleted = False
            , forumPostEdited = Nothing
            , forumPostBody = toBody area
            }
