{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | User forum handler.
module Handler.Forum.API
    ( getDeletePostR
    , getLikePostR
    , getLike
    , getDeleteTopicR
    , getLockTopicR
    , getUnlockTopicR
    , modifyTopic
    ) where

import ClassyPrelude hiding (delete)
import Yesod

import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Yesod.Auth as Auth
import           Database.Persist.Sql (SqlPersistT)

import           Application.App (Handler)
import qualified Application.App as App
import           Application.Fields (Privilege(..), TopicState(..))
import           Application.Model (EntityField(..), ForumLike(..), ForumPost(..), ForumTopic(..), User(..))

modifyTopic :: ∀ m. MonadIO m => Key ForumTopic -> SqlPersistT m ()
modifyTopic topicId = do
    currentTime <- liftIO getCurrentTime
    updateWhere [ForumTopicModified <. currentTime, ForumTopicId ==. topicId]
                [ForumTopicModified =. currentTime]

attemptMaybeT :: ∀ m. Monad m => MaybeT m () -> m Value
attemptMaybeT m = returnJson . isJust =<< runMaybeT m

-- | Marks a forum post as deleted.
-- Deleted posts are visible only to Moderators and Admins.
-- Returns @True@ if successful, otherwise @False@.
getDeletePostR :: Key ForumPost -> Handler Value
getDeletePostR postId = attemptMaybeT do
    (who, user) <- MaybeT Auth.maybeAuthPair
    post        <- lift . runDB $ get404 postId
    guard $ forumPostAuthor post == who || userPrivilege user > Normal
    lift $ runDB do
        update postId [ForumPostDeleted =. True]
        update (forumPostAuthor post) [UserPosts -=. 1]
        modifyTopic $ forumPostTopic post

-- | Likes or unlikes a 'ForumPost', altering its 'forumPostLikes'.
-- Returns @True@ if successful, otherwise @False@.
getLikePostR :: Key ForumPost -> Handler Value
getLikePostR forumLikePost = attemptMaybeT do
    who   <- MaybeT Auth.maybeAuthId
    post  <- lift . runDB $ get404 forumLikePost
    guard $ forumPostAuthor post /= who
    liked <- lift . runDB $ getLike forumLikePost who
    case liked of
        Just (Entity likeId _) ->
            lift . runDB $ delete likeId
        Nothing -> do
            lift . runDB . void $ insert ForumLike{ forumLikePost
                                                  , forumLikeUser = who
                                                  }
            empty

getLike :: ∀ m. MonadIO m
        => Key ForumPost -> Key User -> SqlPersistT m (Maybe (Entity ForumLike))
getLike post who =
    selectFirst [ForumLikePost ==. post, ForumLikeUser ==. who] []

-- | Marks a forum topic as 'Deleted'.
-- Deleted topics are visible only to Moderators and Admins.
-- Returns @True@ if successful, otherwise @False@.
getDeleteTopicR :: Key ForumTopic -> Handler Value
getDeleteTopicR topicId = attemptMaybeT do
    privilege <- App.getPrivilege
    guard $ privilege > Normal
    lift $ runDB do
        update topicId [ForumTopicState =. Deleted]
        modifyTopic topicId

-- | Marks a forum topic as 'Locked'.
-- New posts cannot be added to locked topics
-- Returns @True@ if successful, otherwise @False@..
getLockTopicR :: Key ForumTopic -> Handler Value
getLockTopicR = setTopicState Locked

-- | Unmarks a forum topic as 'Locked'. Topic must not be 'Deleted'.
-- Returns @True@ if successful, otherwise @False@.
getUnlockTopicR :: Key ForumTopic -> Handler Value
getUnlockTopicR = setTopicState Open

setTopicState :: TopicState -> Key ForumTopic -> Handler Value
setTopicState state topicId = attemptMaybeT do
    privilege <- App.getPrivilege
    guard $ privilege > Normal
    topic <- MaybeT . runDB $ get topicId
    guard $ forumTopicState topic /= Deleted && forumTopicState topic /= state
    lift $ runDB do
        update topicId [ForumTopicState =. state]
        modifyTopic topicId
    return ()
