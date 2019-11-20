{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | User forum handler.
module Handler.Forum
    ( getProfileR
    , getForumsR
    , getBoardR
    , getTopicR
    , postTopicR
    , getNewTopicR
    , postNewTopicR
    , selectWithAuthors
    , filterTopics
    ) where

import ClassyPrelude hiding (delete)
import Yesod

import qualified Yesod.Auth as Auth
import           Database.Persist.Sql (SqlPersistT)

import           Application.App (AppPersistEntity, Handler, Route(..))
import qualified Application.App as App
import           Application.Fields (ForumBoard, ForumCategory(..), Markdown(..), Privilege(..), TopicState(..), boardCategory, boardDesc, boardName)
import           Application.Model (Cite(..), EntityField(..), ForumPost(..), ForumTopic(..), HasAuthor(..), User(..))
import           Application.Settings (widgetFile)
import qualified Game.Characters as Characters
import           Handler.Forum.API (getLike, modifyTopic)
import qualified Handler.Forum.Form as Form
import qualified Handler.Link as Link
import           Util ((!?), epoch, mapFromKeyed)

-- | Renders a 'User' profile.
getProfileR :: Text -> Handler Html
getProfileR name = do
    muser          <- runDB $ selectFirst [UserName ==. name] []
    Entity _ user  <- maybe notFound return muser
    let User{..}    = user
        team        = maybe [] (mapMaybe Characters.lookup) userTeam
        (level, xp) = quotRem userXp 5000
    defaultLayout $(widgetFile "forum/profile")

data BoardIndex = BoardIndex ForumBoard Int (Maybe (Cite ForumTopic))
inCategory :: ForumCategory -> BoardIndex -> Bool
inCategory category (BoardIndex x _ _) = category == boardCategory x

-- | Renders the forums.
getForumsR :: Handler Html
getForumsR = do
    modified  <- runDB $ selectFirst [] [Desc ForumTopicModified]
    App.lastModified $ maybe epoch (forumTopicModified . entityVal) modified
    privilege <- App.getPrivilege
    citelink  <- liftIO Link.cite
    allBoards <- runDB $ traverse (indexBoard privilege) [minBound..maxBound]
    let boards category = filter (inCategory category) allBoards
    defaultLayout $(widgetFile "forum/browse")
  where
    categories = [minBound..maxBound]
    indexBoard privilege board = do
        size <- count [ForumTopicBoard ==. board, ForumTopicState !=. Deleted]
        post <- selectWithAuthors
                (filterTopics privilege [ForumTopicBoard ==. board])
                [Desc ForumTopicTime, LimitTo 1]
        return . BoardIndex board size $ headMay post

-- | Renders a 'ForumBoard'.
getBoardR :: ForumBoard -> Handler Html
getBoardR board = do
    privilege <- App.getPrivilege
    timestamp <- liftIO Link.makeTimestamp
    topics    <- runDB $ selectWithAuthors
                 (filterTopics privilege [ForumTopicBoard ==. board])
                 [Desc ForumTopicTime]
    App.lastModified . maximum $
        epoch :| (forumTopicModified . citeVal <$> topics)
    defaultLayout $(widgetFile "forum/board")

-- | Renders a 'ForumTopic'.
getTopicR :: Key ForumTopic -> Handler Html
getTopicR topicId = do
    mwho           <- Auth.maybeAuthId
    privilege      <- App.getPrivilege
    ForumTopic{..} <- runDB $ get404 topicId
    (title, _)     <- breadcrumbs
    time           <- liftIO getCurrentTime
    timestamp      <- liftIO Link.makeTimestamp
    posts          <- runDB $ traverse (getLikes mwho) =<<
                      selectWithAuthors
                      (filterPosts privilege [ForumPostTopic ==. topicId])
                      [Asc ForumPostTime]
    mwidget        <- forM (guard (forumTopicState == Open) >> mwho) $
                      generateFormPost . renderTable . Form.post topicId time
    defaultLayout $(widgetFile "forum/topic")
  where
    topicKey = toPathPiece topicId


-- | Adds to a 'ForumTopic'. Requires authentication.
postTopicR :: Key ForumTopic -> Handler Html
postTopicR topicId = do
    ForumTopic{..} <- runDB $ get404 topicId
    if forumTopicState /= Open then redirect $ TopicR topicId else do
        who        <- Auth.requireAuthId
        privilege  <- App.getPrivilege
        (title, _) <- breadcrumbs
        time       <- liftIO getCurrentTime
        timestamp  <- liftIO Link.makeTimestamp
        ((result, widget), enctype) <- runFormPost . renderTable $
                                       Form.post topicId time who

        case result of
            FormSuccess (Form.NewPost post) -> do
                runDB do
                    insert400_ post
                    update topicId [ ForumTopicPosts +=. 1
                                   , ForumTopicTime   =. time
                                   , ForumTopicLatest =. who
                                   ]
                    update who [ UserPosts +=. 1 ]
                    modifyTopic topicId
                redirect $ TopicR topicId

            FormSuccess (Form.EditPost postId postBody) -> do
                post        <- runDB $ get404 postId
                when ( forumTopicState == Open
                       && not (forumPostDeleted post)
                       && (forumPostAuthor post == who || privilege > Normal)
                     ) . runDB $ update postId [ForumPostBody =. postBody]
                redirect $ TopicR topicId

            _ -> do
                posts <- runDB $ traverse (getLikes $ Just who) =<<
                         selectWithAuthors
                         (filterPosts privilege [ForumPostTopic ==. topicId])
                         [Asc ForumPostTime]
                let mwho    = Just who
                    mwidget = Just (widget, enctype)
                defaultLayout $(widgetFile "forum/topic")
  where
    topicKey = toPathPiece topicId

-- | Renders a page for creating a new 'ForumTopic'. Requires authentication.
getNewTopicR :: ForumBoard -> Handler Html
getNewTopicR board = do
    (who, user)       <- Auth.requireAuthPair
    time              <- liftIO getCurrentTime
    (title, _)        <- breadcrumbs
    (widget, enctype) <- generateFormPost . renderTable $
                         Form.topic user board time who
    defaultLayout $(widgetFile "forum/new")

-- | Creates a new 'ForumTopic'. Requires authentication.
postNewTopicR :: ForumBoard -> Handler Html
postNewTopicR board = do
    (who, user) <- Auth.requireAuthPair
    time        <- liftIO getCurrentTime
    (title, _)  <- breadcrumbs
    ((result, widget), enctype) <- runFormPost . renderTable $
                                   Form.topic user board time who
    case result of
        FormSuccess (Form.NewTopic topic makePost) -> do
            topicId <- runDB $ insert400 topic
            let post = makePost topicId
            runDB do
                insert400_ post
                modifyTopic topicId
            redirect $ TopicR topicId
        _ -> defaultLayout $(widgetFile "forum/new")

canDelete :: Key User -> Privilege -> ForumPost -> Bool
canDelete who privilege post = forumPostAuthor post == who || privilege > Normal

canLike :: Maybe (Key User) -> ForumPost -> Bool
canLike Nothing _ = False
canLike (Just who) post = who /= forumPostAuthor post

data LikedPost = LikedPost { likedPost :: Cite ForumPost
                           , likes     :: Int
                           , liked     :: Bool
                           }

markdowns :: [LikedPost] -> HashMap Text Markdown
markdowns posts = mapFromKeyed ( toPathPiece . citeKey
                               , forumPostBody . citeVal
                               ) $ likedPost <$> posts

filterPosts :: Privilege -> [Filter ForumPost] -> [Filter ForumPost]
filterPosts p xs
  | p > Normal = xs
  | otherwise  = (ForumPostDeleted ==. False) : xs

filterTopics :: Privilege -> [Filter ForumTopic] -> [Filter ForumTopic]
filterTopics p xs
  | p > Normal = xs
  | otherwise  = (ForumTopicState !=. Deleted) : xs

-- | Fills out author information from the database.
selectWithAuthors :: ∀ m a. (MonadIO m, HasAuthor a, AppPersistEntity a)
                  => [Filter a] -> [SelectOpt a] -> SqlPersistT m [Cite a]
selectWithAuthors selectors opts = traverse go =<< selectList selectors opts
  where
    go (Entity citeKey citeVal) = do
        citeAuthor <- get404 author
        citeLatest <- if author == latest then
                          return citeAuthor
                      else
                          get404 latest
        return Cite {citeKey, citeAuthor, citeLatest, citeVal}
      where
        author = getAuthor citeVal
        latest = getLatest citeVal

getLikes :: ∀ m. MonadIO m
         => Maybe (Key User) -> Cite ForumPost -> SqlPersistT m LikedPost
getLikes mwho likedPost =
    LikedPost likedPost
    <$> count [ForumLikePost ==. likedPostId]
    <*> maybe (return False) ((isJust <$>) . getLike likedPostId) mwho
  where
    likedPostId = citeKey likedPost

-- | Displays a user's rank, or their 'Privilege' level if higher than 'Normal'.
userRank :: User -> Text
userRank user = case userPrivilege user of
    Normal -> fromMaybe "Hokage" $ userRanks !? (userXp user `quot` 5000)
    _      -> tshow $ userPrivilege user
  where
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
