{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

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
    let User { userAvatar
             , userClan
             , userJoined
             , userLosses
             , userName
             , userPosts
             , userRecord
             , userStreak
             , userTeam
             , userXp
             , userWins
             }      = user
        team        = getTeam userTeam
        (level, xp) = quotRem userXp 5000
    defaultLayout $(widgetFile "forum/profile")
  where
    getTeam (Just names) = Characters.lookupAll names
    getTeam Nothing      = []

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
    allBoards <- runDB $ mapM (indexBoard privilege) [minBound..maxBound]
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
    topics    <- runDB $ getTopics privilege
    App.lastModified . maximum
        $ epoch :| (forumTopicModified . citeVal <$> topics)
    defaultLayout $(widgetFile "forum/board")
  where
    getTopics privilege = selectWithAuthors
                          (filterTopics privilege [ForumTopicBoard ==. board])
                          [Desc ForumTopicTime]

-- | Renders a 'ForumTopic'.
getTopicR :: Key ForumTopic -> Handler Html
getTopicR topicId = do
    mwho       <- Auth.maybeAuthId
    privilege  <- App.getPrivilege
    topic      <- runDB $ get404 topicId
    (title, _) <- breadcrumbs
    time       <- liftIO getCurrentTime
    timestamp  <- liftIO Link.makeTimestamp
    posts      <- runDB do posts <- getPosts privilege
                           mapM (getLikes mwho) posts
    mwidget    <- forM (guard (forumTopicState topic == Open) >> mwho)
                $ generateFormPost . renderTable . Form.post topicId time
    let ForumTopic{forumTopicBoard, forumTopicState} = topic
    defaultLayout $(widgetFile "forum/topic")
  where
    topicKey = toPathPiece topicId
    getPosts privilege = selectWithAuthors
                         (filterPosts privilege [ForumPostTopic ==. topicId])
                         [Asc ForumPostTime]


-- | Adds to a 'ForumTopic'. Requires authentication.
postTopicR :: Key ForumTopic -> Handler Html
postTopicR topicId = do
    ForumTopic{forumTopicBoard, forumTopicState} <- runDB $ get404 topicId
    if forumTopicState /= Open then redirect $ TopicR topicId else do
        who        <- Auth.requireAuthId
        privilege  <- App.getPrivilege
        (title, _) <- breadcrumbs
        time       <- liftIO getCurrentTime
        timestamp  <- liftIO Link.makeTimestamp
        let form    = renderTable $ Form.post topicId time who
        ((result, widget), enctype) <- runFormPost form

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
                runDB do
                    post <- get404 postId
                    when (permit who privilege post)
                        $ update postId [ForumPostBody =. postBody]
                redirect $ TopicR topicId

            _ -> do
                posts <- runDB do posts <- getPosts privilege
                                  mapM (getLikes $ Just who) posts
                let mwho    = Just who
                    mwidget = Just (widget, enctype)
                defaultLayout $(widgetFile "forum/topic")
  where
    topicKey = toPathPiece topicId
    getPosts privilege = selectWithAuthors
                         (filterPosts privilege [ForumPostTopic ==. topicId])
                         [Asc ForumPostTime]
    permit who privilege ForumPost{forumPostAuthor, forumPostDeleted} =
        not forumPostDeleted && (forumPostAuthor == who || privilege > Normal)


-- | Renders a page for creating a new 'ForumTopic'. Requires authentication.
getNewTopicR :: ForumBoard -> Handler Html
getNewTopicR board = do
    (who, user)       <- Auth.requireAuthPair
    time              <- liftIO getCurrentTime
    (title, _)        <- breadcrumbs
    (widget, enctype) <- generateFormPost . renderTable
                       $ Form.topic user board time who
    defaultLayout $(widgetFile "forum/new")

-- | Creates a new 'ForumTopic'. Requires authentication.
postNewTopicR :: ForumBoard -> Handler Html
postNewTopicR board = do
    (who, user) <- Auth.requireAuthPair
    time        <- liftIO getCurrentTime
    (title, _)  <- breadcrumbs
    ((result, widget), enctype) <- runFormPost . renderTable
                                 $ Form.topic user board time who
    case result of
        FormSuccess (Form.NewTopic topic makePost) -> do
            topicId <- runDB do topicId <- insert400 topic
                                insert400_ $ makePost topicId
                                modifyTopic topicId
                                return topicId
            redirect $ TopicR topicId
        _ -> defaultLayout $(widgetFile "forum/new")

canDelete :: Key User -> Privilege -> ForumPost -> Bool
canDelete who privilege ForumPost{forumPostAuthor} = who == forumPostAuthor
                                                    || privilege > Normal

canLike :: Maybe (Key User) -> ForumPost -> Bool
canLike (Just who) ForumPost{forumPostAuthor} = who /= forumPostAuthor
canLike Nothing    _                          = False

data LikedPost = LikedPost
    { likedPost :: Cite ForumPost
    , likes     :: Int
    , liked     :: Bool
    }

markdowns :: [LikedPost] -> HashMap Text Markdown
markdowns posts = mapFromKeyed (toPathPiece . citeKey, forumPostBody . citeVal)
    $ likedPost <$> posts

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
selectWithAuthors selectors opts = mapM go =<< selectList selectors opts
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
getLikes mwho post = LikedPost post
    <$> count [ForumLikePost ==. likedPostId]
    <*> maybe (return False) justLike mwho
  where
    likedPostId = citeKey post
    justLike who = isJust <$> getLike likedPostId who

-- | Displays a user's rank, or their 'Privilege' level if higher than 'Normal'.
userRank :: User -> Text
userRank User{userXp, userPrivilege = Normal} = fromMaybe "Hokage"
    $ userRanks !? (userXp `quot` 5000)
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
userRank User{userPrivilege} = tshow userPrivilege
