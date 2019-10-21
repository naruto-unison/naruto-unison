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
    ) where

import ClassyPrelude
import Yesod

import qualified Data.Text as Text
import qualified Yesod.Auth as Auth

import           Application.App (AppPersistEntity, Handler, Route(..))
import           Application.Fields (ForumBoard, ForumCategory(..), Privilege(..), boardCategory, boardDesc, boardName)
import           Application.Model (Cite(..), EntityField(..), ForumPost(..), ForumTopic(..), ForumTopicId, HasAuthor(..), User(..), UserId)
import           Application.Settings (widgetFile)
import qualified Game.Characters as Characters
import qualified Game.Model.Class as Class
import qualified Handler.Link as Link
import           Util ((!?))

-- | Renders a 'User' profile.
getProfileR :: Text -> Handler Html
getProfileR name = do
    muser                  <- runDB $ selectFirst [UserName ==. name] []
    Entity _ user@User{..} <- maybe notFound return muser
    let (level, xp)         = quotRem userXp 5000
    defaultLayout do
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "forum/profile")

data BoardIndex = BoardIndex ForumBoard Int (Maybe (Cite ForumTopic))
inCategory :: ForumCategory -> BoardIndex -> Bool
inCategory category (BoardIndex x _ _) = category == boardCategory x

-- | Renders the forums.
getForumsR :: Handler Html
getForumsR = do
    citelink <- liftIO Link.cite
    allBoards <- traverse indexBoard [minBound..maxBound]
    let boards category = filter (inCategory category) allBoards
    defaultLayout $(widgetFile "forum/browse")
  where
    categories = [minBound..maxBound]
    indexBoard board = do
        posts <- selectWithAuthors [ForumTopicBoard ==. board]
                                   [Desc ForumTopicTime]
        pure $ BoardIndex board (length posts) (headMay posts)

-- | Renders a 'ForumBoard'.
getBoardR :: ForumBoard -> Handler Html
getBoardR board = do
    timestamp  <- liftIO Link.makeTimestamp
    topics     <- selectWithAuthors [ForumTopicBoard ==. board] []
    defaultLayout $(widgetFile "forum/board")

-- | Renders a 'ForumTopic'.
getTopicR :: ForumTopicId -> Handler Html
getTopicR topicId = do
    mwho           <- Auth.maybeAuthId
    (title, _)     <- breadcrumbs
    time           <- liftIO getCurrentTime
    timestamp      <- liftIO Link.makeTimestamp
    ForumTopic{..} <- runDB $ get404 topicId
    posts          <- selectWithAuthors [ForumPostTopic ==. topicId] []
    mwidget        <- forM mwho $
                      generateFormPost . renderTable . newPostForm topicId time
    defaultLayout $(widgetFile "forum/topic")

-- | Adds to a 'ForumTopic'. Requires authentication.
postTopicR :: ForumTopicId -> Handler Html
postTopicR topicId = do
    who        <- Auth.requireAuthId
    (title, _) <- breadcrumbs
    time       <- liftIO getCurrentTime
    timestamp  <- liftIO Link.makeTimestamp
    ((result, widget), enctype) <- runFormPost . renderTable $
                                   newPostForm topicId time who
    case result of
        FormSuccess post -> runDB do
            insert400_ post
            update topicId [ ForumTopicPosts +=. 1
                           , ForumTopicTime   =. time
                           , ForumTopicLatest =. who
                           ]
        _ -> return ()
    ForumTopic{..} <- runDB $ get404 topicId
    posts          <- selectWithAuthors [ForumPostTopic ==. topicId] []
    let mwidget     = Just (widget, enctype)
    defaultLayout $(widgetFile "forum/topic")

-- | Renders a page for creating a new 'ForumTopic'. Requires authentication.
getNewTopicR :: ForumBoard -> Handler Html
getNewTopicR board = do
    (who, user) <- Auth.requireAuthPair
    time        <- liftIO getCurrentTime
    (title, _)  <- breadcrumbs
    (widget, enctype) <- generateFormPost . renderTable $
                         newTopicForm user board time who
    defaultLayout $(widgetFile "forum/new")

-- | Creates a new 'ForumTopic'. Requires authentication.
postNewTopicR :: ForumBoard -> Handler Html
postNewTopicR board = do
    (who, user) <- Auth.requireAuthPair
    time        <- liftIO getCurrentTime
    (title, _)  <- breadcrumbs
    ((result, widget), enctype) <- runFormPost . renderTable $
                                   newTopicForm user board time who
    case result of
        FormSuccess (NewTopic topic makePost) -> do
            topicId <- runDB $ insert400 topic
            let post = makePost topicId
            runDB $ insert400_ post
            redirect $ TopicR topicId
        _ -> defaultLayout $(widgetFile "forum/new")

userRanks :: [Text]
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

-- | Fills out author information from the database.
selectWithAuthors :: ∀ a. (HasAuthor a, AppPersistEntity a)
                  => [Filter a] -> [SelectOpt a] -> Handler [Cite a]
selectWithAuthors selectors opts = runDB do
    selected <- selectList selectors opts
    traverse go selected
  where
    go (Entity citeKey citeVal) = do
        citeAuthor <- get404 author
        citeLatest <- if | author == latest -> return citeAuthor
                         | otherwise        -> get404 latest
        return Cite {citeKey, citeAuthor, citeLatest, citeVal}
      where
        author = getAuthor citeVal
        latest = getLatest citeVal

-- | Displays a user's rank, or their 'Privilege' level if higher than 'Normal'.
userRank :: User -> Text
userRank user = case userPrivilege user of
    Normal -> fromMaybe "Hokage" $ userRanks !? (userXp user `quot` 5000)
    _      -> tshow $ userPrivilege user

data NewTopic = NewTopic ForumTopic (ForumTopicId -> ForumPost)

toBody :: Textarea -> [Text]
toBody (Textarea area) = Text.splitOn "\n" area

newTopicForm :: User -> ForumBoard -> UTCTime -> UserId
             -> AForm Handler NewTopic
newTopicForm User{..} forumTopicBoard forumPostTime forumPostAuthor =
    makeNewTopic
    <$> areq textField "Title" Nothing
    <*> areq textareaField "Post" Nothing
  where
    forumTopicAuthor = forumPostAuthor
    forumTopicLatest = forumPostAuthor
    forumTopicTime   = forumPostTime
    forumTopicStaff  = userPrivilege /= Normal
    forumTopicPosts  = 1
    makeNewTopic rawTitle area = NewTopic ForumTopic{..}
                                 \forumPostTopic -> ForumPost{..}
      where
        forumTopicTitle = filter (/= Link.staffTag) rawTitle
        forumPostBody = toBody area

newPostForm :: ForumTopicId -> UTCTime -> UserId -> AForm Handler ForumPost
newPostForm forumPostTopic forumPostTime forumPostAuthor =
    makePost . toBody <$> areq textareaField "" Nothing
  where
    makePost forumPostBody = ForumPost{..}
