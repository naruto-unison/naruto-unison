{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE QuasiQuotes                 #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- | User forum handler.
module Handler.Forum
    ( getProfileR
    , getForumsR
    , getBoardR
    , getTopicR
    , postTopicR
    , getNewTopicR
    , postNewTopicR
    , selectWithAuthors, makeCitelink
    ) where

import ClassyPrelude
import Yesod

import qualified Data.Time.Format as Format
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Text as Text
import qualified Yesod.Auth as Auth

import Util (shorten)
import Application.App (AppPersistEntity, Handler, Route(..), Widget)
import Application.Fields (ForumBoard, ForumCategory(..), Privilege(..), boardCategory, boardDesc, boardName)
import Application.Model (Cite(..), EntityField(..), HasAuthor(..), Post(..), Topic(..), TopicId, User(..), UserId)
import Application.Settings (widgetFile)

import qualified Game.Model.Class as Class
import qualified Game.Characters as Characters

-- | Renders a 'User' profile.
getProfileR :: Text -> Handler Html
getProfileR name = do
    muser                  <- runDB $ selectFirst [UserName ==. name] []
    Entity _ user@User{..} <- maybe notFound return muser
    let (level, xp)         = quotRem userXp 5000
    defaultLayout do
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "forum/profile")

data BoardIndex = BoardIndex ForumBoard Int (Maybe (Cite Topic))
inCategory :: ForumCategory -> BoardIndex -> Bool
inCategory category (BoardIndex x _ _) = category == boardCategory x

-- | Renders the forums.
getForumsR :: Handler Html
getForumsR = do
    citelink <- liftIO makeCitelink
    allBoards <- traverse indexBoard [minBound..maxBound]
    let boards category = filter (inCategory category) allBoards
    defaultLayout $(widgetFile "forum/browse")
  where
    categories = [minBound..maxBound]
    indexBoard board = do
        posts <- selectWithAuthors [TopicBoard ==. board] [Desc TopicTime]
        pure $ BoardIndex board (length posts) (listToMaybe posts)

-- | Renders a 'ForumBoard'.
getBoardR :: ForumBoard -> Handler Html
getBoardR board = do
    timestamp  <- liftIO makeTimestamp
    topics     <- selectWithAuthors [TopicBoard ==. board] []
    defaultLayout $(widgetFile "forum/board")

-- | Renders a 'Topic'.
getTopicR :: TopicId -> Handler Html
getTopicR topicId = do
    mwho       <- Auth.maybeAuthId
    (title, _) <- breadcrumbs
    time       <- liftIO getCurrentTime
    timestamp  <- liftIO makeTimestamp
    zone       <- liftIO LocalTime.getCurrentTimeZone
    Topic{..}  <- runDB $ get404 topicId
    posts      <- selectWithAuthors [PostTopic ==. topicId] []
    mwidget    <- traverse
                  (generateFormPost . renderTable . newPostForm topicId time)
                  mwho
    defaultLayout $(widgetFile "forum/topic")

-- | Adds to a 'Topic'. Requires authentication.
postTopicR :: TopicId -> Handler Html
postTopicR topicId = do
    who         <- Auth.requireAuthId
    (title, _)  <- breadcrumbs
    time        <- liftIO getCurrentTime
    timestamp   <- liftIO makeTimestamp
    ((result, widget), enctype) <- runFormPost . renderTable $
                                   newPostForm topicId time who
    case result of
        FormSuccess post -> runDB do
            insert400_ post
            update topicId [ TopicPosts +=. 1
                           , TopicTime   =. time
                           , TopicLatest =. who
                           ]
        _ -> return ()
    Topic{..}  <- runDB $ get404 topicId
    posts      <- selectWithAuthors [PostTopic ==. topicId] []
    let mwidget = Just (widget, enctype)
    defaultLayout $(widgetFile "forum/topic")

-- | Renders a page for creating a new 'Topic'. Requires authentication.
getNewTopicR :: ForumBoard -> Handler Html
getNewTopicR board = do
    (who, user) <- Auth.requireAuthPair
    time        <- liftIO getCurrentTime
    (title, _)  <- breadcrumbs
    (widget, enctype) <- generateFormPost . renderTable $
                         newTopicForm user board time who
    defaultLayout $(widgetFile "forum/new")

-- | Creates a new 'Topic'. Requires authentication.
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

-- | Appended to titles of posts and threads by staff.
staffTag :: Char
staffTag = '*'

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
        return Cite{..}
      where
        author = getAuthor citeVal
        latest = getLatest citeVal

-- | Displays a user's rank, or their 'Privilege' level if higher than 'Normal'.
userRank :: User -> Text
userRank User{..} = case userPrivilege of
    Normal -> maybe "Hokage" fst . uncons $ drop level userRanks
    _      -> tshow userPrivilege
  where
    level = userXp `quot` 5000

data NewTopic = NewTopic Topic (TopicId -> Post)

toBody :: Textarea -> [Text]
toBody (Textarea area) = Text.splitOn "\n" area

newTopicForm :: User -> ForumBoard -> UTCTime -> UserId
             -> AForm Handler NewTopic
newTopicForm User{..} topicBoard postTime postAuthor = makeNewTopic
    <$> areq textField "Title" Nothing
    <*> areq textareaField "Post" Nothing
  where
    topicAuthor = postAuthor
    topicLatest = postAuthor
    topicTime   = postTime
    topicStaff  = userPrivilege /= Normal
    topicPosts  = 1
    makeNewTopic rawTitle area = NewTopic Topic{..} \postTopic -> Post{..}
      where
        topicTitle = filter (/= staffTag) rawTitle
        postBody = toBody area

newPostForm :: TopicId -> UTCTime -> UserId -> AForm Handler Post
newPostForm postTopic postTime postAuthor =
    makePost . toBody <$> areq textareaField "" Nothing
  where
    makePost postBody = Post{..}

userlink :: User -> Widget
userlink User{..} = $(widgetFile "widgets/userlink")

topiclink :: Cite Topic -> Widget
topiclink Cite{..} = $(widgetFile "widgets/topiclink")

makeCitelink :: IO (Cite Topic -> Widget)
makeCitelink = do
    timestamp <- makeTimestamp
    return \cite@Cite{..} -> $(widgetFile "widgets/citelink")

makeTimestamp :: IO (UTCTime -> Widget)
makeTimestamp = do
    time   <- getCurrentTime
    zone   <- LocalTime.getCurrentTimeZone
    return . pureTimestamp zone $ utctDay time

pureTimestamp :: LocalTime.TimeZone -> Day -> UTCTime -> Widget
pureTimestamp zone today unzoned = $(widgetFile "widgets/timestamp")
  where
    zoned = LocalTime.utcToLocalTime zone unzoned
    time  = Format.formatTime Format.defaultTimeLocale format zoned
    format
      | utctDay unzoned == today = "%l:%M %p"
      | otherwise                = "%m/%d/%y"
