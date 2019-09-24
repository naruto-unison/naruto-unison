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

import Core.App (AppPersistEntity, Handler, Route(..), Widget)
import Core.Fields (ForumBoard, ForumCategory(..), Privilege(..), boardCategory, boardDesc, boardName)
import Core.Model (Cite(..), EntityField(..), HasAuthor(..), Post(..), Topic(..), TopicId, User(..), UserId)
import Core.Settings (widgetFile)
import Core.Util (shorten)
import qualified Characters
-- | Renders a 'User' profile.
getProfileR :: Text -> Handler Html
getProfileR name = do
    muser                  <- runDB $ selectFirst [UserName ==. name] []
    Entity _ user@User{..} <- maybe notFound return muser
    let (level, xp)         = quotRem userXp 5000
    defaultLayout do
        setTitle . toHtml $ "User: " ++ userName
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
    defaultLayout do
        setTitle "Naruto Unison Forums"
        $(widgetFile "forum/browse")
  where
    categories = [minBound..maxBound]
    indexBoard board = do
        posts <- selectWithAuthors [TopicBoard ==. board] [Desc TopicTime]
        pure $ BoardIndex board (length posts) (listToMaybe posts)

-- | Renders a 'ForumBoard'.
getBoardR :: ForumBoard -> Handler Html
getBoardR board = do
    timestamp <- liftIO makeTimestamp
    topics    <- selectWithAuthors [TopicBoard ==. board] []
    defaultLayout do
        setTitle . toHtml $ "Forum: " ++ boardName board
        $(widgetFile "forum/board")

-- | Renders a 'Topic'.
getTopicR :: TopicId -> Handler Html
getTopicR topicId = do
    who       <- Auth.requireAuthId
    time      <- liftIO getCurrentTime
    timestamp <- liftIO makeTimestamp
    zone      <- liftIO LocalTime.getCurrentTimeZone
    Topic{..} <- runDB $ get404 topicId
    posts     <- selectWithAuthors [PostTopic ==. topicId] []
    (widget, enctype) <- generateFormPost . renderTable $
                         newPostForm topicId who time
    defaultLayout do
        setTitle . toHtml $ "Topic: " ++ topicTitle
        $(widgetFile "forum/topic")

-- | Adds to a 'Topic'.
postTopicR :: TopicId -> Handler Html
postTopicR topicId = do
    who       <- Auth.requireAuthId
    time      <- liftIO getCurrentTime
    timestamp <- liftIO makeTimestamp
    ((result, widget), enctype) <- runFormPost . renderTable $
                                   newPostForm topicId who time
    case result of
        FormSuccess post -> runDB do
            insert400_ post
            update topicId [ TopicPosts +=. 1
                           , TopicTime   =. time
                           , TopicLatest =. who
                           ]
        _ -> return ()
    Topic{..} <- runDB $ get404 topicId
    posts     <- selectWithAuthors [PostTopic ==. topicId] []
    defaultLayout do
        setTitle . toHtml $ "Topic: " ++ topicTitle
        $(widgetFile "forum/topic")

-- | Renders a page for creating a new 'Topic'.
getNewTopicR :: ForumBoard -> Handler Html
getNewTopicR board = do
    (who, user) <- Auth.requireAuthPair
    time        <- liftIO getCurrentTime
    (widget, enctype) <- generateFormPost . renderTable $
                         newTopicForm user board who time
    defaultLayout do
        setTitle "New Topic"
        $(widgetFile "forum/new")

-- | Creates a new 'Topic'.
postNewTopicR :: ForumBoard -> Handler Html
postNewTopicR board = do
    (who, user) <- Auth.requireAuthPair
    time        <- liftIO getCurrentTime
    ((result, widget), enctype) <- runFormPost . renderTable $
                                   newTopicForm user board who time
    case result of
        FormSuccess (NewTopic topic makePost) -> do
            topicId <- runDB $ insert400 topic
            let post = makePost topicId
            runDB $ insert400_ post
            redirect $ TopicR topicId
        _ -> defaultLayout do
            setTitle "New Topic"
            $(widgetFile "forum/new")

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
selectWithAuthors selectors opts = runDB (selectList selectors opts)
                                   >>= traverse go
  where
    go (Entity citeKey citeVal) = runDB do
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

newTopicForm :: User -> ForumBoard -> UserId -> UTCTime
             -> AForm Handler NewTopic
newTopicForm User{..} topicBoard postAuthor postTime = makeNewTopic
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

newPostForm :: TopicId -> UserId -> UTCTime -> AForm Handler Post
newPostForm postTopic postAuthor postTime =
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
