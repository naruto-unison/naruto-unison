{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Miscellaneous website handlers.
module Handler.News
  ( getNewsR, getNewsPageR
  , getNewsTaggedR, getNewsTaggedPageR
  , getNewsPostR
  , getCreateNewsR, postCreateNewsR
  , getEditNewsR, postEditNewsR
  ) where

import ClassyPrelude
import Yesod

import           Data.Text (dropAround)
import           Text.Blaze.Html (preEscapedToHtml)
import qualified Yesod.Auth as Auth

import           Application.App (Route(..))
import qualified Application.App as App
import           Application.Model (EntityField(..))
import           Application.Model.News (News(News), NewsId)
import qualified Application.Model.News as News
import qualified Application.Model.NewsPost as NewsPost
import           Application.Model.NewsPost (NewsPost(NewsPost))
import           Application.Settings (widgetFile)
import           Class.Display (display)
import qualified Class.Parse as Parse
import           Handler.Client.Data (addDataJS)
import qualified Handler.Site.Link as Link
import           Util (rightToMaybe)

postsPerPage :: Int64
postsPerPage = 10

toPage :: Text -> Int64
toPage page = fromMaybe 1 do
    n <- rightToMaybe $ Parse.parseToEnd page
    guard $ n > 0
    return n

listNews :: Int64 -> Maybe Text -> App.Handler Html
listNews page mtag = do
    newsList <- runDB $ NewsPost.selectAll postsPerPage page mtag
    defaultLayout do
        addDataJS
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "news/news")
  where
    prevPage = case (page, mtag) of
        (1, _) -> Nothing
        (2, Nothing) -> Just NewsR
        (2, Just tag) -> Just $ NewsTaggedR tag
        (i, Nothing)  -> Just $ NewsPageR           $ tshow $ i - 1
        (i, Just tag) -> Just $ NewsTaggedPageR tag $ tshow $ i - 1
    nextPage = case mtag of
        Nothing  -> Just $ NewsPageR           $ tshow $ page + 1
        Just tag -> Just $ NewsTaggedPageR tag $ tshow $ page + 1

-- | Renders the homepage of the website.
getNewsR :: App.Handler Html
getNewsR = listNews 1 Nothing

-- | Renders the homepage of the website.
getNewsPageR :: Text -> App.Handler Html
getNewsPageR page = listNews (toPage page) Nothing

getNewsTaggedR :: Text -> App.Handler Html
getNewsTaggedR tag = listNews 1 $ Just tag

getNewsTaggedPageR :: Text -> Text -> App.Handler Html
getNewsTaggedPageR tag page = listNews (toPage page) $ Just tag

getNewsPostR :: NewsId -> App.Handler Html
getNewsPostR newsID = do
    mnewsPost <- runDB $ NewsPost.getOne newsID
    NewsPost{author, tags, news = News{content, time, title}} <- case mnewsPost of
        Just newsPost -> return newsPost
        Nothing -> notFound
    defaultLayout do
        addDataJS
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "news/post")

getCreateNewsR :: App.Handler Html
getCreateNewsR = do
    App.unchanged304
    (newsForm, enctype) <- generateFormPost createNewsForm
    defaultLayout $(widgetFile "news/create")

postCreateNewsR :: App.Handler Html
postCreateNewsR = do
    ((result, newsForm), enctype) <- runFormPost createNewsForm
    case result of
        FormSuccess (news, tags) -> do
            newsID <- runDB do
                newsID <- insert400 news
                News.insertTags newsID tags
                return newsID
            redirect $ NewsPostR newsID
        FormMissing -> do
            setMessage "Form missing"
            defaultLayout $(widgetFile "news/create")
        FormFailure errs -> do
            setMessage $ toHtml
                $ "Invalid update: " ++ intercalate ", " (display <$> errs)
            defaultLayout $(widgetFile "news/create")

getEditNewsR :: NewsId -> App.Handler Html
getEditNewsR newsID = do
    news <- runDB $ get404 newsID
    tags <- runDB $ News.getTags newsID
    (newsForm, enctype) <- generateFormPost $ editNewsForm (news, tags)
    defaultLayout $(widgetFile "news/edit")


postEditNewsR :: NewsId -> App.Handler Html
postEditNewsR newsID = do
    news <- runDB $ get404 newsID
    tags <- runDB $ News.getTags newsID
    ((result, newsForm), enctype) <- runFormPost $ editNewsForm (news, tags)
    case result of
        FormSuccess (News{title, content}, tags') -> do
            let oldTags = setFromList @(HashSet _) tags
                newTags = setFromList @(HashSet _) tags'
            runDB do
                update newsID [ NewsTitle =. title, NewsContent =. content ]
                News.insertTags newsID $ toList $ newTags \\ oldTags
                News.removeTags newsID $ toList $ oldTags \\ newTags
            redirect $ NewsPostR newsID
        FormMissing -> do
            setMessage "Form missing"
            defaultLayout $(widgetFile "news/edit")
        FormFailure errs -> do
            setMessage $ toHtml
                $ "Invalid update: " ++ intercalate ", " (display <$> errs)
            defaultLayout $(widgetFile "news/edit")


splitTags :: Text -> [Text]
splitTags tags = dropAround (== ' ') <$> splitElem ',' tags

joinTags :: [Text] -> Text
joinTags = intercalate ", "

createNewsForm :: Html -> App.MForm (News, [Text])
createNewsForm = renderDivs do
    author  <- lift Auth.requireAuthId
    time    <- lift $ liftIO getCurrentTime
    title   <- areq textField "Title" Nothing
    content <- areq textareaField "Content" Nothing
    tags    <- areq textField "Tags" Nothing
    return (News { author = Just author
                 , time
                 , title
                 , content = unTextarea content
                 }, splitTags tags)

editNewsForm :: (News, [Text]) -> Html -> App.MForm (News, [Text])
editNewsForm (news, tagList) = renderDivs do
    title   <- areq textField "Title" $ Just news.title
    content <- areq textareaField "Content" $ Just $ Textarea news.content
    tags    <- areq textField "Tag" $ Just $ joinTags tagList
    return (news { News.title = title
                 , News.content = unTextarea content
                 }, splitTags tags)
