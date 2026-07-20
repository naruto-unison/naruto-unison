module Application.Model.NewsPost
    ( NewsPost(..)
    , getOne
    , selectAll
    ) where

import ClassyPrelude hiding (groupBy, on)
import Database.Esqueleto.Experimental
import Database.Esqueleto.PostgreSQL

import Control.Monad.Trans.Maybe (MaybeT(..))

import Application.Model.Internal (EntityField(..), News, NewsId, NewsTag, Tag, User)
import Application.Model.News (getTags)

data NewsPost = NewsPost
    { newsID :: NewsId
    , news   :: News
    , author :: Maybe User
    , tags   :: [Text]
    }

getOne :: ∀ m. MonadIO m => NewsId -> SqlPersistT m (Maybe NewsPost)
getOne newsID = runMaybeT do
    (Entity _ news, author) <- MaybeT $ selectOne do
        (news :& author) <-
            from $ table @News
            `leftJoin` table @User
            `on` (\(news :& user) -> just (news ^. NewsAuthor) ==. user ?. UserId)
        return (news, author)
    tags <- lift $ getTags newsID
    return NewsPost
        { newsID
        , news
        , author = entityVal <$> author
        , tags
        }

selectAll :: ∀ m. MonadIO m => Int64 -> Int64 -> Maybe Text -> SqlPersistT m [NewsPost]
selectAll limit_ page mtagName = fromMaybe [] <$> runMaybeT do
    mtag <- forM mtagName \tagName ->
        MaybeT $ selectOne do
            tag <- from $ table @Tag
            where_ $ tag ^. TagName ==. val tagName
            return $ tag ^. TagId
    lift $ (toPost <$>) <$> select do
        (news :& author :& _ :& tag) <-
            from $ table @News
            `leftJoin` table @User
            `on` (\(news :& user) -> just (news ^. NewsAuthor) ==. user ?. UserId)
            `leftJoin` table @NewsTag
            `on` (\(news :& _ :& newsTag) -> just (news ^. NewsId) ==. newsTag ?. NewsTagNews)
            `leftJoin` table @Tag
            `on` (\(_ :& _ :& newsTag :& tag) -> newsTag ?. NewsTagTag ==. tag ?. TagId)
        forM mtag \(Value tagId) ->
            where_ $ news ^. NewsId `in_` subSelectList do
                newsTag <- from $ table @NewsTag
                where_ $ newsTag ^. NewsTagTag ==. val tagId
                return $ newsTag ^. NewsTagNews
        groupBy $ news ^. NewsId
        groupBy $ author ?. UserId
        orderBy [ desc $ news ^. NewsTime ]
        limit limit_
        offset $ limit_ * (page - 1)
        return (news, author, maybeArray $ arrayAgg $ tag ?. TagName)
  where
    toPost (Entity newsID news, author, Value tags) = NewsPost
        { newsID
        , news
        , author = entityVal <$> author
        , tags = catMaybes tags
        }

