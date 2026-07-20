module Application.Model.News
    ( News(..), NewsId
    , getTitle
    , insertTag, getTags, insertTags, removeTags
    ) where

import ClassyPrelude hiding (delete, on)
import Database.Esqueleto.Experimental

import Application.Model.Internal (EntityField(..), News(..), NewsId, NewsTag(NewsTag), Tag(Tag), TagId)

getTitle :: ∀ m. MonadIO m => NewsId -> SqlPersistT m Text
getTitle newsID = toTitle <$> selectOne do
    news <- from $ table @News
    where_ $ news ^. NewsId ==. val newsID
    return $ news ^. NewsTitle
  where
    toTitle (Just (Value title)) = title
    toTitle Nothing              = ""

insertTag :: ∀ m. MonadIO m => Text -> SqlPersistT m TagId
insertTag name = entityKey <$> (upsert (Tag name) [])

getTags :: MonadIO m => NewsId -> SqlPersistT m [Text]
getTags newsID = (unValue <$>) <$> select do
    (tag :& newsTag) <-
        from $ table @Tag
        `leftJoin` table @NewsTag
        `on` \(tag :& newsTag) -> just (tag ^. TagId) ==. newsTag ?. NewsTagTag
    where_ $ newsTag ?. NewsTagNews ==. just (val newsID)
    return $ tag ^. TagName

insertTags :: MonadIO m => NewsId -> [Text] -> SqlPersistT m ()
insertTags _ [] = return ()
insertTags newsId tags = do
    tagIds <- mapM insertTag tags
    insertMany_ $ NewsTag newsId <$> tagIds

removeTags :: MonadIO m => NewsId -> [Text] -> SqlPersistT m ()
removeTags _ [] = return ()
removeTags newsId tags = delete do
        newsTag <- from $ table @NewsTag
        where_ $ newsTag ^. NewsTagNews ==. val newsId
        where_ $ newsTag ^. NewsTagTag `in_` subSelectList do
            tag <- from $ table @Tag
            where_ $ tag ^. TagName `in_` valList tags
            return $ tag ^. TagId

