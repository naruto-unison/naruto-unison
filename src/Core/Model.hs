{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrictData    #-}
module Core.Model where

import ClassyPrelude.Yesod
import qualified Database.Persist.Quasi as Quasi
import qualified Database.Persist.Postgresql as Sql

import Core.Fields (ForumBoard, Privilege)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith Quasi.lowerCaseSettings "config/models.persistentmodels")


instance Ord Post where
    compare x y = compare (postTime x) (postTime y)

instance Hashable (Key User) where
    hashWithSalt salt = hashWithSalt salt .
                        (fromIntegral :: ∀ a. Integral a => a -> Int) .
                        Sql.fromSqlKey
{-}
instance PathPiece UTCTime where
  fromPathPiece = Aeson.decodeStrict . encodeUtf8
  toPathPiece   = toStrict . decodeUtf8 . Aeson.encode
-}
class HasAuthor a where
    getAuthor :: a -> UserId
    getLatest :: a -> UserId

instance ToJSON User where
    toJSON User{..} = object
        [ "name"       .= userName
        , "avatar"     .= userAvatar
        , "clan"       .= userClan
        , "xp"         .= userXp
        , "wins"       .= userWins
        , "losses"     .= userLosses
        , "streak"     .= userStreak
        , "background" .= userBackground
        , "privilege"  .= userPrivilege
        , "condense"   .= userCondense
        ]

instance HasAuthor Topic where
    getAuthor = topicAuthor
    getLatest = topicLatest
instance HasAuthor Post where
    getAuthor = postAuthor
    getLatest = postAuthor

data Cite a = Cite { citeKey    :: Key a
                   , citeVal    :: a
                   , citeAuthor :: User
                   , citeLatest :: User
                   }
