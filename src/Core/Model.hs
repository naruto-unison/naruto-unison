{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE NoStrictData    #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types generated from @config/models.persistentmodels@.
module Core.Model where

import ClassyPrelude
import Yesod

import qualified Database.Persist.Quasi as Quasi
import qualified Database.Persist.Postgresql as Sql

import Core.Fields (ForumBoard, Privilege)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith Quasi.lowerCaseSettings "config/models.persistentmodels")

instance Ord Post where
    compare = comparing postTime

instance Hashable (Key User) where
    hashWithSalt salt = hashWithSalt salt . fromEnum . Sql.fromSqlKey

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

-- | Types that can be summarized with oldest and most recent users to post.
class HasAuthor a where
    getAuthor :: a -> UserId
    getLatest :: a -> UserId

instance HasAuthor Topic where
    getAuthor = topicAuthor
    getLatest = topicLatest

instance HasAuthor Post where
    getAuthor = postAuthor
    getLatest = postAuthor

-- | A summary with a link, name, and oldest and most recent users to post.
data Cite a = Cite { citeKey    :: Key a
                   , citeVal    :: a
                   , citeAuthor :: User
                   , citeLatest :: User
                   }
