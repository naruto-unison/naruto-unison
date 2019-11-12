{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE NoStrictData    #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types generated from @config/models.persistentmodels@.
module Application.Model where

import ClassyPrelude
import Yesod

import qualified Database.Persist.Quasi as Quasi
import qualified Database.Persist.Sql as Sql

import Application.Fields (ForumBoard, Markdown(..), Privilege(..), TopicState(..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith Quasi.lowerCaseSettings "config/models.persistentmodels")

instance Ord ForumPost where
    compare = comparing forumPostTime

instance Hashable (Key User) where
    hashWithSalt salt = hashWithSalt salt . fromEnum . Sql.fromSqlKey

instance ToJSON User where
    toJSON User{..} = object
        [ "privilege"  .= userPrivilege
        , "name"       .= userName
        , "avatar"     .= userAvatar
        , "background" .= userBackground
        , "xp"         .= userXp
        , "wins"       .= userWins
        , "losses"     .= userLosses
        , "streak"     .= userStreak
        , "record"     .= userRecord
        , "clan"       .= userClan
        , "muted"      .= userMuted
        , "condense"   .= userCondense
        , "dna"        .= userDna
        ]

newUser :: Text -> Maybe Text -> Day -> User
newUser ident verkey day = User
    { userIdent      = ident
    , userPassword   = Nothing
    , userVerkey     = verkey
    , userVerified   = False
    , userJoined     = day
    , userPrivilege  = Normal
    , userName       = ident
    , userAvatar     = "/img/icon/default.jpg"
    , userBackground = Nothing
    , userXp         = 0
    , userWins       = 0
    , userLosses     = 0
    , userStreak     = 0
    , userRecord     = 0
    , userLatestWin  = Nothing
    , userLatestGame = Nothing
    , userClan       = Nothing
    , userTeam       = Nothing
    , userPractice   = ["Naruto Uzumaki", "Sakura Haruno", "Sasuke Uchiha"]
    , userMuted      = False
    , userCondense   = False
    , userRating     = 0.0
    , userDeviation  = 350.0 / 173.7178
    , userVolatility = 0.06
    , userDna        = 0
    , userPosts      = 0
    }

-- | Types that can be summarized with oldest and most recent users to post.
class HasAuthor a where
    getAuthor :: a -> UserId
    getLatest :: a -> UserId

instance HasAuthor ForumPost where
    getAuthor = forumPostAuthor
    getLatest = forumPostAuthor

instance HasAuthor ForumTopic where
    getAuthor = forumTopicAuthor
    getLatest = forumTopicLatest

-- | A summary with a link, name, and oldest and most recent users to post.
data Cite a = Cite { citeKey    :: Key a
                   , citeVal    :: a
                   , citeAuthor :: User
                   , citeLatest :: User
                   }
