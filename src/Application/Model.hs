{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types generated from @config/models.persistentmodels@.
module Application.Model
    ( EntityField(..)
    , Unique(..)
    , Character(..), CharacterId
    , Mission(..), MissionId
    , News(..), NewsId
    , Unlocked(..), UnlockedId
    , Usage(..), UsageId
    , User(..), UserId, newUser
    , Privilege(..)
    , entityDefListFormigrateAll
    , migrateAll) where

import ClassyPrelude
import Yesod

import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.Sql (fromSqlKey)
import Text.Blaze (ToMarkup(..))

-- | User privilege. Determines authorization level.
data Privilege
    = Guest
    | Normal
    | Moderator
    | Admin
  deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic)
instance FromJSON Privilege
instance ToJSON Privilege
derivePersistField "Privilege"

instance ToMarkup Privilege where
    toMarkup = toMarkup . show

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

instance Hashable (Key User) where
    hashWithSalt salt = hashWithSalt salt . fromEnum . fromSqlKey

instance ToJSON User where
    toJSON User
        { userAvatar
        , userBackground
        , userClan
        , userCondense
        , userDna
        , userLosses
        , userName
        , userPrivilege
        , userRecord
        , userStreak
        , userWins
        , userXp
        } = object
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
    , userCondense   = False
    , userRating     = 0.0
    , userDeviation  = 350.0 / 173.7178
    , userVolatility = 0.06
    , userDna        = 0
    }
