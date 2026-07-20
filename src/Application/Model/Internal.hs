{-# OPTIONS_HADDOCK hide, not-home #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoStrictData          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Types generated from @config/models.persistentmodels@.
module Application.Model.Internal
    ( EntityField(..)
    , Unique(..)
    , Character(..), CharacterId
    , Mission(..), MissionId
    , News(..), NewsId
    , NewsTag(..), NewsTagId
    , Unlocked(..), UnlockedId
    , Usage(..), UsageId
    , User(..), UserId, level, levelXp, rank
    , Privilege(..)
    , Tag(..), TagId
    , migrateAll) where

import ClassyPrelude
import Yesod

import Database.Persist.Sql (fromSqlKey)
import Text.Blaze (ToMarkup(..))

import Util ((!?))

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

share [ mkPersistWith (sqlSettings { mpsFieldLabelModifier = \_entityName fieldName -> fieldName }) []
      , mkMigrate "migrateAll"
      ] [persistLowerCase|
    Character
        name  Text
        UniqueName name
        deriving Eq

    Mission
        user       UserId
        character  CharacterId
        objective  Int
        UniqueMission user character objective
        progress   Int

    News
        author   (Maybe UserId)
        time     UTCTime
        title    Text
        content  Text

    Tag
        name  Text
        UniqueTag name

    NewsTag
        news  NewsId
        tag   TagId
        UniqueNewsTag news tag

    Unlocked
        user       UserId
        character  CharacterId
        UniqueUnlocked user character

    Usage
        character  CharacterId
        UniqueUsage character
        wins      Int
        losses    Int
        picked    Int
        unpicked  Int

    User
        ident       Text
        UniqueUser ident
        password    Text  Maybe
        verkey      Text  Maybe
        verified    Bool
        joined      Day
        privilege   Privilege
        name        Text
        avatar      Text
        background  Text  Maybe
        xp          Int
        wins        Int
        losses      Int
        streak      Int
        record      Int
        latestWin   Day  Maybe
        latestGame  Day  Maybe
        clan        Text  Maybe
        team        [Text]  Maybe
        practice    [Text]
        rating      Double
        deviation   Double
        volatility  Double
        dna         Int
|]

instance Hashable (Key User) where
    hashWithSalt salt = hashWithSalt salt . fromEnum . fromSqlKey

xpPerLevel :: Int
xpPerLevel = 5000

level :: User -> Int
level User{xp} = (xp `quot` xpPerLevel) + 1

levelXp :: User -> Int
levelXp User{xp} = xp `rem` xpPerLevel

rank :: User -> Text
rank user@User{privilege = Normal} = fromMaybe "Hokage"
    $ userRanks !? (level user - 1)
  where
    userRanks :: Vector Text
    userRanks = fromList [ "Academy Student"
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
rank User{privilege} = tshow privilege


instance ToJSON User where
    toJSON user@User
        { avatar
        , background
        , clan
        , dna
        , losses
        , name
        , privilege
        , record
        , streak
        , wins
        } = object
        [ "privilege"  .= privilege
        , "name"       .= name
        , "avatar"     .= avatar
        , "background" .= background
        , "wins"       .= wins
        , "losses"     .= losses
        , "streak"     .= streak
        , "record"     .= record
        , "clan"       .= clan
        , "dna"        .= dna
        , "rank"       .= rank user
        , "level"      .= level user
        , "xp"         .= levelXp user
        ]

