{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Yesod models from config/models.
module Core.Model where

import StandardLibrary
import qualified Data.Aeson as Aeson
import qualified Database.Persist.Postgresql as SQL
import qualified Database.Persist.Quasi as Quasi

import Core.Fields

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith Quasi.lowerCaseSettings "config/models")

instance Ord Post where
    compare x y = compare (postTime x) (postTime y)

instance Hashable (Key User) where
    hashWithSalt salt = hashWithSalt salt .
                        (fromIntegral :: ∀ a. Integral a => a -> Int) .
                        SQL.fromSqlKey

instance PathPiece UTCTime where
  fromPathPiece = Aeson.decodeStrict . encodeUtf8
  toPathPiece   = toStrict . decodeUtf8 . Aeson.encode

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
                   