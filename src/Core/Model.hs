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

instance Hashable (Key User) where
    hashWithSalt salt = hashWithSalt salt .
                        (fromIntegral :: ∀ a. Integral a => a -> Int) .
                        SQL.fromSqlKey

instance PathPiece UTCTime where
  fromPathPiece = Aeson.decodeStrict . encodeUtf8
  toPathPiece   = toStrict . decodeUtf8 . Aeson.encode
