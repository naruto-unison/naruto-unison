{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_HADDOCK hide                #-}

module Core.Model where

import Data.Hashable
import Data.Time.Clock
import Database.Persist.Quasi
import Database.Persist.Postgresql (fromSqlKey)
import ClassyPrelude.Yesod
import Data.Aeson (decodeStrict, encode)
--import Data.Text.Encoding

import Core.Fields
import Core.Unicode

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance Hashable (Key User) where
    hashWithSalt salt = hashWithSalt salt
                      ∘ (fromIntegral ∷ Integral a ⇒ a → Int)
                      ∘ fromSqlKey

instance PathPiece UTCTime where
  fromPathPiece = decodeStrict ∘ encodeUtf8
  toPathPiece   = toStrict ∘ decodeUtf8 ∘ encode
