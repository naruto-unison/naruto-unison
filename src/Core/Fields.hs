{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide     #-}

module Core.Fields where

import GHC.Generics
import Data.Aeson
import ClassyPrelude.Yesod (derivePersistField)

-- * USER FIELDS

data Privilege = Normal 
               | Moderator 
               | Admin 
               deriving (Enum, Eq, Show, Read, Generic, FromJSON, ToJSON)
derivePersistField "Privilege"