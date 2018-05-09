{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Persistent fields.
module Core.Fields where

import GHC.Generics
import Data.Aeson
import ClassyPrelude.Yesod (derivePersistField)

data Privilege = Normal 
               | Moderator 
               | Admin 
               deriving (Enum, Eq, Show, Read, Generic, FromJSON, ToJSON)
derivePersistField "Privilege"
