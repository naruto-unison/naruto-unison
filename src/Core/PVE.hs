{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide     #-}

module Core.PVE where

import GHC.Generics
import Data.Aeson
import ClassyPrelude.Yesod (derivePersistField)

-- * USER FIELDS

data Threshold = Above Int | Below Int | Equals Int
                 deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)
derivePersistField "Threshold"

data Character = PC String | NPC String

data Chakra = Bloodline | Genjutsu | Ninjutsu | Taijutsu | Random | AnyChakra

data Variable = Health