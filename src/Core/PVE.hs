{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_HADDOCK hide       #-}

module Core.PvE where

import StandardLibrary
import GHC.Generics ()
import Data.Aeson ()

-- * USER FIELDS

data Threshold = Above Int | Below Int | Equals Int
                 deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

derivePersistField "Threshold"

data Character = PC String | NPC String

data Chakra = Bloodline | Genjutsu | Ninjutsu | Taijutsu | Random | AnyChakra

data Variable = Health
