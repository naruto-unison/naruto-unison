{-# LANGUAGE DeriveAnyClass #-}
module Model.Variant
  ( Variant(..)
  , cooldown
  , none
  , Varying(..)
  ) where

import ClassyPrelude

import Data.Aeson (ToJSON(..))

import Class.TurnBased (TurnBased(..))

data Varying
    = Duration Int
    | FromSkill Text
    deriving (Eq, Ord, Show, Read)

instance ToJSON Varying where
    toJSON = toJSON . getDur

instance TurnBased Varying where
    getDur (Duration x) = x
    getDur FromSkill{}  = 0
    setDur x Duration{} = Duration x
    setDur _ v          = v

data Variant = Variant { variant   :: Int -- ^ Index in 'skills'
                       , ownCd     :: Bool -- ^ Uses a different cooldown than the baseline 'Skill'
                       , dur       :: Varying
                       } deriving (Eq, Ord, Show, Read, Generic, ToJSON)
instance TurnBased Variant where
    getDur        = getDur . dur
    setDur x vari = vari { dur = setDur x $ dur vari }

cooldown :: Variant -> Int
cooldown v
  | ownCd v   = variant v
  | otherwise = 0

none :: Variant
none = Variant 0 False $ Duration 0

