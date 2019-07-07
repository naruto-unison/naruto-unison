{-# LANGUAGE DeriveAnyClass #-}
module Model.Variant
  ( Variant(..)
  , cooldown
  , none
  ) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import Class.TurnBased (TurnBased(..))

data Variant = Variant { variant   :: Int -- ^ Index in 'skills'
                       , ownCd     :: Bool -- ^ Uses a different cooldown than the baseline 'Skill'
                       , name      :: Text
                       , fromSkill :: Bool -- ^ Duration is based on a 'Skill'
                       , dur       :: Int
                       } deriving (Eq, Ord, Show, Read, Generic, ToJSON)
instance TurnBased Variant where
    getDur        = dur
    setDur x vari = vari { dur = x }

cooldown :: Variant -> Int
cooldown v
  | ownCd v   = variant v
  | otherwise = 0

none :: Variant
none = Variant 0 False "" False 0

