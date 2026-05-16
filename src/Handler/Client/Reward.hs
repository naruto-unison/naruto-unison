{-# LANGUAGE DeriveAnyClass #-}

module Handler.Client.Reward (Reward(..)) where

import ClassyPrelude
import Data.Aeson (ToJSON)

-- | XP or DNA awarded to a player.
data Reward = Reward { reason :: Text
                     , amount :: Int
                     } deriving (Eq, Ord, Show, Read, Generic, ToJSON)
