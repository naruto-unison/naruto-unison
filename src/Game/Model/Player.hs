module Game.Model.Player
  ( Player(..)
  , opponent
  ) where

import ClassyPrelude hiding (even)

import           Data.Aeson (ToJSON)
import           Data.Bits
import           Data.Enum.Set (AsEnumSet)
import           System.Random.Stateful (Uniform(..), UniformRange(..))
import qualified System.Random.Stateful as R

import           Class.Parity (Parity)
import qualified Class.Parity as Parity

data Player = A | B deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance AsEnumSet Player

instance ToJSON Player

instance Parity Player where
    even A = True
    even B = False
    {-# INLINE even #-}

instance Uniform Player where
    uniformM g = fromWord <$> R.uniformWord32 g
      where
        fromWord w = toEnum $ fromEnum $ w .&. 1
    {-# INLINE uniformM #-}

instance UniformRange Player where
    uniformRM (A,A) _ = return A
    uniformRM (B,B) _ = return B
    uniformRM _     g = R.uniformM g
    {-# INLINE uniformRM #-}

opponent :: Player -> Player
opponent A = B
opponent B = A
