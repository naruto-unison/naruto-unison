-- DO NOT EXPOSE ANY FUNCTION THAT COULD BE USED TO CONSTRUCT OR ALTER A SLOT. 
-- It must be guaranteed that all Slots wrap numbers in the range [0, gameSize).
module Model.Slot
  ( Slot, toInt, read
  , teamSize, gameSize
  , all, allies, enemies
  , liftChoice
  ) where

import ClassyPrelude hiding (all)

import           Data.Aeson (ToJSON)
import qualified Data.List as List
import qualified Data.Text.Read as Read

import Class.Parity (Parity)

-- | A 'Slot' represents an index in 'Model.Game.ninjas'.
-- It is hidden behind a newtype and cannot be constructed outside this module.
-- This prevents arithmetic manipulation and out-of-bounds errors.
-- It has the added advantage of making function signatures much more readable!
newtype Slot = Slot Int deriving (Eq, Ord, Show, Read, ToJSON, Parity)

{-# INLINE toInt #-}
toInt :: Slot -> Int
toInt (Slot i) = i

teamSize :: Int
teamSize = 3
gameSize :: Int
gameSize = teamSize * 2

all :: [Slot]
all = Slot <$> [0 .. gameSize - 1]

-- | Values with the same parity. Bounded by @[0, 'gameSize')@.
par :: Int -> [Int]
par ((`rem` 2) -> i) = [i, i + 2 .. gameSize - 1]

-- | Slots with the same parity. Bounded by @[0, 'gameSize')@.
allies :: Slot -> [Slot]
allies (Slot i) = Slot <$> List.delete i (par i)

-- | Slots with opposite parity. Bounded by @[0, 'gameSize']@.
enemies :: Slot -> [Slot]
enemies (Slot i) = Slot <$> par (i + 1)

-- | Translates a 'Model.Skill.Target' into a list of 'Model.Ninja.Ninja's.
liftChoice :: (Int -> Int -> (Maybe Int, Maybe Int) -> [Int])
       -> Slot -> Slot -> (Maybe Slot, Maybe Slot) -> [Slot]
liftChoice f (Slot user) (Slot target) (ally, enemy) =
    Slot <$> f user target (sLift ally, sLift enemy)
  where
    sLift (Just (Slot s)) = Just s
    sLift Nothing         = Nothing

read :: Text -> Either String (Slot, Text)
read = (first Slot <$>) . Read.decimal
