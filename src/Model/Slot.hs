module Model.Slot
  ( Slot, toInt, read
  , teamSize, gameSize
  , all, allies, enemies
  , liftChoice
  ) where

import ClassyPrelude.Yesod hiding (all)
import qualified Data.List as List
import qualified Data.Text.Read as Read

import Class.Parity (Parity)

newtype Slot = Slot Int deriving (Eq, Show, Read, ToJSON, Parity)

{-# INLINE toInt #-}
toInt :: Slot -> Int
toInt (Slot i) = i

teamSize :: Int
teamSize = 3
gameSize :: Int
gameSize = teamSize * 2

all :: [Slot]
all = Slot <$> [0 .. gameSize - 1]

par :: Int -> [Int]
par ((`rem` 2) -> i) = [i, i + 2 .. gameSize - 1]

allies :: Slot -> [Slot]
allies (Slot i) = Slot <$> List.delete i (par i)

enemies :: Slot -> [Slot]
enemies (Slot i) = Slot <$> par (i + 1)

-- | Translates a 'Target' into a list of 'Ninja's.
liftChoice :: (Int -> Int -> (Maybe Int, Maybe Int) -> [Int])
       -> Slot -> Slot -> (Maybe Slot, Maybe Slot) -> [Slot]
liftChoice f (Slot user) (Slot target) (ally, enemy) =
    Slot <$> f user target (sLift ally, sLift enemy)
  where
    sLift (Just (Slot s)) = Just s
    sLift Nothing         = Nothing

read :: Text -> Either String (Slot, Text)
read = (first Slot <$>) . Read.decimal
