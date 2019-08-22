-- DO NOT EXPOSE ANY FUNCTION THAT COULD BE USED TO CONSTRUCT OR ALTER A SLOT.
-- It must be guaranteed that all Slots are within their 'Bounded' range.
module Model.Slot
  ( Slot, toInt, read
  , teamSize
  , all, allies, enemies, evens, odds
  , random
  ) where

import ClassyPrelude hiding (all)

import           Data.Aeson (ToJSON)
import qualified Data.Text.Read as Read
import           Text.Read hiding (read)
import           Text.Read.Lex (numberToInteger)

import qualified Class.Parity as Parity
import           Class.Parity (Parity)
import qualified Class.Random as R
import           Class.Random (MonadRandom)

teamSize :: Int
teamSize = 3

maxVal :: Int
maxVal = teamSize * 2 - 1

-- | A @Slot@ represents an index in 'Model.Game.ninjas'.
-- It is hidden behind a newtype and cannot be constructed or modified outside
-- this module in order to prevent out-of-bound errors.
-- This has the added advantage of making function signatures more readable!
newtype Slot = Slot { toInt :: Int }
               deriving (Eq, Ord, Show, ToJSON, Parity)

instance Read Slot where
    readPrec = parens $ prec 10 do
        Number n <- lexP
        case numberToInteger n of
            Just (fromInteger -> i) | i >= 0 && i <= maxVal -> return $ Slot i
            _                                               -> empty

instance Bounded Slot where
    minBound = Slot 0
    maxBound = Slot maxVal

all :: [Slot]
all = Slot <$> [0 .. maxVal]

evens :: [Slot]
evens = Slot <$> [0, 2 .. maxVal]

odds :: [Slot]
odds = Slot <$> [1, 3 .. maxVal]

-- | Slots with the same parity.
allies :: Slot -> [Slot]
allies x
  | Parity.even x = x `delete` evens
  | otherwise     = x `delete` odds

-- | Slots with opposite parity.
enemies :: Slot -> [Slot]
enemies x
  | Parity.even x = x `delete` odds
  | otherwise     = x `delete` evens

read :: Text -> Either String (Slot, Text)
read s = case Read.decimal s of
    Right (val, _)
      | val < 0 || val > maxVal -> Left "input is out of range"
    x                           -> first Slot <$> x

random :: ∀ m. MonadRandom m => m Slot
random = Slot <$> R.random 0 maxVal
