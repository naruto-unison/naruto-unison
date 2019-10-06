-- DO NOT EXPOSE ANY FUNCTION THAT COULD BE USED TO CONSTRUCT OR ALTER A SLOT.
-- It must be guaranteed that all Slots are within their 'Bounded' range.
module Game.Model.Slot
  ( Slot, toInt, read
  , teamSize
  , all, allies, enemies
  , random
  ) where

import ClassyPrelude hiding (all)

import           Data.Aeson (ToJSON)
import qualified Data.Text.Read as Read
import           Text.Read hiding (read)
import           Text.Read.Lex (numberToInteger)

import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Class.Random (MonadRandom)
import qualified Class.Random as R

teamSize :: Int
teamSize = 3

maxVal :: Int
maxVal = teamSize * 2 - 1

-- | A @Slot@ represents an index in 'Model.Game.ninjas'.
-- It is hidden behind a newtype and cannot be constructed or modified outside
-- this module in order to prevent out-of-bound errors.
-- This has the added advantage of making function signatures more readable!
newtype Slot = Slot { toInt :: Int } deriving (Eq, Ord, Show, ToJSON)

instance Parity Slot where
    even (Slot x) = x < teamSize
    {-# INLINE even #-}

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

-- | Slots with the same parity.
allies :: ∀ a. Parity a => a -> [Slot]
allies x
  | Parity.even x = Slot <$> [0 .. teamSize - 1]
  | otherwise     = Slot <$> [teamSize .. maxVal]

-- | Slots with opposite parity.
enemies :: ∀ a. Parity a => a -> [Slot]
enemies x
  | Parity.even x = Slot <$> [teamSize .. maxVal]
  | otherwise     = Slot <$> [0 .. teamSize - 1]

read :: Text -> Either String (Slot, Text)
read s = case Read.decimal s of
    Right (val, _)
      | val < 0 || val > maxVal -> Left "input is out of range"
    x                           -> first Slot <$> x

random :: ∀ m. MonadRandom m => m Slot
random = Slot <$> R.random 0 maxVal
