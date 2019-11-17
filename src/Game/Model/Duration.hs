module Game.Model.Duration
  ( Duration(..)
  , throttle
  , incr
  , Turns
  ) where

import ClassyPrelude

import Data.Aeson (ToJSON(..))

import Text.Blaze (ToMarkup(..))
import Text.Read (Read(..))

import Class.Display (Display(..))
import Class.Parity (Parity)

-- | A type synonym that should always be used in "Game.Action" modules to
-- represent time, rather than @Int@ or @Duration@.
-- Wherever it appears, it should immediately be converted to a @Duration@ via
-- view pattern.
type Turns = Int
newtype Duration = Duration { sync :: Turns } deriving (Eq, Parity)

instance Ord Duration where
    0 `compare` 0 = EQ
    0 `compare` _ = GT
    _ `compare` 0 = LT
    x `compare` y = sync x `compare` sync y
    {-# INLINE compare #-}

instance Num Duration where
    (Duration x) + (Duration y) = Duration (x + y)
    {-# INLINE (+) #-}
    (Duration x) - (Duration y) = Duration (x - y)
    {-# INLINE (-) #-}
    (Duration x) * (Duration y) = Duration ( x * y)
    {-# INLINE (*) #-}
    negate (Duration x) = Duration (negate x)
    {-# INLINE negate #-}
    abs (Duration x) = Duration (abs x)
    {-# INLINE abs #-}
    signum (Duration x) = Duration (signum x)
    {-# INLINE signum #-}
    fromInteger (fromInteger -> d)
      | d >= 0    = Duration $ 2 * d
      | otherwise = Duration $ -2 * d - 1
    {-# INLINE fromInteger #-}

unDuration :: Duration -> Turns
unDuration (Duration d)
  | even d    = d `quot` 2
  | otherwise = negate $ 1 + d `quot` 2
{-# INLINE unDuration #-}

instance Show Duration where
    showsPrec i = showsPrec i . unDuration

instance Read Duration where
    readPrec = (fromIntegral :: Int -> Duration) <$> readPrec

instance Display Duration where
    display = display . unDuration

instance ToJSON Duration where
    toJSON = toJSON . unDuration
    {-# INLINE toJSON #-}

instance ToMarkup Duration where
    toMarkup = toMarkup . unDuration

-- | Decreases a duration.
-- Returns Nothing if the duration is reduced to 0.
throttle :: Int -> Duration -> Maybe Duration
throttle 0 dur = Just dur
throttle _ 0   = Just 0
throttle amount (Duration x)
  | x < amount * 2 = Nothing
  | otherwise      = Just . Duration $ x - amount * 2

-- | Increments a @Duration@ unless it is 0.
incr :: Duration -> Duration
incr 0            = 0
incr (Duration x) = Duration (x + 1)
