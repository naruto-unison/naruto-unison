module Game.Model.Duration
  ( Duration(..)
  , sync
  , throttle
  ) where

import ClassyPrelude hiding (even)

import Data.Aeson (ToJSON(..))

import Text.Blaze (ToMarkup(..))
import Text.Read
import Text.Read.Lex (numberToInteger)

import Class.Display (Display(..))
import Class.Parity (Parity(..))

-- | A type synonym that should always be used in "Game.Action" modules to
-- represent time, rather than @Int@ or @Duration@.
-- Wherever it appears, it should immediately be converted to a @Duration@ via
-- view pattern.
data Duration
    = Duration Int
    | Permanent
    deriving (Eq)

sync :: Duration -> Int
sync Permanent    = 0
sync (Duration x) = x

instance Parity Duration where
    even Permanent    = True
    even (Duration x) = even x

instance Ord Duration where
    Permanent  `compare` Permanent  = EQ
    Permanent  `compare` Duration _ = GT
    Duration _ `compare` Permanent  = LT
    Duration x `compare` Duration y = x `compare` y
    {-# INLINE compare #-}

instance Enum Duration where
    toEnum d
      | d >= 0    = Duration $ 2 * d
      | otherwise = Duration $ -2 * d - 1
    {-# INLINE toEnum #-}

    fromEnum Permanent = 0
    fromEnum (Duration d)
      | even d    = d `quot` 2
      | otherwise = negate $ 1 + d `quot` 2
    {-# INLINE fromEnum #-}

    pred Permanent    = Permanent
    pred (Duration x) = Duration $ x - 1
    {-# INLINE pred #-}

    succ Permanent    = Permanent
    succ (Duration x) = Duration $ x + 1
    {-# INLINE succ #-}

map1 :: (Int -> Int) -> Duration -> Duration
map1 f (Duration x) = Duration (f x)
map1 _ _            = Permanent
{-# INLINE map1 #-}

map2 :: (Int -> Int -> Int) -> Duration -> Duration -> Duration
map2 f (Duration x) (Duration y) = Duration (f x y)
map2 _ _            _            = Permanent
{-# INLINE map2 #-}

instance Num Duration where
    (+) = map2 (+)
    {-# INLINE (+) #-}
    (-) = map2 (-)
    {-# INLINE (-) #-}
    (*) = map2 (*)
    {-# INLINE (*) #-}
    negate = map1 negate
    {-# INLINE negate #-}
    abs = map1 abs
    {-# INLINE abs #-}
    signum = map1 signum
    {-# INLINE signum #-}
    fromInteger = toEnum . fromInteger
    {-# INLINE fromInteger #-}

instance Show Duration where
    showsPrec i Permanent = showsPrec i ("Permanent" :: String)
    showsPrec i dur       = showsPrec i $ fromEnum dur

instance Read Duration where
    readPrec = parens $ prec 10 do
        lx <- lexP
        case lx of
            String "Permanent"                 -> return Permanent
            Number (numberToInteger -> Just d) -> return $ fromInteger d
            _                                  -> empty

instance Display Duration where
    display Permanent = "Permanent"
    display dur       = display $ fromEnum dur

instance ToJSON Duration where
    toJSON Permanent    = toJSON (0 :: Int)
    toJSON (Duration x) = toJSON x
    {-# INLINE toJSON #-}

instance ToMarkup Duration where
    toMarkup Permanent = "Permanent"
    toMarkup dur       = toMarkup $ fromEnum dur

-- | Decreases a duration.
-- Returns Nothing if the duration is reduced to Permanent.
throttle :: Int -> Duration -> Maybe Duration
throttle 0 dur        = Just dur
throttle _ Permanent = Just Permanent
throttle amount (Duration x)
  | x < amount * 2 = Nothing
  | otherwise      = Just . Duration $ x - amount * 2
