module Class.Parity
  ( Parity(..)
  , allied
  , split
  , allies, enemies
  ) where

import ClassyPrelude.Yesod hiding (even)
import qualified Prelude

split :: ∀ o. (Monoid o, SemiSequence o) => o -> (o, o)
split = foldr (\x ~(xs, ys) -> (x `cons` ys, xs)) (mempty, mempty)

class Parity a where
    even :: a -> Bool

allied :: ∀ a b. (Parity a, Parity b) => a -> b -> Bool
allied x y = even x == even y

allies :: ∀ a o. (Parity a, Monoid o, SemiSequence o) => a -> o -> o
allies (even -> True) = fst . split
allies _              = snd . split

enemies :: ∀ a o. (Parity a, Monoid o, SemiSequence o) => a -> o -> o
enemies (even -> False) = fst . split
enemies _               = snd . split

instance Parity Bool where
    even = id
instance Parity Int where
    even = Prelude.even
instance Parity Int32 where
    even = Prelude.even
instance Parity Int64 where
    even = Prelude.even
instance Parity Integer where
    even = Prelude.even
instance Parity Word where
    even = Prelude.even
instance Parity Word8 where
    even = Prelude.even
instance Parity Word32 where
    even = Prelude.even
instance Parity Word64 where
    even = Prelude.even
