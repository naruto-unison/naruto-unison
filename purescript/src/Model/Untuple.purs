module Model.Untuple
  ( Untuple(..)
  , untuple
  ) where

import Prelude
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Foreign as Foreign
import Generic as G

import Model.Slot (Slot)

newtype Untuple a = Untuple a

untuple :: ∀ a. Untuple a -> a
untuple (Untuple x) = x

instance _decodeUntuple_ :: G.Decode a => G.Decode (Untuple a) where
    decode x = do
        arr <- Foreign.readArray x
        case head arr of
            Nothing -> Foreign.fail $ Foreign.ForeignError "Empty array"
            Just head' -> Untuple <$> G.decode head'
