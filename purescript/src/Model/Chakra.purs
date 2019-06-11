module Model.Chakra
  ( Chakras(..)
  , fromChakras, fromInt
  , map
  , lack
  , affordable
  , canExchange
  , rate
  , total
  , cost
  ) where

import Prelude hiding (map)
import Data.Array (replicate)
import Data.Foldable (any)
import Data.Newtype as Newtype
import Generic as G
import Halogen.HTML as H
import Halogen.HTML (HTML, ClassName(..))
import Halogen.HTML.Properties as P

newtype Chakras = Chakras { blood :: Int
                          , gen   :: Int
                          , nin   :: Int
                          , tai   :: Int
                          , rand  :: Int
                          }

total :: Chakras -> Int
total (Chakras x) = x.blood + x.gen + x.nin + x.tai

fromInt :: Int -> Chakras
fromInt x = Chakras { blood: x, gen: x, nin: x, tai: x, rand: x }


zip :: (Int -> Int -> Int) -> Chakras -> Chakras -> Chakras
zip f (Chakras x) (Chakras y) = Chakras { blood: f x.blood y.blood
                                        , gen:   f x.gen   y.gen
                                        , nin:   f x.nin   y.nin
                                        , tai:   f x.tai   y.tai
                                        , rand:  f x.rand  y.rand
                                        }

map :: ({ blood :: Int, gen :: Int, nin :: Int, tai :: Int, rand :: Int }
    -> { blood :: Int, gen :: Int, nin :: Int, tai :: Int, rand :: Int })
    -> Chakras
map = flip (Newtype.over Chakras) zero

fromChakras :: Chakras -> Array String
fromChakras (Chakras x) = replicate x.blood "blood"
                       <> replicate x.gen   "gen"
                       <> replicate x.nin   "nin"
                       <> replicate x.tai   "tai"
                       <> replicate x.rand  "rand"

lack :: Chakras -> Boolean
lack (Chakras x) =
    x.blood < 0 || x.gen < 0 || x.nin < 0 || x.tai < 0 || x.rand < 0

rate :: Int
rate = 5

affordable :: Chakras -> Chakras -> Boolean
affordable (Chakras x) (Chakras y) = rate < total x'
  where x' = Chakras { blood: if y.blood /= 0 then 0 else x.blood
                     , gen:   if y.gen   /= 0 then 0 else x.gen
                     , nin:   if y.nin   /= 0 then 0 else x.nin
                     , tai:   if y.tai   /= 0 then 0 else x.tai
                     , rand: 0
                     }

canExchange :: Chakras -> Boolean
canExchange x = any (affordable x) $ map <$>
                [_{ blood = 1 }, _{ gen = 1 }, _{ nin = 1 }, _{ tai = 1 }]

cost :: ∀ a b. Chakras -> Array (HTML a b)
cost = (cost' <$> _) <<< fromChakras
  where cost' s = H.div [P.class_ <<< ClassName $ "chakra " <> s] []

instance _semiringChakras_ :: Semiring Chakras where
    zero = fromInt 0
    one  = fromInt 1
    add = zip (+)
    mul = zip (*)
instance _ringChakras_ :: Ring Chakras where
    sub = zip (-)

derive instance _20_ :: G.Generic Chakras _
instance _21_ :: G.Decode Chakras where
    decode = G.decodeObj
derive instance _22_ :: Eq Chakras
derive instance _23_ :: G.Newtype Chakras _
