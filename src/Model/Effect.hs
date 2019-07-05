
module Model.Effect
  ( Amount(..)
  , Constructor(..)
  , Effect(..)
  , construct
  , helpful
  , sticky
  , bypassEnrage
  , boosted
  , identity
  ) where

import ClassyPrelude

import Core.Util (enumerate)
import Model.Internal (Amount(..), Constructor(..), Effect(..), helpful, sticky)

construct :: Constructor -> [Effect]
construct (Only x) = [x]
construct (Any x)  = enumerate x

identity :: ∀ a. Num a => Amount -> a
identity Flat    = 0
identity Percent = 1

-- | Scales the power of an effect.
boosted :: Int -> Effect -> Effect
boosted b (Afflict  x) = Afflict  $ x * b
boosted b (Build    x) = Build    $ x * b
boosted b (Heal     x) = Heal     $ x * b
boosted b (Snare    x) = Snare    $ x * b
boosted b (Unreduce x) = Unreduce $ x * b
boosted b (Bleed      c Flat x) = Bleed      c Flat $ x * b
boosted b (Reduce     c Flat x) = Reduce     c Flat $ x * b
boosted b (Strengthen c Flat x) = Strengthen c Flat $ x * b
boosted b (Weaken     c Flat x) = Weaken     c Flat $ x * b
boosted _ ef = ef

-- | Not canceled by 'Enrage'.
bypassEnrage :: Effect -> Bool
bypassEnrage Exhaust{} = True
bypassEnrage Unexhaust = True
bypassEnrage ef        = helpful ef || sticky ef
