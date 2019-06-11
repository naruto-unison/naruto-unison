module Model.Channeling
  ( Channeling(..)
  , dur
  ) where

import Generic as G

data Channeling
    = Instant
    | Passive
    | Action  Int
    | Control Int
    | Ongoing Int

dur :: Channeling -> Int
dur Instant     = 1
dur Passive     = 1
dur (Action x)  = x
dur (Control x) = x
dur (Ongoing x) = x

derive instance _40_ :: G.Generic Channeling _
instance _41_ :: G.Decode Channeling where
    decode = G.decodeObj
