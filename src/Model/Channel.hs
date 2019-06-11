module Model.Channel
  ( Channel(..)
  , disrupts
  , Channeling(..)
  , noInterrupt
  ) where

import ClassyPrelude.Yesod

import Model.Internal (Channel(..), Channeling(..))

disrupts :: Channel -> Bool
disrupts Channel {dur = Control{}} = True
disrupts Channel {dur = Action{}}  = True
disrupts _                         = False

noInterrupt :: Channeling -> Bool
noInterrupt Passive   = True
noInterrupt Ongoing{} = True
noInterrupt _         = False
