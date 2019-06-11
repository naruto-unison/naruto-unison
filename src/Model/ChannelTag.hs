module Model.ChannelTag
  ( ChannelTag(..)
  , new
  ) where

import ClassyPrelude.Yesod

import           Model.Internal (ChannelTag(..))
import qualified Model.Copy as Copy
import           Model.Skill (Skill)
import           Model.Slot (Slot)

new :: Skill -> Slot -> ChannelTag
new skill source = ChannelTag { root   = Copy.root skill source
                              , source
                              , skill
                              , ghost  = False
                              , dur    = 3
                              }
