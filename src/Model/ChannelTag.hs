module Model.ChannelTag
  ( ChannelTag(..)
  , new
  ) where

import ClassyPrelude

import           Model.Internal (ChannelTag(..))
import qualified Model.Copy as Copy
import           Model.Skill (Skill)
import           Model.Slot (Slot)

new :: Skill -> Slot -> ChannelTag
new skill user = ChannelTag { source   = Copy.source skill user
                            , user
                            , skill
                            , ghost  = False
                            , dur    = 3
                            }
