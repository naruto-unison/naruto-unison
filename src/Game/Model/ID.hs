module Game.Model.ID
    ( ID(..)
    , HasID(..)
    ) where

import ClassyPrelude

import Game.Model.Slot (Slot)

data ID = ID
    { user  :: Slot
    , owner :: Slot
    , name  :: Text
    } deriving (Eq, Ord, Show, Read, Generic)

class HasID a where
    from :: a -> ID
