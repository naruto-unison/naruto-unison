module Game.Model.ID
    ( ID(..)
    , HasID(..)
    , fromOwner
    , withName
    ) where

import ClassyPrelude

import Game.Model.Slot (Slot)

data ID = ID
    { user  :: Slot
    , owner :: Slot
    , name  :: Text
    } deriving (Eq, Ord, Show, Generic)

instance Hashable ID

class HasID a where
    from :: a -> ID

fromOwner :: ID -> ID
fromOwner someID = someID { user = someID.owner }

withName :: Text -> ID -> ID
withName name someID = someID { name = name }
