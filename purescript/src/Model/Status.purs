module Model.Status
  ( Status(..)
  , Bomb(..)
  , Effect(..)
  , removable
  ) where

import Prelude
import Generic as G

import Model.Untuple (Untuple)
import Model.Class (Class)
import Model.Skill (Skill)
import Model.Slot (Slot)

newtype Status = Status { amount  :: Int
                        , name    :: String
                        , root    :: Slot
                        , source  :: Slot
                        , user    :: Slot
                        , skill   :: Skill
                        , effects :: Array Effect
                        , classes :: Array Class
                        , bombs   :: Array (Untuple Bomb)
                        , maxDur  :: Int
                        , dur     :: Int
                        }


data Bomb
    = Done
    | Expire
    | Remove

newtype Effect = Effect { desc    :: String
                        , helpful :: Boolean
                        , sticky  :: Boolean
                        , trap    :: Boolean
                        }

removable :: Boolean -> Effect -> Boolean
removable onAlly (Effect ef) = not ef.sticky && onAlly /= ef.helpful

derive instance _100_ :: G.Generic Effect _
instance _101_ :: G.Decode Effect where
    decode = G.decodeObj
derive instance _103_ :: G.Newtype Effect _

derive instance _230_ :: G.Generic Status _
instance _231_ :: G.Decode Status where
    decode = G.decodeObj
derive instance _233_ :: G.Newtype Status _

derive instance _10_ :: G.Generic Bomb _
instance _11_ :: G.Decode Bomb where
    decode = G.decodeEnum
