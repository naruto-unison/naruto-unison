module Model
  ( Act(..), actCost
  , Barrier(..)
  , Channel(..), ChannelTag(..)
  , Defense(..)
  , Face(..)
  , Ninja(..)
  , Trap(..)
  , Direction(..)
  , Variant(..)
  ) where

import Prelude
import Data.Maybe (Maybe)
import Generic as G

import Model.Chakra (Chakras)
import Model.Channeling (Channeling)
import Model.Class (Class)
import Model.Skill (Skill(..), Copy)
import Model.Status (Status)
import Model.Slot (Slot)

newtype Act = Act { user    :: Slot
                  , skill   :: Slot
                  , target  :: Slot
                  , skill'  :: Skill
                  , targets :: Array Slot
                  }
instance _eqAct_ :: Eq Act where
    eq (Act x) (Act y) = x.user == y.user
                         && x.skill == y.skill
                         && x.target == y.target
actCost :: Act -> Chakras
actCost (Act {skill': Skill x}) = x.cost

newtype Barrier = Barrier { amount :: Int
                          , source :: Slot
                          , name   :: String
                          , dur    :: Int
                          }


newtype Channel = Channel { root   :: Slot
                          , skill  :: Skill
                          , target :: Slot
                          , dur    :: Channeling
                          }

newtype ChannelTag = ChannelTag { root   :: Slot
                                , source :: Slot
                                , skill  :: Skill
                                , ghost  :: Boolean
                                , dur    :: Int
                                }

newtype Defense = Defense { amount :: Int
                          , source :: Slot
                          , name   :: String
                          , dur    :: Int
                          }

newtype Face = Face { icon   :: String
                    , source :: Slot
                    , dur    :: Int
                    }

newtype Ninja = Ninja { slot      :: Slot
                      , health    :: Int
                      , defense   :: Array Defense
                      , barrier   :: Array Barrier
                      , statuses  :: Array Status
                      , charges   :: Array Int
                      , cooldowns :: Array Int
                      , variants  :: Array (Array Variant)
                      , copies    :: Array (Maybe Copy)
                      , channels  :: Array Channel
                      , traps     :: Array Trap
                      , face      :: Array Face
                      , parrying  :: Array Skill
                      , tags      :: Array ChannelTag
                      , lastSkill :: Maybe Skill
                      , skills    :: Array Skill
                      }

newtype Trap = Trap { direction :: Direction
                    , trigger   :: String
                    , name      :: String
                    , desc      :: String
                    , source    :: Slot
                    , classes   :: Array Class
                    , tracker   :: Int
                    , dur       :: Int
                    }

data Direction
    = To
    | From
    | Per

newtype Variant = Variant { variant :: Int
                          , ownCd   :: Boolean
                          , name    :: String
                          , dur     :: Int
                          }


-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ :: G.Generic Barrier _
instance _1_ :: G.Decode Barrier where
    decode = G.decodeObj
derive instance _3_ :: G.Newtype Barrier _

derive instance _30_ :: G.Generic Channel _
instance _31_ :: G.Decode Channel where
    decode = G.decodeObj
derive instance _33_ :: G.Newtype Channel _

derive instance _50_ :: G.Generic ChannelTag _
instance _51_ :: G.Decode ChannelTag where
    decode = G.decodeObj
derive instance _53_ :: G.Newtype ChannelTag _

derive instance _90_ :: G.Generic Defense _
instance _91_ :: G.Decode Defense where
    decode = G.decodeObj
derive instance _93_ :: G.Newtype Defense _

derive instance _110_ :: G.Generic Face _
instance _111_ :: G.Decode Face where
    decode = G.decodeObj
derive instance _113_ :: G.Newtype Face _

derive instance _140_ :: G.Generic Ninja _
instance _141_ :: G.Decode Ninja where
    decode = G.decodeObj
derive instance _143_ :: G.Newtype Ninja _

derive instance _180_ :: G.Generic Trap _
instance _181_ :: G.Decode Trap where
    decode = G.decodeObj
derive instance _183_ :: G.Newtype Trap _

derive instance _190_ :: G.Generic Direction _
instance _191_ :: G.Decode Direction where
    decode = G.decodeEnum

derive instance _220_ :: G.Generic Variant _
instance _221_ :: G.Decode Variant where
    decode = G.decodeObj
derive instance _223_ :: G.Newtype Variant _

derive instance _253_ :: G.Newtype Act _
