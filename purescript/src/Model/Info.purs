module Model.Info (Info, infos) where

import Prelude
import Data.Foldable (any, all, find, notElem, sum)
import Data.Array ((:), filter, partition, replicate)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))

import Model (Channel(..), ChannelTag(..), Ninja(..), Trap(..))
import Model.Channeling as Channeling
import Model.Status (Effect(..), Status(..))
import Model.Skill (Skill(..))
import Util (groupBy')

type Info = { name    :: String
            , desc    :: String
            , classes :: Array String
            , dur     :: Int
            , root    :: Int
            , src     :: Int
            , effects :: Array Effect
            , trap    :: Boolean
            , ghost   :: Boolean
            , count   :: Int
            }

infos :: Ninja -> Array Info
infos (Ninja n) = organize <<< partition (eq n.slot <<< _.src) $
                  statuses' <> traps
  where organize {yes, no} = no <> yes
        statuses  = infoStatus <$> n.statuses
        traps     = concatInfo <$> reduce <$> groupBy' eqInfo
                    (infoTrap <$> n.traps)
        statuses' = filter (not <<< any eqInfo traps) statuses >>= unfoldStat
        unfoldStat st
          | st.count <= 1 || "Resource" `notElem` st.classes = [st]
          | otherwise = replicate st.count st { count = 1 }
        reduce ts@(t :| ts') = case find (eqInfo t) statuses of
            Just match -> match :| t : ts'
            Nothing    -> ts

eqInfo :: Info -> Info -> Boolean
eqInfo x y = x.desc == y.desc && x.classes == y.classes && x.dur == y.dur

concatInfo :: NonEmpty Array Info -> Info
concatInfo (x :| xs) =
    { name:    x.name
    , desc:    x.desc
    , classes: x.classes
    , dur:     x.dur
    , root:    x.root
    , src:     x.src
    , effects: xs' >>= _.effects
    , trap:    any (_.trap) xs'
    , ghost:   all _.ghost xs'
    , count:   sum $ _.count <$> xs'
    }
  where xs' = x : xs

infoChannel :: Channel -> Info
infoChannel (Channel chan@{skill: Skill s}) =
    { name:    s.name
    , desc:    s.desc
    , classes: s.classes
    , dur:     Channeling.dur chan.dur
    , root:    chan.root
    , src:     chan.root
    , effects: []
    , trap:    false
    , ghost:   false
    , count:   1
    }

infoChannelTag :: ChannelTag -> Info
infoChannelTag (ChannelTag ct@{skill: Skill s}) =
    { name:    s.name
    , desc:    s.desc
    , classes: s.classes
    , dur:     ct.dur
    , root:    ct.root
    , src:     ct.source
    , effects: []
    , trap:    false
    , ghost:   true
    , count:   1
    }

infoStatus :: Status -> Info
infoStatus (Status st@{skill: Skill s}) =
    { name:    st.name
    , desc:    s.desc
    , classes: st.classes
    , dur:     st.dur
    , root:    st.root
    , src:     st.source
    , effects: st.effects
    , trap:    false
    , ghost:   false
    , count:   st.amount
    }

infoTrap :: Trap -> Info
infoTrap (Trap trap) =
    { name:    trap.name
    , desc:    trap.desc
    , classes: trap.classes
    , dur:     trap.dur
    , root:    trap.source
    , src:     trap.source
    , effects: [toEf]
    , trap:    true
    , ghost:   false
    , count:   1
    }
  where
    toEf = Effect { desc:    trap.trigger
                  , helpful: false
                  , sticky:  true
                  , trap:    true
                  }
