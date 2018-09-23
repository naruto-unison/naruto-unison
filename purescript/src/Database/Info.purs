module Database.Info (Info, infos) where

import StandardLibrary

import Database.Structure

type Info = { name    :: String
            , desc    :: String
            , classes :: Array String
            , dur     :: Int
            , root    :: Int
            , src     :: Int
            , effects :: Array SkillEffect
            , trap    :: Boolean
            , ghost   :: Boolean
            }

infos :: Ninja -> Array Info
infos (Ninja n) = statuses' <> traps
  where statuses  = infoStatus <$> n.nStatuses
        traps     = concatInfo <$> reduce <$> groupBy' eqInfo 
                    (infoTrap <$> n.nTraps)
        statuses' = filter (not <<< any eqInfo traps) statuses
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
    }
  where xs' = x : xs

infoChannel :: Channel -> Info
infoChannel (Channel ch@{channelSkill: Skill s}) =
    { name:    s.label
    , desc:    s.desc
    , classes: s.classes
    , dur:     channelingDur ch.channelDur
    , root:    ch.channelRoot
    , src:     ch.channelRoot
    , effects: []
    , trap:    false
    , ghost:   false
    }

infoChannelTag :: ChannelTag -> Info
infoChannelTag (ChannelTag ct@{tagSkill: Skill s}) =
    { name:    s.label
    , desc:    s.desc
    , classes: s.classes
    , dur:     ct.tagDur
    , root:    ct.tagRoot
    , src:     ct.tagSrc
    , effects: []
    , trap:    false
    , ghost:   true
    }

infoStatus :: Status -> Info
infoStatus (Status st@{statusSkill: Skill s}) =
    { name:    st.statusL
    , desc:    s.desc
    , classes: st.statusClasses
    , dur:     st.statusDur
    , root:    st.statusRoot
    , src:     st.statusSrc
    , effects: st.statusEfs
    , trap:    false
    , ghost:   false
    }
      
infoTrap :: Trap -> Info
infoTrap (Trap t) =
    { name:    t.trapL
    , desc:    t.trapDesc
    , classes: t.trapClasses
    , dur:     t.trapDur
    , root:    t.trapSrc
    , src:     t.trapSrc
    , effects: [toEf]
    , trap:    true
    , ghost:   false
    }
  where 
    toEf = SkillEffect { effectDesc:    t.trapTrigger
                       , effectHelpful: false
                       , effectSticky:  true
                       , effectTrap:    true
                       }
