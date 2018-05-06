module Info (Info, infos) where

import Prelude

import Data.Array  
import Data.Maybe 
import Data.NonEmpty

import Operators
import Structure 

type Info = { name    ∷ String
            , desc    ∷ String
            , classes ∷ Array String
            , dur     ∷ Int
            , root    ∷ Int
            , src     ∷ Int
            , effects ∷ Array Effect
            , trap    ∷ Boolean
            , ghost   ∷ Boolean
            }

infos ∷ Ninja → Array Info
infos (Ninja {nStatuses, nTraps}) = statuses' ⧺ traps
  where statuses  = map infoStatus nStatuses
        traps     = map concatInfo ∘ map reduce ∘ groupBy eqInfo 
                  $ map infoTrap nTraps
        statuses' = filter (not ∘ any eqInfo traps) statuses
        reduce ts@(t :| ts') = case find (eqInfo t) statuses of
            Just match → match :| t : ts'
            Nothing    → ts

eqInfo ∷ Info → Info → Boolean
eqInfo a b = a.desc ≡ b.desc ∧ a.classes ≡ b.classes ∧ a.dur ≡ b.dur

concatInfo ∷ NonEmpty Array Info → Info
concatInfo (x :| xs) = { name:    x.name
                       , desc:    x.desc
                       , classes: x.classes
                       , dur:     x.dur
                       , root:    x.root
                       , src:     x.src
                       , effects: concatMap _.effects xs'
                       , trap:    any (_.trap) xs'
                       , ghost:   all _.ghost xs'
                       }
  where xs' = x : xs

infoChannel ∷ Channel → Info
infoChannel (Channel 
    { channelDur, channelRoot, channelSkill: Skill {classes, desc, label}}) 
    = { name: label
      , desc:    desc
      , classes: classes
      , dur:     channelingDur channelDur
      , root:    channelRoot
      , src:     channelRoot
      , effects: []
      , trap:    false
      , ghost:   false
      }
infoChannelTag ∷ ChannelTag → Info
infoChannelTag (ChannelTag 
    {tagDur, tagRoot, tagSrc, tagSkill: Skill {desc, classes, label}})
    = { name:    label
      , desc:    desc
      , classes: classes
      , dur:     tagDur
      , root:    tagRoot
      , src:     tagSrc
      , effects: []
      , trap:    false
      , ghost:   true
      }

infoStatus ∷ Status → Info
infoStatus (Status 
    { statusClasses, statusDur, statusEfs, statusL, statusRoot, statusSrc 
    , statusSkill: Skill { desc } 
    }) 
    = { name:    statusL
      , desc:    desc
      , classes: statusClasses
      , dur:     statusDur
      , root:    statusRoot
      , src:     statusSrc
      , effects: statusEfs
      , trap:    false
      , ghost:   false
      }
      
infoTrap ∷ Trap → Info
infoTrap (Trap {trapClasses, trapDesc, trapDur, trapL, trapSrc, trapTrigger}) 
    = { name:    trapL
      , desc:    trapDesc
      , classes: trapClasses
      , dur:     trapDur
      , root:    trapSrc
      , src:     trapSrc
      , effects: [toEf trapTrigger]
      , trap:    true
      , ghost:   false
      }
  where toEf trigger = Effect { effectDesc:    trigger
                              , effectHelpful: false
                              , effectSticky:  true
                              , effectTrap:    true
                              }