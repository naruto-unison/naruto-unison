-- | A collection of 'Ninja' → 'Ninja' transformations.
module Game.Ninja where

import qualified Data.Sequence as S

import Data.List
import Data.Maybe
import Data.Sequence (adjust', index)
import Data.Text     (Text)

import Calculus
import Core.Unicode
import Game.Structure
import Game.Functions

addStatus ∷ Status → Ninja → Ninja
addStatus st n@Ninja{..} = n { nStatuses = st' : f nStatuses }
  where f | Nonstacking ∈ statusClasses st' = filter filt
          | otherwise                       = deleteBy del st'
        st' = st { statusClasses = statusF $ statusClasses st }
        filt = not ∘ lEq st
        statusF       | null $ statusBombs st = filter (InvisibleTraps ≠)
                      | otherwise             = map invis
        del a b = lEq a b ∧ statusC a ≠ statusC b 
                ∧ (statusDur a ≡ 1 ∨ statusDur b ≡ 1)
        invis InvisibleTraps = Invisible
        invis a              = a

addStacks ∷ Int → Text → Int → Skill → Slot → Slot → Ninja → Ninja
addStacks dur' l i skill@Skill{..} src c n@Ninja{..} = n 
    { nStatuses = replicate i status ⧺ nStatuses }
  where dur    = copyDur copying ∘ incr $ sync dur'
        status = Status l root src c skill [] (Unremovable : classes) [] dur dur
        root   = cp skill src

addOwnStacks ∷ Int → Text → Int → Int → Ninja → Int → Ninja
addOwnStacks dur' l s v n@Ninja{..} i = addStacks dur' l i skill nId nId n
  where skill = characterSkills nCharacter !! s !! v

alterCd ∷ Int → Int → Int → Ninja → Ninja
alterCd s v cd n@Ninja{..} = n { nCooldowns = adjustCd s v (+ cd) nCooldowns}

attack ∷ Int → Ninja → Ninja
attack hp n@Ninja{..} = n { nHealth = healthBound (minHealth n) $ nHealth - hp }

cancelChannel ∷ Text → Ninja → Ninja
cancelChannel l n@Ninja{..} = n 
    { nChannels = filter ((l ≠) ∘ label ∘ channelSkill) nChannels }  

clear ∷ Text → Slot → Ninja → Ninja
clear l src n@Ninja{..} = n { nStatuses = filter (not ∘ lMatch l src) nStatuses }

clearTrap ∷ Text → Slot → Ninja → Ninja
clearTrap l src n@Ninja{..} = n { nTraps = S.filter (not ∘ lMatch l src) nTraps }

cure ∷ (Effect → Bool) → Ninja → Ninja
cure match n@Ninja{..} = n { nStatuses = mapMaybe (cure' match n) nStatuses }

cure' ∷ (Effect → Bool) → Ninja → Status → Maybe Status
cure' match n status@Status{..}
  | statusSrc ≡ nId n                   = Just status
  | null statusEfs                      = Just status
  | Unremovable ∈ statusClasses         = Just status
  | is Plague n                         = Just status
  | null statusEfs'                     = Nothing
  | otherwise                           = Just status { statusEfs = statusEfs' }
  where statusEfs' = filter keep statusEfs
        keep Reveal = True
        keep a      = helpful a ∨ not (match a)

cureBane ∷ Ninja → Ninja
cureBane n@Ninja{..}
  | is Plague n = n
  | otherwise   = cure cured n { nStatuses = filter uncured nStatuses }
  where cured (Afflict _)  = True
        cured _            = False
        uncured Status{..} = Bane ∉ statusClasses ∨ nId ≡ statusSrc

decrStats ∷ Ninja → Ninja
decrStats n@Ninja{..} = n { nStatuses = expire <$> nStatuses }
  where expire status@Status{..} | statusDur ≡ 1 = status { statusEfs = [] }
                                 | otherwise     = status

decr ∷ Ninja → Ninja
decr n@Ninja{..} = case findMatch nStatuses of
        Just (Snapshot n') → n'
        _ → n { nDefense   = mapMaybe decrTurn nDefense 
              , nStatuses  = mapMaybe decrTurn nStatuses
              , nBarrier   = mapMaybe decrTurn nBarrier
              , nFace      = mapMaybe decrTurn nFace     
              , nChannels  = mapMaybe decrTurn newChans 
                           ⧺ mapMaybe decrTurn nChannels
              , nTags      = mapMaybe decrTurn nTags
              , newChans   = []
              , nTraps     = seqMaybe $ decrTurn        <$> nTraps    
              , nVariants  = mapMaybe   decrTurn        <$> nVariants 
              , nCopied    =        (≫= decrTurn)       <$> nCopied 
              , nCooldowns = ((max 0 ∘ subtract 1) <$>) <$> nCooldowns 
              , nParrying  = []
              }
  where findMatch = find match ∘ reverse ∘ concatMap statusEfs 
                  ∘ filter ((≤ 2) ∘ statusDur)
        match (Snapshot _) = True
        match _            = False

hasten ∷ Int → Text → Slot → Ninja → Ninja
hasten dur l src n@Ninja{..} = n
    { nStatuses = mapMaybe (prolong' (-dur) l src) nStatuses }

kill ∷ Bool → Ninja → Ninja
kill endurable n@Ninja{..}
  | endurable = n { nHealth = minHealth n }
  | otherwise = n { nHealth = 0, nStatuses = preventRes : nStatuses }
  where preventRes = Status "nores" nId nId nId newSkill [Plague] [] [] (-1) (-1)

prolong' ∷ Int → Text → Slot → Status → Maybe Status
prolong' dur l src status@Status{..} 
  | statusDur ≡ 0 ∨ not (lMatch l src status) = Just status
  | statusDur' ≤ 0                            = Nothing
  | otherwise = Just status { statusDur    = statusDur'
                            , statusMaxDur = max statusMaxDur statusDur'
                            }
              -- TODO figure out why the fuck this works
    where dur' | odd statusDur ≡ even dur  = dur
               | dur < 0                   = dur + 1
               | otherwise                 = dur - 1
          statusDur' = statusDur + dur'
 
prolong ∷ Int → Text → Slot → Ninja → Ninja
prolong dur l src n@Ninja{..} = n
    { nStatuses = mapMaybe (prolong' dur l src) nStatuses }
         
purge ∷ Ninja → Ninja
purge n@Ninja{..}
  | is Enrage n = n
  | otherwise   = case find purges nStatuses of
    Nothing → n
    Just st → n { nStatuses = doPurge st : delete st nStatuses }

purgeAll ∷ Ninja → Ninja
purgeAll n@Ninja{..}
  | is Enrage n = n
  | otherwise   = n { nStatuses = map doPurge nStatuses }

refresh ∷ Text → Slot → Ninja → Ninja
refresh l src n@Ninja{..} = n { nStatuses = map f nStatuses }
  where f st@Status{..} | lMatch l src st = st { statusDur = statusMaxDur }
                        | otherwise       = st

removeStack ∷ Text → Ninja → Ninja
removeStack l n@Ninja{..} = n 
    { nStatuses = deleteOne (statusMatch l nId) nStatuses }

removeStacks ∷ Text → Int → Slot → Ninja → Ninja
removeStacks l i src n@Ninja{..} = n { nStatuses = nStatuses ∖ stacks }
  where stacks = take i $ filter (lMatch l src) nStatuses

removeOwn ∷ Text → Ninja → Int → Ninja
removeOwn l n@Ninja{..} _ = clear l nId n

reset ∷ Int → Int → Ninja → Ninja
reset s v n@Ninja{..} = n { nCooldowns = insertCd s v 0 nCooldowns }

resetAll ∷ Ninja → Ninja
resetAll n@Ninja{..} = n { nCooldowns = ø }

resetCharges ∷ Ninja → Ninja
resetCharges n = n { nCharges = four0s }

sacrifice ∷ Int → Int → Ninja → Ninja
sacrifice minh' hp n@Ninja{..} = n { nHealth = healthBound minh $ nHealth - hp }
  where minh = min minh' $ minHealth n

setHealth ∷ Int → Ninja → Ninja
setHealth hp n = n { nHealth = healthBound 0 hp }

unCounter ∷ Ninja → Ninja
unCounter n@Ninja{..} = n { nTraps = S.filter (keep ∘ trapTrigger) nTraps }
  where keep (OnCounter _) = False
        keep _             = True

unVariant ∷ Text → Ninja → Ninja
unVariant l n@Ninja{..} = n { nVariants = f <$> nVariants }
  where f = ensure ∘ filter ((l ≠) ∘ variantL)
        ensure [] = [noVariant]
        ensure a  = a

updateCd ∷ Bool → Int → Skill → Int → Ninja → Ninja
updateCd True a skill s n@Ninja{..} = (updateCd False a skill s n)
                                      { nCharges = adjust' (+ 1) s nCharges }
updateCd False a Skill{..} s n@Ninja{..}
   | copied copying = n
   | cd ≡ 0         = n
   | otherwise      = n { nCooldowns = insertCd s vari cd' nCooldowns }
  where cd'    = sync $ cd + 1 + getSnare n + a
        vari   = variantCD ∘ head $ nVariants `index` s
        copied NotCopied     = False
        copied (Shallow _ _) = True
        copied (Deep _ _)    = False