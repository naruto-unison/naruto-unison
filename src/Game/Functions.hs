-- | Functions for manipulating data structures in "Game.Structure".
module Game.Functions
 ( incr
 , byPar, opponent
 , lost
 , illegal
 , copyDur, copyRoot, cp, statusMatch, numStacks, numActive, matchRequire
 , targetable
 , (••), changeWith, addClass, costPer, setCost, targetAll
 , swapSkill
 , alter
 , isAlive, living, alives, playerTeam, dead, healthBound, minHealth
 , getCds
 , fromChannel, channelSkill, isChanneling
 , hasDefense
 , has, hasOwn, hasTrap, is, is'
 , nEfs, nStats, filterEffects
 , cures, purges, doPurge
 , counter, triggerCopy, parry, redirect, reapply, reflect, snareTrap
 , getTargets, getShare
 , getBless, getBleed, getBoost, getBuild, getImmune, getLink, getReduce
 , getWard, getScale, getSnare, getStrengthen, getWeaken, getStun, getSwap
 , getNet
 , getGameChakra, setGameChakra, (+~), (-~), lack, chakraTotal, χ
 , gainChakra, addChakra, listChakra
 , getSkill, getSkill', getSkills
 , classTrs, getTraps, getPerTraps, getTrackTraps
 , getPlayer, getVs, yieldVictor
 , Ninja (..)
 , Game (..)
 , Character (..)
 , Skill (..)
 ) where

import qualified Data.Sequence as S

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.List
import Data.Sequence  (Seq, (!?))
import Data.Text      (Text)

import Calculus
import Core.Model
import Core.Unicode
import Game.Structure

-- * ARITHMETIC

-- | Adds 1 to positives, subtracts 1 from negatives, and leaves 0s unchanged.
incr ∷ Int → Int
incr x
  | x < 0     = x - 1
  | x > 0     = x + 1
  | otherwise = x

byPar ∷ Int → (a2 → a2) → (a2, a2) → (a2, a2) -- ^ 'par' wrapper of 'do2'
byPar = do2 ∘ even

opponent ∷ Player → Player -- ^ flips 'PlayerA' and 'PlayerB'
opponent PlayerA = PlayerB
opponent PlayerB = PlayerA

lost ∷ Int → Int → Int → Int -- ^ scales by intervals of health lost
lost interval per = (per *) ∘ (÷ interval) ∘ (100 -)

-- * TEAMS

illegal ∷ Player → Act → Bool -- ^ if a 'Ninja' is not on a 'Player''s team
illegal p = (fromEnum p ≠) ∘ spar ∘ actC

-- * NINJAS

alter ∷ (Seq Ninja → Seq Ninja) → Game → Game 
alter f game@Game{..} = game { gameNinjas = f gameNinjas }

isAlive ∷ Ninja → Bool
isAlive = (> 0) ∘ nHealth

living ∷ Game → [Slot]
living = map nId ∘ filter isAlive ∘ toList ∘ gameNinjas

minHealth ∷ Ninja → Int
minHealth n
  | is Endure n = 1
  | otherwise   = 0

playerTeam ∷ Player → Game → [Ninja]
playerTeam p = alliesP p ∘ gameNinjas

alives ∷ Player → Seq Ninja → [Ninja]
alives = (filter isAlive ∘) ∘ alliesP

numAlive ∷ Player → Seq Ninja → Int
numAlive = (length ∘) ∘ alives

dead ∷ Player → Game → Bool -- ^ if a 'Player''s entire team is dead
dead p = all (not ∘ isAlive) ∘ playerTeam p

healthBound ∷ Int → Int → Int
healthBound minhp = min 100 ∘ max minhp

getCd ∷ Variant → Seq Int → Int
getCd = (fromMaybe 0 ∘) ∘ S.lookup ∘ variantCD

getCds ∷ Ninja → Seq Int
getCds Ninja{..} = S.zipWith copyCd nCopied 
                 $ S.zipWith getCd (head <$> nVariants) 
                   nCooldowns
  where isShallow (Shallow _ _) = True
        isShallow _             = False
        copyCd copied cooldown = case copied of
          Just copied' → if | isShallow ∘ copying $ copiedSkill copied' → 0
                            | otherwise → cooldown
          Nothing      → cooldown

-- * CHANNELING

fromChannel ∷ Ninja → Channel → Act
fromChannel Ninja{..} Channel{..} = Act nId (Right channelSkill) channelT

isChanneling ∷ Text → Ninja → Bool
isChanneling l = any ((l ≡) ∘ label ∘ channelSkill) ∘ nChannels

-- * DEFENSE

hasDefense ∷ Text → Slot → Ninja → Bool 
hasDefense l c = any (lMatch l c) ∘ nDefense

-- * STATUSES

selfStat ∷ Ninja → Status → Bool
selfStat Ninja{..} Status{..} = statusSrc ≡ nId ∨ statusC ≡ nId

isCost ∷ Effect → Bool
isCost (Exhaust _) = True
isCost Unexhaust   = True
isCost _           = False

-- | Used by 'getStat' to prevent recursion when checking for 'Enrage' and 'Seal'
rawStat ∷ Ninja → Status → Status
rawStat n@Ninja{..} st
  | selfStat n st = st
  | Enrage ∈ efs  = st { statusEfs = filter enraged         $ statusEfs st }
  | Seal   ∈ efs  = st { statusEfs = filter (not ∘ helpful) $ statusEfs st }
  | otherwise     = st
  where efs = concatMap statusEfs nStatuses
        enraged ef = helpful ef ∨ sticky ef ∨ isCost ef

getStat ∷ Ninja → Status → Status
getStat n@Ninja{..} st = st' { statusEfs = bst ∘ filter keep $ statusEfs st' }
  where st'  = rawStat n st
        efs  = concatMap (statusEfs ∘ rawStat n) nStatuses
        bst  = map (boost $ getBoost (statusSrc st) n)
        keep Enrage       = True
        keep Seal         = True
        keep (Stun _)     = Focus ∉ efs
        keep (Immune _)   = Expose ∉ efs
        keep (Reduce _ _) = Expose ∉ efs
        keep ef | selfStat n st = True
                | Enrage ∈ efs  = helpful ef ∨ sticky ef ∨ isCost ef
                | Seal   ∈ efs  = not $ helpful ef
                | otherwise     = ef ∉ [ nul | Nullify nul ← efs]

nStats ∷ Ninja → [Status]
nStats n@Ninja{..} = map (getStat n) nStatuses

nEfs ∷ Ninja → [Effect]
nEfs = concatMap statusEfs ∘ nStats


has ∷ Text → Slot → Ninja → Bool
has l src = any match ∘ nStatuses
    where match Status{..} = statusL ≡ l 
                           ∧ (statusC ≡ src ∨ statusC ≡ src ∨ statusRoot ≡ src)
                           ∧ (Unshifted ∈ statusClasses ∨ Shifted ∉ statusClasses)

hasTrap ∷ Text → Slot → Ninja → Bool
hasTrap l src = any match ∘ nTraps
    where match Trap{..} = trapL ≡ l ∧ trapSrc ≡ src
                           ∧ (Unshifted ∈ trapClasses ∨ Shifted ∉ trapClasses)

hasOwn ∷ Text → Ninja → Bool
hasOwn l n@Ninja{..} = has l nId n

is ∷ Effect → Ninja → Bool
is ef = (ef ∈) ∘ nEfs

is' ∷ (Class → Effect) → Ninja → Bool
is' efs = (map efs allClasses ⩀) ∘ nEfs

numStacks ∷ Text → Slot → Ninja → Int
numStacks l src = length ∘ filter (lMatch l src) ∘ nStatuses

numActive ∷ Text → Ninja → Int
numActive l n@Ninja{..}
  | stacks > 0         = stacks
  | isChanneling l n   = 1  
  | has l nId n        = 1
  | hasDefense l nId n = 1
  | otherwise          = 0
  where stacks = numStacks l nId n

filterEffects ∷ Ninja → [Effect] → [Effect]
filterEffects n = map adjustEffect ∘ filter keepEffects
  where adjustEffect (Reduce _ a) = Reduce All (a - getUnreduce n)
        adjustEffect f            = f
        keepEffects (Immune _) = not $ is Expose n
        keepEffects _          = True

canPurge ∷ Effect → Bool
canPurge ef = helpful ef ∨ not (sticky ef)

doPurge ∷ Status → Status
doPurge st@Status{..}
  | purges st = st { statusEfs = filter canPurge statusEfs }
  | otherwise = st

purges ∷ Status → Bool
purges Status{..} = Unremovable ∉ statusClasses ∧ any canPurge statusEfs

cures ∷ Status → Bool
cures Status{..} = Unremovable ∉ statusClasses ∧ any match statusEfs
  where match ef = not (helpful ef) ∧ not (sticky ef)

copyDur ∷ Copying → Int → Int
copyDur (Shallow _ d) = absmin d
copyDur (Deep    _ d) = absmin d
copyDur  NotCopied    = id

copyRoot ∷ Copying → Slot → Slot
copyRoot (Shallow a _) = const a
copyRoot (Deep    a _) = const a
copyRoot NotCopied     = id

cp ∷ Skill → Slot → Slot
cp = copyRoot ∘ copying

statusMatch ∷ Text → Slot → Status → Bool
statusMatch l src Status{..} = l ≡ statusL ∧ statusSrc ≡ src

matchRequire ∷ Requirement → Slot → Ninja → Bool
matchRequire Usable     _ _           = True
matchRequire Unusable   _ _           = False
matchRequire (HasU l)   t n@Ninja{..} = t ≡ nId ∨ has l t n ∨ hasTrap l t n
matchRequire (HasI i l) t n@Ninja{..}
  | i > 0     = t ≠ nId ∨ numActive l n ≥ i
  | i < 0     = t ≠ nId ∨ numActive l n < (-i) 
  | otherwise = True

-- | Obtain an 'Effect' and delete it from its owner.
getOne ∷ (Effect → Bool) → Ninja → Maybe (Ninja, Effect, Status)
getOne matches n@Ninja{..} = do
    match         ← find (any matches ∘ statusEfs) nStatuses
    let stats'Del = delete match nStatuses
    case statusEfs match of
        [a] → return (n { nStatuses = stats'Del }, a, match)
        efs → do
            a ← find matches efs
            -- let stats'Add     = match { statusEfs = delete a efs} : stats'Del
            return (n { nStatuses = stats'Del }, a, match)

getOne' ∷ (Effect → Bool) → Ninja → Maybe Ninja
getOne' = (fst3 <$>) ∘ getOne
  where fst3 (Just (n, _, _)) = Just n
        fst3 _                = Nothing

-- | Trigger a 'Counter'.
counter ∷ [Class] → Ninja → Ninja → Maybe Ninja
counter classes n nt
  | nocounter = Just nt
  | isJust $ find (any matchesAll ∘ statusEfs) $ nStatuses nt = Just nt
  | otherwise = getOne' matchesOne nt
  where nocounter = Uncounterable ∉ classes 
                  ∧ any ((OnCounterAll ≡) ∘ trapTrigger) (nTraps n)
        matchesAll (CounterAll Uncounterable) = True
        matchesAll (CounterAll cla) = cla ∈ classes ∧ Uncounterable ∉ classes
        matchesAll _ = False
        matchesOne (Counter Uncounterable) = True
        matchesOne (Counter cla) = cla ∈ classes ∧ Uncounterable ∉ classes
        matchesOne _ = False
        
triggerCopy ∷ [Class] → Ninja → Bool → [(Slot, Text, Int, Int)]
triggerCopy classes Ninja{..} harm = mapMaybe ifCopy allStatuses
  where allStatuses = filter (any matches ∘ statusEfs) nStatuses
        matches (Copy _ cla _ noharm) = (harm ∨ noharm) ∧ cla ∈ classes
        matches _                     = False
        ifCopy Status{..} = case find matches statusEfs of
            Just (Copy dur _ a _) → Just (statusSrc, statusL, a, dur)
            _                     → Nothing

-- | Trigger a 'Reflect'.
reflect ∷ [Class] → Ninja → Ninja → Maybe Ninja
--reflect = counterGen Reflect Unreflectable
reflect clas n nt
  | [Mental, Unreflectable] ⩀ clas = Nothing
  | any ((ReflectAll ∈) ∘ statusEfs) $ nStatuses nt = Just nt
  | any ((OnReflectAll ≡) ∘ trapTrigger) $ nTraps n  = Just nt
  | otherwise = getOne' (Reflect ≡) nt

-- | Trigger a 'Parry'.
parry ∷ Skill → Ninja → Maybe (Ninja, Slot, Int)
parry skill@Skill{..} n@Ninja{..} = case parried of
    Just wasParried → return wasParried
    Nothing → case getOne matchesOne n of
            Just (n'', Parry _ a, st) → Just (n'', statusC st, a)
            _                        → Nothing
    where n' = n { nParrying = skill : nParrying }
          matchesAll (ParryAll Uncounterable _) = True
          matchesAll (ParryAll cla _) = cla ∈ classes ∧ Uncounterable ∉ classes
          matchesAll _ = False
          matchesOne (Parry Uncounterable _) = True
          matchesOne (Parry cla _) = cla ∈ classes ∧ Uncounterable ∉ classes
          matchesOne _ = False
          parried = do
              status         ← find (any matchesAll ∘ statusEfs) nStatuses
              (ParryAll _ a) ← find matchesAll $ statusEfs status
              return (n', statusC status, a)

reapply ∷ [Class] → Ninja → Maybe Slot
reapply classes Ninja{..}
  | Unreflectable ∈ classes = Nothing
  | otherwise = statusC <$> find ((Reapply ∈) ∘ statusEfs) nStatuses

redirect ∷ [Class] → Ninja → Maybe Slot
redirect classes Ninja{..}
  | Unreflectable ∈ classes = Nothing
  | otherwise = statusC <$> find (any match ∘ statusEfs) nStatuses
  where match (Redirect cla) = cla ∈ classes
        match _              = False
-- | Trigger a 'SnareTrap'.

-- | Trigger a 'Parry'.
snareTrap ∷ Skill → Ninja → Maybe (Ninja, Int)
snareTrap Skill{..} n@Ninja{..} = case getOne matchesOne n of
    Just (n', SnareTrap _ a, _) → Just (n', a)
    _                           → Nothing
    where matchesOne (SnareTrap cla _) = cla ∈ classes
          matchesOne _                 = False

getTargets ∷ Effect → Ninja → [Slot]
getTargets ef n = [statusC st | st ← nStats n, ef ∈ statusEfs st]

getShare ∷ Ninja → [Slot]
getShare n@Ninja{..} = filter (nId ≠) ∘ map statusC 
                     ∘ filter ((Share ∈) ∘ statusEfs) $ nStats n

-- ** LIST COMPREHENSIONS, MOTHERFUCKERS
getBleed             ∷ [Class] → Ninja → Int
getBleed clas n      = sum [bleed  | Bleed cla bleed    ← nEfs n, cla ∈ clas]
getBless             ∷ Ninja → Int
getBless n           = sum [bless  | Bless bless        ← nEfs n]
getBuild             ∷ Ninja → Int
getBuild n           = sum [build  | Build build        ← nEfs n]
getImmune            ∷ Ninja → [Class]
getImmune n          =     [clas   | Immune clas        ← nEfs n]
getSnare             ∷ Ninja → Int
getSnare n           = sum [snare  | Snare snare        ← nEfs n]
getStun              ∷ Ninja → [Class]
getStun n            =     [cla    | Stun cla           ← nEfs n]
getUnreduce          ∷ Ninja → Int
getUnreduce n        = sum [unred  | Unreduce unred     ← nEfs n]
getWeaken            ∷ [Class] → Ninja → Int
getWeaken clas n     = sum [weak   | Weaken cla weak    ← nEfs n, cla ∈ clas]
getStrengthen        ∷ [Class] → Ninja → Int
getStrengthen clas n = sum [str    | Strengthen cla str ← nEfs n, cla ∈ clas]
getLink              ∷ Slot → Ninja → Int
getLink c n          = sum [link   | Link link          ← nEfs']
  where nEfs' = concatMap statusEfs $ filter ((c ≡) ∘ statusC) $ nStats n

getBoost             ∷ Slot → Ninja → Int
getBoost c Ninja{..}
  | c ≡ nId          = 1
  | otherwise        = product $ 1 : [boo    | Boost boo          ← nEfs']
  where nEfs' = concatMap statusEfs $ filter ((c ≡) ∘ statusC) nStatuses

getScale ∷ [Class] → Ninja → Rational
getScale clas n = product $ 1 : [ scale | Scale cla scale ← nEfs n
                                        , cla ∈ clas 
                                        , scale ≥ 1 ∨ Affliction ∉ clas
                                        ]

getReduce ∷ [Class] → Ninja → Int
getReduce [Affliction] n = sum [reduce | Reduce Affliction reduce ← nEfs n]
getReduce clas n = sum [reduce | Reduce cla reduce ← nEfs n, cla ∈ clas]

getWard ∷ [Class] → Ninja → Rational
getWard [Affliction] n = product
                         $ 1 : [1 - ward | Ward Affliction ward ← nEfs n]
getWard clas n = product $ 1 : [1 - ward | Ward cla ward ← nEfs n, cla ∈ clas]

getSwap ∷ [Class] → Ninja → Maybe Status
getSwap clas n
  | Unreflectable ∈ clas = Nothing
  | otherwise            = find (any matches ∘ statusEfs) $ nStats n
  where matches (Swap cla) = cla ∈ clas
        matches _          = False

afflict1 ∷ Player → Game → Slot → Status → Int
afflict1 player game t Status{..}
    | summed ≠ 0 ∧ alliedP player statusSrc = summed + ext
    | otherwise                             = 0
  where nt = gameNinja t game
        n  = gameNinja statusSrc game
        summed = sum [afflict | Afflict afflict ← statusEfs]
        ext | t ≡ statusSrc          = 0
            | is (Stun Affliction) n = 0
            | otherwise = getStrengthen [Affliction, All] n
                        + getBleed      [Affliction, All] nt
                        + getLink       statusSrc         nt

heal1 ∷ Player → Game → Ninja → Status → Int
heal1 player game n Status{..}
  | summed ≠ 0 ∧ alliedP player statusSrc = getBoost statusSrc n * summed 
                                          + getBless (gameNinja statusSrc game)
  | otherwise                             = 0
  where summed = sum [heal | Heal heal ← statusEfs]

getAfflict ∷ Player → Game → Ninja → Int
getAfflict player game n@Ninja{..} = sum ∘ map (afflict1 player game nId) 
                                   ∘ is ImmuneSelf n 
                                   ? filter ((≠ nId) ∘ statusSrc) 
                                   $ nStats n

getHeal ∷ Player → Game → Ninja → Int
getHeal player game n@Ninja{..}
  | is Plague n = 0
  | otherwise   = sum ∘ map (heal1 player game n) $ nStats n

getNet ∷ Player → Game → Ninja → Int -- ^ afflicts - heals
getNet player game n = getAfflict player game n - getHeal player game n

getExhaust ∷ [Class] → Ninja → Chakras
getExhaust classes n = χØ { rand = length ∘ filter fx $ nEfs n }
  where fx (Exhaust cla) = cla ∈ classes
        fx _             = False

-- * CHAKRA

getGameChakra ∷ Player → Game → Chakras
getGameChakra player = out2 (PlayerA ≡ player) ∘ gameChakra
setGameChakra ∷ Player → Chakras → Game → Game
setGameChakra player chakra game@Game{..} = game
    { gameChakra = in2 (PlayerA ≡ player) chakra gameChakra }

infixl 6 +~
(+~) ∷ Chakras → Chakras → Chakras
(Chakras b g n t r) +~ (Chakras b' g' n' t' r')
    = Chakras (b + b') (g + g') (n + n') (t + t') (r + r')

infixl 6 -~
(-~) ∷ Chakras → Chakras → Chakras
(Chakras b g n t r) -~ (Chakras b' g' n' t' r')
    = Chakras (b - b') (g - g') (n - n') (t - t') (r - r')

infixl 7 *~
(*~) ∷ Chakras → Int → Chakras
(Chakras b g n t r) *~ a = Chakras (b * a) (g * a) (n * a) (t * a) (r * a)

χ ∷ [ChakraType] → Chakras
χ = foldl' (+~) χØ ∘ map toChak
  where toChak Blood = Chakras 1 0 0 0 0
        toChak Gen   = Chakras 0 1 0 0 0
        toChak Nin   = Chakras 0 0 1 0 0
        toChak Tai   = Chakras 0 0 0 1 0
        toChak Rand  = Chakras 0 0 0 0 1

lack ∷ Chakras → Bool
lack (Chakras b g n t r) = b < 0 ∨ g < 0 ∨ n < 0 ∨ t < 0 ∨ r < 0

chakraTotal ∷ Chakras → Int
chakraTotal (Chakras b g n t r) = b + g + n + t + r

gainChakra ∷ Player → [Int] → Seq Ninja → Chakras → Chakras
gainChakra player gain ns chakras = foldl' (addChakra 1) chakras 
                                  $ take (numAlive player ns) gain

addChakra ∷ Int → Chakras → Int → Chakras
addChakra m ch 0 = ch { blood = m + blood ch }
addChakra m ch 1 = ch { gen   = m + gen   ch }
addChakra m ch 2 = ch { nin   = m + nin   ch }
addChakra m ch 3 = ch { tai   = m + tai   ch }
addChakra _ ch _ = ch

listChakra ∷ Chakras → [Int]
listChakra (Chakras b g n t _) = replicate b 0
                               ⧺ replicate g 1
                               ⧺ replicate n 2
                               ⧺ replicate t 3

-- * RETRIEVAL

chakraClasses ∷ Skill → Skill
chakraClasses skill@Skill{..} = skill { classes = f classes }
  where Chakras b g n t r = cost
        f                 = (b > 0) ? (Bloodline :) 
                          ∘ (g > 0) ? (Genjutsu  :) 
                          ∘ (n > 0) ? (Ninjutsu  :) 
                          ∘ (t > 0) ? (Taijutsu  :)
                          ∘ (r > 0) ? (Random    :)

changeSkill ∷ Ninja → Skill → Skill
changeSkill n skill = skill' 
    { cost = getExhaust (classes skill') n +~ cost skill' }
  where skill' = chakraClasses $ changes skill n skill

infixl 1 ••
(••) ∷ (Ninja → Skill → Skill) → (Ninja → Skill → Skill) → Ninja → Skill → Skill
(f •• g) n skill = g n $ f n skill
{-# INLINE (••) #-}

changeWith ∷ Text → (Ninja → Skill → Skill) → Ninja → Skill → Skill
changeWith l f n@Ninja{..}
  | hasOwn l n ∨ hasDefense l nId n = f n
  | otherwise                       = id

addClass ∷ Class → Ninja → Skill → Skill
addClass cla _ skill@Skill{..} = skill { classes = cla : classes }

costPer ∷ Text → [ChakraType] → Ninja → Skill → Skill
costPer l chaks n skill@Skill{..} = skill 
    { cost = cost +~ χ chaks *~ numActive l n }

setCost ∷ [ChakraType] → Ninja → Skill → Skill
setCost chaks _ skill = skill { cost = χ chaks }

targetAll ∷ Ninja → Skill → Skill
targetAll _ skill@Skill{..} = skill
  { start = map f start, effects = map f effects, disrupt = map f disrupt }
  where f (targ, ef) = (f' targ, ef)
        f' Enemy = Enemies
        f' Ally  = Allies
        f' XAlly = XAllies
        f' a     = a

swapSkill ∷ Status → Skill → Skill
swapSkill Status{..} skill@Skill{..} = skill { effects = map f' effects 
                                             , start   = map f' start
                                             , disrupt = map f' disrupt
                                             }
  where f' (a, b) = (f a, b)
        f Self         = Self
        f Ally         = Specific statusSrc
        f XAlly        = Specific statusSrc
        f RAlly        = REnemy
        f Allies       = Enemies
        f XAllies      = Enemies
        f Enemy        = Self
        f REnemy       = RAlly
        f Enemies      = Allies
        f XEnemies     = XAllies
        f Everyone     = Everyone
        f (Specific a) = Specific a

restrict ∷ Ninja → Skill → Skill
restrict n skill@Skill{..}
  | is Restrict n = skill { effects = mapMaybe f effects 
                          , start   = mapMaybe f start
                          , disrupt = mapMaybe f disrupt
                          }
  | otherwise     = skill
  where f (XEnemies, _)  = Nothing
        f (REnemy,   _)  = Nothing
        f (Everyone, ef) = Just (Allies, ef)
        f (Enemies, ef)  = Just (Enemy, ef)
        f a              = Just a

usable ∷ Ninja → Maybe Int → Skill → Skill
usable n@Ninja{..} s skill@Skill{..}
  | charges > 0 ∧ maybe False (≥ charges) ((nCharges !?) =≪ s) = unusable
  | maybe False (>0) $ (getCds n !?) =≪ s = unusable
  | is Focus n                            = skill'
  | isNothing s ∧ nointerrupt             = skill'
  | classes ⩀ getStun n                   = unusable
  | isNothing s                           = skill'
  | Single ∉ classes                      = skill'
  | has label nId n                       = unusable
  | isChanneling label n                  = unusable
  | hasDefense label nId n                = unusable
  | otherwise                             = skill'
  where nointerrupt = channel ≡ Passive ∨ isOngoing channel
        unusable    = skill { require = Unusable }
        skill'      = case require of
          req@(HasI _ _) → if isNothing s ∨ matchRequire req nId n 
                           then skill { require = Usable }
                           else skill { require = Unusable }
          _              → skill

targetable ∷ Skill → Ninja → Ninja → Ninja → Bool
targetable Skill{..} nSrc n nt
  | not $ matchRequire require src nt           = False
  | not (isAlive nt) ∧ Necromancy ∉ classes     = False
  | isAlive nt ∧ src ≠ t ∧ Necromancy ∈ classes = False
  | Bypassing ∈ classes                         = True
  | harm ∧ (classes ⩀ getImmune nt)             = False
  | src ≠ t ∧ not harm ∧ is Seal nt             = False
  | c ≠ t ∧ (is Isolate n ∨ dueling ∨ taunted)  = False
  | t ∈ getTargets Block n                      = False
  | otherwise                                   = True
  where src     = nId nSrc
        c       = nId n
        t       = nId nt
        harm    = not $ allied c t ∧ allied src t
        dueling = notIn c $ getTargets Duel nt
        taunted = notIn t $ getTargets Taunt n
        notIn a xs = not (null xs) ∧ a ∉ xs

getSkill' ∷ Ninja → Int → Int → Skill
getSkill' n@Ninja{..} s v = restrict n ∘ changeSkill n 
                          $ characterSkills nCharacter !! s !! v

getSkill ∷ Ninja → Either Int Skill → Skill
getSkill n      (Right skill) = usable n Nothing skill
getSkill n@Ninja{..} (Left s) = usable n (Just s) ∘ maybe def copiedSkill ∘ join 
                              $ S.lookup s nCopied
    where def = getSkill' n s (if s > 3 then 0 else getVar s n)

getVar ∷ Int → Ninja → Int
getVar s = maybe 0 (variantV ∘ head) ∘ S.lookup s ∘ nVariants

getSkills ∷ Ninja → [Skill]
getSkills n = map (getSkill n ∘ Left) [0..3]

getTraps ∷ Bool → Trigger → Ninja → Seq TrapTransform
getTraps False _ _ = ø
getTraps True tr Ninja{..} = trapEf <$> S.filter ((tr ≡) ∘ trapTrigger) nTraps

classTrs ∷ Bool → (Class → Trigger) → [Class] → Ninja → Seq TrapTransform
classTrs False _  _       _     = ø
classTrs True  tr classes n = seqConcat $ map classTrigger classes
    where classTrigger cla = getTraps True (tr cla) n
      
getPerTraps ∷ Bool → Trigger → Int → Ninja → Seq (Game → Game)
getPerTraps False _  _      _         = ø
getPerTraps True  tr amount Ninja{..} = traps ◇ hooks
  where traps = trapEf' <$> S.filter ((tr ≡) ∘ trapTrigger) nTraps
        hooks = S.fromList ∘ map (hookEf ∘ snd) ∘ filter ((tr ≡) ∘ fst) 
              $ characterHooks nCharacter
        trapEf' Trap{..} = trapEf trapTrack nId
        hookEf f = fn nId $ (flip f) amount

getTrackTraps ∷ Bool → Trigger → Ninja → Seq (Game → Game)
getTrackTraps False _ _ = ø
getTrackTraps True tr Ninja{..} = trapEf' <$> S.filter match nTraps
  where trapEf' Trap{..} = trapEf trapTrack nId
        match Trap{..} = trapTrigger ≡ tr ∧ trapDur ≤ 2 ∧ trapTrack > 0

-- MAYBES

getPlayer ∷ Key User → Game → Maybe Player
getPlayer who Game{gamePlayers = (pA, pB)}
  | who ≡ pA  = Just PlayerA
  | who ≡ pB  = Just PlayerB
  | otherwise = Nothing

getVs ∷ Key User → Game → Maybe (Key User)
getVs who game@Game{..} = do
    player ← getPlayer who game
    return $ out2 (player ≡ PlayerB) gamePlayers

yieldVictor ∷ Game → Game
yieldVictor game@Game{..}
  | isJust gameVictor = game
  | deadA ∧ deadB     = game { gameVictor = Just Tie }
  | deadA             = game { gameVictor = Just VictorB }
  | deadB             = game { gameVictor = Just VictorA }
  | otherwise         = game
  where deadA  = dead PlayerA game
        deadB  = dead PlayerB game
