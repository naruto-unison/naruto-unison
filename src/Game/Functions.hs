-- | Functions for manipulating data in "Game.Structure".
--
-- @'Text' -> 'Slot'@ arguments check for 'lMatch'.
module Game.Functions
    ( 
    -- * Arithmetic
      incr
    , byPar, opponent
    , lost
    , illegal
    -- * 'Chakras'
    , (-~), getGameChakra, setGameChakra, lack, χ, unχ, χGain, χTotal
    -- * 'Channel'
    , fromChannel, isChanneling
    -- * 'Defense'
    , hasDefense
    -- * 'Effect'
    , filterEffects
    -- ** Reduction of all 'Effect's on a 'Ninja'
    , getBoost, getBleed, getBless, getBuild, getImmune, getLink, getNet
    , getReduce, getScale, getShare, getSnare, getStrengthen, getStun, getWard
    , getWeaken
    -- * 'Ninja'
    , alter, getCds
    -- ** Life and death
    , alives, dead, healthBound, isAlive, minHealth
    -- * 'Player'
    , getPlayer, getVs, yieldVictor
    -- * 'Skill'
    , chakraClasses, defaultL
    -- ** 'Copied'
    , copyDur, copyRoot
    -- ** Modification
    , getSkill, getSkill', getSkills
    -- *** 'changes'
    , (••)
    , addClass, changeWith, costPer, restrict, setCost, swapSkill, targetAll
    -- ** 'Requirement'
    , matchRequire
    -- * 'Status'
    , has, hasOwn, is, is', numActive, numStacks
    -- * 'Trap'
    , classTrs, getPerTraps, getTrackTraps, getTraps
    -- * Triggering 'Effect's
    , copy, counter, parry, reapply, redirect, reflect, snareTrap, swap
    -- * Usability
    , targetable, usable
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

-- | Applies a function to the first in a pair if odd or the second if even.
byPar ∷ Int → (a → a) → (a, a) → (a, a)
byPar = do2 ∘ even

-- | Flips 'PlayerA' and 'PlayerB'.
opponent ∷ Player → Player
opponent PlayerA = PlayerB
opponent PlayerB = PlayerA

-- | Scales an amount by health lost, in specified intervals.
lost ∷ Int -- ^ Interval
     → Int -- ^ Amount per interval
     → Int -- ^ Health lost
     → Int -- ^ Scaled amount
lost interval per = (per *) ∘ (÷ interval) ∘ (100 -)

-- A 'Player' attempts to control a 'Ninja' not on their team.
illegal ∷ Player → Act → Bool
illegal p = (fromEnum p ≠) ∘ spar ∘ actC

-- * CHAKRAS

-- | Product.
infixl 7 *~
(*~) ∷ Chakras → Int → Chakras
(Chakras b g n t r) *~ a = Chakras (b * a) (g * a) (n * a) (t * a) (r * a)

-- | Subtraction.
infixl 6 -~
(-~) ∷ Chakras → Chakras → Chakras
(-~) a = (a ⧺) ∘ (*~ (-1))

getGameChakra ∷ Player → Game → Chakras
getGameChakra player = out2 (PlayerA ≡ player) ∘ gameChakra
setGameChakra ∷ Player → Chakras → Game → Game
setGameChakra player chakra game@Game{..} = game
    { gameChakra = in2 (PlayerA ≡ player) chakra gameChakra }

-- Any component is negative.
lack ∷ Chakras → Bool
lack (Chakras b g n t r) = b < 0 ∨ g < 0 ∨ n < 0 ∨ t < 0 ∨ r < 0

χ ∷ (Foldable a, Functor a) ⇒ a ChakraType → Chakras
χ = catMap toChak
  where toChak Blood = Chakras 1 0 0 0 0
        toChak Gen   = Chakras 0 1 0 0 0
        toChak Nin   = Chakras 0 0 1 0 0
        toChak Tai   = Chakras 0 0 0 1 0
        toChak Rand  = Chakras 0 0 0 0 1

unχ ∷ Chakras → [ChakraType]
unχ (Chakras b g n t _) = replicate b Blood
                        ⧺ replicate g Gen
                        ⧺ replicate n Nin
                        ⧺ replicate t Tai

-- | Adds 1 chakra per living 'Ninja' on the team of a 'Player'.
χGain ∷ Player → [ChakraType] → Seq Ninja → Chakras → Chakras
χGain player gain ns = (⧺ χ (take (numAlive player ns) gain))

-- | Sum of components.
χTotal ∷ Chakras → Int
χTotal (Chakras b g n t r) = b + g + n + t + r

-- * CHANNELS

fromChannel ∷ Ninja → Channel → Act
fromChannel Ninja{..} Channel{..} = Act nId (Right channelSkill) channelT

isChanneling ∷ Text → Ninja → Bool
isChanneling l = any ((l ≡) ∘ label ∘ channelSkill) ∘ nChannels

-- * DEFENSE

hasDefense ∷ Text → Slot → Ninja → Bool 
hasDefense l c = any (lMatch l c) ∘ nDefense

-- ** EFFECTS

-- | Modifies 'Effect's when they are first added to a 'Ninja'.
filterEffects ∷ MonadPlus m ⇒ Ninja → m Effect → m Effect
filterEffects n = adjustEffect ↤∘ mfilter keepEffects
  where adjustEffect (Reduce cla a) = Reduce cla (a - getUnreduce n)
        adjustEffect f              = f
        keepEffects (Immune _)      = not $ is Expose n
        keepEffects _               = True

-- ** REDUCING EFFECTS

getTargets ∷ Effect → Ninja → [Slot]
getTargets ef n = [statusC | Status{..} ← nStats n, ef ∈ statusEfs ]

-- | 'Share's.
getShare ∷ Ninja → [Slot]
getShare n@Ninja{..} = [ statusC | Status{..} ← nStats n
                                 , nId ≠ statusC
                                 , Share ∈ statusEfs 
                                 ]

-- LIST COMPREHENSIONS, MOTHERFUCKERS
-- | 'Bleed' sum.
getBleed             ∷ [Class] → Ninja → Int
getBleed clas n      = sum [ bleed  | Bleed cla bleed    ← nEfs n, cla ∈ clas ]
-- | 'Bless' sum.
getBless             ∷ Ninja → Int
getBless n           = sum [ bless  | Bless bless        ← nEfs n ]
-- | 'Build' sum.
getBuild             ∷ Ninja → Int
getBuild n           = sum [ build  | Build build        ← nEfs n ]
-- | 'Immune's.
getImmune            ∷ Ninja → [Class]
getImmune n          =     [ clas   | Immune clas        ← nEfs n ]
-- | 'Snare' sum.
getSnare             ∷ Ninja → Int
getSnare n           = sum [ snare  | Snare snare        ← nEfs n ]
-- | 'Stun's.
getStun              ∷ Ninja → [Class]
getStun n            =     [ cla    | Stun cla           ← nEfs n ]
-- | 'Unreduce' sum.
getUnreduce          ∷ Ninja → Int
getUnreduce n        = sum [ unred  | Unreduce unred     ← nEfs n ]
-- | 'Strengthen' sum.
getStrengthen        ∷ [Class] → Ninja → Int
getStrengthen clas n = sum [ str    | Strengthen cla str ← nEfs n, cla ∈ clas ]
-- | 'Weaken' sum.
getWeaken            ∷ [Class] → Ninja → Int
getWeaken clas n     = sum [ weak   | Weaken cla weak    ← nEfs n, cla ∈ clas ]
-- | 'Link' sum.
getLink              ∷ Slot → Ninja → Int
getLink c n          = sum [ link   | Link link          ← nEfs' ]
  where nEfs' = cat [ statusEfs | Status{..} ← nStats n, statusC ≡ c ]

-- | 'Boost' sum.
getBoost             ∷ Slot → Ninja → Int
getBoost c n@Ninja{..}
  | c ≡ nId   = 1
  | otherwise = product $ 1 : [ boo | Boost boo ← nEfs' ]
  where nEfs' = cat [ statusEfs | Status{..} ← nStats n, statusC ≡ c ]

-- | 'Exhaust' sum.
getExhaust ∷ [Class] → Ninja → Chakras
getExhaust classes n = ø { rand = length xs }
  where xs = [ cla | Exhaust cla ← nEfs n, cla ∈ classes ]

-- | 'Reduce' sum.
getReduce ∷ [Class] → Ninja → Int
getReduce [Affliction] n = sum [ reduce | Reduce Affliction reduce ← nEfs n ]
getReduce clas n = sum [ reduce | Reduce cla reduce ← nEfs n, cla ∈ clas ]

prod ∷ [Rational] → Rational
prod = product ∘ (1 :)

-- | 'Scale' product.
getScale ∷ [Class] → Ninja → Rational
getScale clas n = prod [ scale | Scale cla scale ← nEfs n
                               , cla ∈ clas
                               , scale ≥ 1 ∨ Affliction ∉ clas 
                               ]

-- | 'Ward' product.
getWard ∷ [Class] → Ninja → Rational
getWard [Affliction] n = prod [ 1 - ward | Ward Affliction ward ← nEfs n ]
getWard clas         n = prod [ 1 - ward | Ward cla ward ← nEfs n, cla ∈ clas ]

-- | 'Afflict' sum minus 'Heal' sum.
getNet ∷ Player → Game → Ninja → Int
getNet player game n = getAfflict player game n - getHeal player game n
-- | 'Afflict' sum.
getAfflict ∷ Player → Game → Ninja → Int
getAfflict player game n@Ninja{..} = sum
    [ afflict1 player game nId st | st@Status{..} ← nStats n 
                                  , not (is ImmuneSelf n) ∨ statusSrc ≠ nId
                                  ]

-- | 'Heal' sum.
getHeal ∷ Player → Game → Ninja → Int
getHeal player game n@Ninja{..}
  | not $ is Plague n = sum $ heal1 player game n ↤ nStats n
  | otherwise         = 0

afflict1 ∷ Player → Game → Slot → Status → Int
afflict1 player game t Status{..}
  | summed ≠ 0 ∧ alliedP player statusSrc = summed + ext
  | otherwise                             = 0
  where nt     = gameNinja t game
        n      = gameNinja statusSrc game
        summed = sum [afflict | Afflict afflict ← statusEfs]
        ext | t ≡ statusSrc          = 0
            | not $ isAlive n        = getBleed [Affliction, All] nt
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

-- * NINJAS

-- | Transforms all 'gameNinjas'.
alter ∷ (Seq Ninja → Seq Ninja) → Game → Game 
alter f game@Game{..} = game { gameNinjas = f gameNinjas }

getCd ∷ Variant → Seq Int → Int
getCd = (fromMaybe 0 ∘) ∘ S.lookup ∘ variantCD

getCds ∷ Ninja → Seq Int
getCds Ninja{..} = S.zipWith copyCd nCopied 
                 $ S.zipWith getCd (head ↤ nVariants) nCooldowns
  where isShallow (Shallow _ _) = True
        isShallow _             = False
        copyCd copied cooldown  = case copied of
          Just copied' | isShallow ∘ copying $ copiedSkill copied' → 0
          _                                                        → cooldown

-- ** LIFE AND DEATH

alives ∷ Player → Seq Ninja → [Ninja]
alives = (mfilter isAlive ∘) ∘ alliesP

-- | The entire team of a 'Player' is dead, in which case they lose.
dead ∷ Player → Game → Bool
dead p = all (not ∘ isAlive) ∘ playerTeam p

-- | Restricts 'nHealth' within a range.
healthBound ∷ Int -- ^ Minimum (usually 'minHealth')
            → Int -- ^ Unbounded health
            → Int -- ^ Health between minimum and 100
healthBound minhp = min 100 ∘ max minhp

isAlive ∷ Ninja → Bool
isAlive = (> 0) ∘ nHealth

-- | 1 if the 'Ninja' is affected by 'Endure', otherwise 0.
minHealth ∷ Ninja → Int
minHealth n
  | is Endure n = 1
  | otherwise   = 0

numAlive ∷ Player → Seq Ninja → Int
numAlive = (length ∘) ∘ alives

playerTeam ∷ Player → Game → [Ninja]
playerTeam p = alliesP p ∘ gameNinjas

-- PLAYERS

getPlayer ∷ Key User → Game → Maybe Player
getPlayer who Game{gamePlayers = (pA, pB)}
  | who ≡ pA  = Just PlayerA
  | who ≡ pB  = Just PlayerB
  | otherwise = Nothing

getVs ∷ Key User → Game → Maybe (Key User)
getVs who game@Game{..} = do
    player ← getPlayer who game
    return $ out2 (player ≡ PlayerB) gamePlayers

-- | If a 'Game' is over and 'gameVictor' is unset,
-- sets it to 'VictorA', 'VictorB', or 'Tie'.
yieldVictor ∷ Game → Game
yieldVictor game@Game{..}
  | isJust gameVictor = game
  | deadA ∧ deadB     = game { gameVictor = Just Tie }
  | deadA             = game { gameVictor = Just VictorB }
  | deadB             = game { gameVictor = Just VictorA }
  | otherwise         = game
  where deadA = dead PlayerA game
        deadB = dead PlayerB game

-- * SKILLS

-- | Adds 'Bloodline', 'Genjutsu', 'Ninjutsu', 'Taijutsu', and 'Random'
-- to the 'classes' of a 'Skill' if they are included in its 'cost'.
chakraClasses ∷ Skill → Skill
chakraClasses skill@Skill{..} = skill { classes = f classes }
  where Chakras b g n t r = cost
        f = (b > 0) ? (Bloodline :) 
          ∘ (g > 0) ? (Genjutsu  :) 
          ∘ (n > 0) ? (Ninjutsu  :) 
          ∘ (t > 0) ? (Taijutsu  :)
          ∘ (r > 0) ? (Random    :)

-- | Replaces an empty string with a 'label'.
defaultL ∷ Text → Skill → Text
defaultL "" Skill{..} = label
defaultL l  _         = l

-- ** COPIED SKILLS

-- | Maximum duration of an effect. 
-- Effects from 'Copied' 'Skill's must not last longer than the 'copyDuration'.
copyDur ∷ Copying → Int → Int
copyDur (Shallow _ d) = absmin d
copyDur (Deep    _ d) = absmin d
copyDur  NotCopied    = id

-- | 'Skill' owner. Determines the folder location of the icon image.
copyRoot ∷ Skill → Slot → Slot
copyRoot = cp ∘ copying
  where cp (Shallow a _) = const a
        cp (Deep    a _) = const a
        cp NotCopied     = id

-- ** MODIFICATION

getSkills ∷ Ninja → [Skill]
getSkills n = (getSkill n ∘ Left) ↤ [0..3]

getSkill ∷ Ninja → Either Int Skill → Skill
getSkill n      (Right skill) = usable n Nothing skill
getSkill n@Ninja{..} (Left s) = usable n (Just s) ∘ maybe def copiedSkill ∘ join 
                              $ S.lookup s nCopied
    where def = getSkill' n s (if s > 3 then 0 else getVar s n)

getVar ∷ Int → Ninja → Int
getVar s = maybe 0 (variantV ∘ head) ∘ S.lookup s ∘ nVariants

-- | Simplified 'getSkill' that ignores 'Copied' 'Skill's 
-- and does not check if the skill is 'usable'.
getSkill' ∷ Ninja → Int → Int → Skill
getSkill' n@Ninja{..} s v = restrict n ∘ changeSkill n 
                          $ characterSkills nCharacter !! s !! v

-- ** 'SkillTransform'

-- | Composition.
infixl 1 ••
(••) ∷ SkillTransform → SkillTransform → SkillTransform
(f •• g) n skill = g n $ f n skill
{-# INLINE (••) #-}

addClass ∷ Class → SkillTransform
addClass cla _ skill@Skill{..} = skill { classes = cla : classes }

changeSkill ∷ SkillTransform
changeSkill n skill = skill' 
    { cost = getExhaust (classes skill') n ⧺ cost skill' }
  where skill' = chakraClasses $ changes skill n skill

-- | Applies a 'SkillTransform' if 'hasOwn'.
changeWith ∷ Text → SkillTransform → SkillTransform
changeWith l f n@Ninja{..}
  | hasOwn l n ∨ hasDefense l nId n = f n
  | otherwise                       = id

-- | Increases 'cost' per 'numActive'.
costPer ∷ (Foldable a, Functor a) ⇒ Text → a ChakraType → SkillTransform
costPer l chaks n skill@Skill{..} = skill 
    { cost = cost ⧺ χ chaks *~ numActive l n }

-- | Turns AoE effects into single-target effects.
restrict ∷ SkillTransform
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

-- | Turns single-target effects into AoE effects.
targetAll ∷ SkillTransform
targetAll _ skill@Skill{..} = skill
  { start = f ↤ start, effects = f ↤ effects, disrupt = f ↤ disrupt }
  where f (targ, ef) = (f' targ, ef)
        f' Enemy     = Enemies
        f' Ally      = Allies
        f' XAlly     = XAllies
        f' a         = a

setCost ∷ (Foldable a, Functor a) ⇒ a ChakraType → SkillTransform
setCost chaks _ skill = skill { cost = χ chaks }

-- | Affects enemies instead of allies and allies instead of enemies.
swapSkill ∷ Status → Skill → Skill
swapSkill Status{..} skill@Skill{..} = skill { effects = f' ↤ effects 
                                             , start   = f' ↤ start
                                             , disrupt = f' ↤ disrupt
                                             }
  where f' (a, b)      = (f a, b)
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

-- ** USABILITY

targetable ∷ Skill 
           → Ninja -- ^ 'Skill' owner
           → Ninja -- ^ User
           → Ninja -- ^ Target
           → Bool
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

noInterrupt ∷ Channeling → Bool
noInterrupt Passive     = True
noInterrupt (Ongoing _) = True
noInterrupt _           = False

usable ∷ Ninja 
       → Maybe Int -- ^ Index in 'characterSkills'
       → Skill → Skill
usable n@Ninja{..} s skill@Skill{..}
  | charges > 0 ∧ maybe False (≥ charges) (s ≫= (nCharges !?)) = unusable
  | maybe False (>0) $ s ≫= (getCds n !?) = unusable
  | is Focus n                            = skill'
  | isNothing s ∧ noInterrupt channel     = skill'
  | classes ⩀ getStun n                   = unusable
  | isNothing s                           = skill'
  | Single ∉ classes                      = skill'
  | has label nId n                       = unusable
  | isChanneling label n                  = unusable
  | hasDefense label nId n                = unusable
  | otherwise                             = skill'
  where unusable    = skill { require = Unusable }
        skill'      = skill { require = check require }
        check req@(HasI _ _) 
          | isNothing s ∨ matchRequire req nId n = Usable
          | otherwise                            = Unusable
        check a = a

-- ** REQUIREMENTS

matchRequire ∷ Requirement → Slot → Ninja → Bool
matchRequire Usable     _ _           = True
matchRequire Unusable   _ _           = False
matchRequire (HasU l)   t n@Ninja{..} = t ≡ nId ∨ has l t n ∨ hasTrap l t n
matchRequire (HasI i l) t n@Ninja{..}
  | i > 0     = t ≠ nId ∨ numActive l n ≥ i
  | i < 0     = t ≠ nId ∨ numActive l n < (-i) 
  | otherwise = True

-- * STATUSES

-- ** MODIFICATION

nEfs ∷ Ninja → [Effect]
nEfs = catMap statusEfs ∘ nStats

nStats ∷ Ninja → [Status]
nStats n@Ninja{..} = getStat n ↤ nStatuses

getStat ∷ Ninja → Status → Status
getStat n@Ninja{..} st = st' { statusEfs = bst }
  where bst  = boost (getBoost (statusSrc st) n) ↤∘ mfilter keep $ statusEfs st'
        st'  = rawStat n st
        efs  = concatMap (statusEfs ∘ rawStat n) nStatuses
        keep Enrage       = True
        keep Seal         = True
        keep (Stun _)     = Focus ∉ efs
        keep (Immune _)   = Expose ∉ efs
        keep (Reduce _ _) = Expose ∉ efs
        keep ef | fromSelf n st = True
                | Enrage ∈ efs  = helpful ef ∨ sticky ef ∨ isCost ef
                | Seal   ∈ efs  = not $ helpful ef
                | otherwise     = ef ∉ [ nul | Nullify nul ← efs]

-- | Used by 'getStat' to prevent recursion 
-- when checking for 'Enrage' and 'Seal'.
rawStat ∷ Ninja → Status → Status
rawStat n@Ninja{..} st
  | fromSelf n st = st
  | Enrage ∈ efs  = st { statusEfs = enraged }
  | Seal   ∈ efs  = st { statusEfs = mfilter (not ∘ helpful) $ statusEfs st }
  | otherwise     = st
  where efs = catMap statusEfs nStatuses
        enraged = [ ef | ef ← statusEfs st, helpful ef ∨ sticky ef ∨ isCost ef ]

-- | 'Exhaust' and 'Unexhaust' are not canceled by immunity.
isCost ∷ Effect → Bool
isCost (Exhaust _) = True
isCost Unexhaust   = True
isCost _           = False

fromSelf ∷ Ninja → Status → Bool
fromSelf Ninja{..} Status{..} = statusSrc ≡ nId ∨ statusC ≡ nId

-- * CHECKING FOR STATUSES ON NINJAS

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
hasOwn l n@Ninja{..} = has l nId n ∨ isChanneling l n ∨ hasDefense l nId n

is ∷ Effect → Ninja → Bool
is ef = (ef ∈) ∘ nEfs

is' ∷ (Class → Effect) → Ninja → Bool
is' efs = ((efs ↤ allClasses) ⩀) ∘ nEfs

numActive ∷ Text → Ninja → Int
numActive l n@Ninja{..}
  | stacks > 0         = stacks
  | hasOwn l n         = 1
  | otherwise          = 0
  where stacks = numStacks l nId n

numStacks ∷ Text → Slot → Ninja → Int
numStacks l src Ninja{..} = length ∘ mfilter (lMatch l src) $ nStatuses

-- * TRAPS

classTrs ∷ (Foldable a, Functor a)
         ⇒ Bool → (Class → Trigger) → a Class → Ninja → Seq TrapTransform
classTrs False _  _       _     = ø
classTrs True  tr classes n = catMap classTrigger classes
    where classTrigger cla = getTraps True (tr cla) n
   
getPerTraps ∷ Bool → Trigger → Int → Ninja → Seq (Game → Game)
getPerTraps False _  _      _         = ø
getPerTraps True  tr amount Ninja{..} = traps ⧺ hooks
  where traps = [ trapEf trapTrack nId | Trap{..} ← nTraps, trapTrigger ≡ tr ]
        hooks = S.fromList 
                [ fn nId $ flip f amount | (p, f) ← characterHooks nCharacter
                                         , tr ≡ p 
                                         ]

getTrackTraps ∷ Bool → Trigger → Ninja → Seq (Game → Game)
getTrackTraps False _ _ = ø
getTrackTraps True tr Ninja{..} = [ trapEf trapTrack nId | Trap{..} ← nTraps
                                                         , trapTrigger ≡ tr
                                                         , trapDur ≤ 2 
                                                         , trapTrack > 0 
                                                         ]

getTraps ∷ Bool → Trigger → Ninja → Seq TrapTransform
getTraps False _ _ = ø
getTraps True tr Ninja{..} = [ trapEf | Trap{..} ← nTraps, trapTrigger ≡ tr ]
   
-- * TRIGGERING EFFECTS

-- | Obtains an 'Effect' and delete its 'Status' from its owner.
getOne ∷ (Effect → Bool) → Ninja → Maybe (Ninja, Effect, Status)
getOne matches n@Ninja{..} = do
    match         ← find (any matches ∘ statusEfs) nStatuses
    let stats'Del = delete match nStatuses
    case statusEfs match of
        [a] → return (n { nStatuses = stats'Del }, a, match)
        efs → do
            a ← find matches efs
            return (n { nStatuses = stats'Del }, a, match)

getOne' ∷ (Effect → Bool) → Ninja → Maybe Ninja
getOne' = fst3 ↤∘ getOne
  where fst3 (Just (n, _, _)) = Just n
        fst3 _                = Nothing

-- | Trigger a 'Copy'.
-- Returns ('statusSrc', 'statusL', 'copyTo', 'copyDuration').
copy ∷ [Class] → Ninja → Bool → [(Slot, Text, Int, Int)]
copy classes Ninja{..} harm = mapMaybe ifCopy allStatuses
  where allStatuses = mfilter (any matches ∘ statusEfs) nStatuses
        matches (Copy _ cla _ noharm) = (harm ∨ noharm) ∧ cla ∈ classes
        matches _                     = False
        ifCopy Status{..} = [ (statusSrc, statusL, copyTo, copyDuration) 
                                | Copy{..} ← find matches statusEfs 
                            ]

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
        
-- | Trigger a 'Parry'.
parry ∷ Skill → Ninja → Maybe (Ninja, Slot, Int)
parry skill@Skill{..} n@Ninja{..} = case parried of
    Just wasParried → return wasParried
    Nothing → 
        [ (n'', statusC st, a) | (n'', Parry _ a, st) ← getOne matchesOne n ]
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

-- | Trigger a 'Reapply'.
reapply ∷ [Class] → Ninja → Maybe Slot
reapply classes Ninja{..}
  | Unreflectable ∈ classes = Nothing
  | otherwise = statusC ↤ find ((Reapply ∈) ∘ statusEfs) nStatuses

-- | Trigger a 'Redirect'.
redirect ∷ [Class] → Ninja → Maybe Slot
redirect classes Ninja{..}
  | Unreflectable ∈ classes = Nothing
  | otherwise = statusC ↤ find (any match ∘ statusEfs) nStatuses
  where match (Redirect cla) = cla ∈ classes
        match _              = False

-- | Trigger a 'Reflect'.
reflect ∷ [Class] → Ninja → Ninja → Maybe Ninja
reflect clas n nt
  | [Mental, Unreflectable] ⩀ clas = Nothing
  | any ((ReflectAll ∈) ∘ statusEfs) $ nStatuses nt = Just nt
  | any ((OnReflectAll ≡) ∘ trapTrigger) $ nTraps n  = Just nt
  | otherwise = getOne' (Reflect ≡) nt

-- | Trigger a 'SnareTrap'.
snareTrap ∷ Skill → Ninja → Maybe (Ninja, Int)
snareTrap Skill{..} n@Ninja{..} 
    = [ (n', a) | (n', SnareTrap _ a, _) ← getOne matchesOne n ]
  where matchesOne (SnareTrap cla _) = cla ∈ classes
        matchesOne _                 = False

-- | Trigger a 'Swap'.
swap ∷ [Class] → Ninja → Maybe Status
swap clas n
  | Unreflectable ∈ clas = Nothing
  | otherwise            = find (any matches ∘ statusEfs) $ nStats n
  where matches (Swap cla) = cla ∈ clas
        matches _          = False
