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
    , getGameChakra, setGameChakra, lack, χ, unχ, χGain, χTotal
    -- * 'Channel'
    , fromChannel, isChanneling
    -- * 'Defense'
    , hasDefense
    -- * 'Effect'
    , filterEffects
    -- ** Reduction of all 'Effect's on a 'Ninja'
    , getBoost, getBleed, getBless, getBuild, getImmune, getIgnore
    , getInvincible, getLink, getNet, getReduce, getScale, getShare, getSnare
    , getStrengthen, getStun, getTaunting, getThreshold, getThrottle, getWard
    , getWeaken
    -- * 'Ninja'
    , alter, getCds
    -- ** Life and death
    , alives, dead, healthBound, isAlive, minHealth
    -- * 'Player'
    , getPlayer, getVs, yieldVictor
    -- * 'Skill'
    , chakraClasses, defaultL, skillSafe
    -- ** 'Copied'
    , copyDur, copyRoot
    -- ** Modification
    , getSkill, getSkill', getSkills
    -- *** 'changes'
    , (••)
    , addClass, changeWith, costPer, restrict, setCost, swapSkill, targetAll
    , targetOnly
    -- ** 'Requirement'
    , matchRequire
    -- * 'Status'
    , has, hasOwn, is, is', numActive, numStacks, deleteStats
    -- * 'Trap'
    , classTrs, getPerTraps, getTrackTraps, getTraps, getTrapsFrom, getTrapsTo
    -- * Triggering 'Effect's
    , copy, counter, parry, reapply, redir, reflect, snareTrap, triggerSwap
    -- * Usability
    , targetable, usable
    ) where

import StandardLibrary

import qualified Data.List     as List
import qualified Data.Sequence as Seq
import qualified Data.Text     as Text

import Calculus
import Core.Model
import Game.Structure

-- * ARITHMETIC

-- | Adds 1 to positives, subtracts 1 from negatives, and leaves 0s unchanged.
incr :: Int -> Int
incr x
  | x < 0     = x - 1
  | x > 0     = x + 1
  | otherwise = x

-- | Applies a function to the first in a pair if odd or the second if even.
byPar :: ∀ a. Int -> (a -> a) -> (a, a) -> (a, a)
byPar = do2 . even

-- | Flips 'PlayerA' and 'PlayerB'.
opponent :: Player -> Player
opponent PlayerA = PlayerB
opponent PlayerB = PlayerA

-- | Scales an amount by health lost, in specified intervals.
lost :: Int -- ^ Interval
     -> Int -- ^ Amount per interval
     -> Int -- ^ Health lost
     -> Int -- ^ Scaled amount
lost interval per = (per *) . (+ interval) . (100 -)

-- A 'Player' attempts to control a 'Ninja' not on their team.
illegal :: Player -> Act -> Bool
illegal p = (fromEnum p /=) . spar . actC

-- * CHAKRAS

getGameChakra :: Player -> Game -> Chakras
getGameChakra PlayerA = fst . gameChakra
getGameChakra PlayerB = snd . gameChakra
setGameChakra :: Player -> Chakras -> Game -> Game
setGameChakra player chakra game@Game{..} = game
    { gameChakra = in2 (PlayerA == player) chakra gameChakra }

-- Any component is negative.
lack :: Chakras -> Bool
lack (Chakras b g n t r) = b < 0 || g < 0 || n < 0 || t < 0 || r < 0

χ :: [ChakraType] -> Chakras
χ = sum . map toChak
  where 
    toChak Blood = Chakras 1 0 0 0 0
    toChak Gen   = Chakras 0 1 0 0 0
    toChak Nin   = Chakras 0 0 1 0 0
    toChak Tai   = Chakras 0 0 0 1 0
    toChak Rand  = Chakras 0 0 0 0 1

unχ :: Chakras -> [ChakraType]
unχ (Chakras b g n t _) = replicate b Blood
                        ++ replicate g Gen
                        ++ replicate n Nin
                        ++ replicate t Tai

-- | Adds 1 chakra per living 'Ninja' on the team of a 'Player'.
χGain :: Player -> [ChakraType] -> Seq Ninja -> Chakras -> Chakras
χGain player gain ns = (+ χ (take (numAlive player ns) gain))

-- | Sum of components.
χTotal :: Chakras -> Int
χTotal (Chakras b g n t r) = b + g + n + t + r

-- * CHANNELS

fromChannel :: Ninja -> Channel -> Act
fromChannel Ninja{..} Channel{..} = Act nId (Right channelSkill) channelT

isChanneling :: Text -> Ninja -> Bool
isChanneling l = any ((l ==) . label . channelSkill) . nChannels

-- * DEFENSE

hasDefense :: Text -> Slot -> Ninja -> Bool 
hasDefense l c = any (lMatch l c) . nDefense

-- ** EFFECTS

-- | Modifies 'Effect's when they are first added to a 'Ninja'.
filterEffects :: ∀ f. 
                (Functor f, IsSequence (f Effect), Element (f Effect) ~ Effect) 
              => Ninja -> f Effect -> f Effect
filterEffects n = map adjustEffect . filter keepEffects
  where 
    adjustEffect (Reduce cla x) = Reduce cla (x - getUnreduce n)
    adjustEffect f              = f
    keepEffects Invulnerable{}        = not $ is Expose n
    keepEffects _               = True

-- ** REDUCING EFFECTS

getTargets :: Effect -> Ninja -> [Slot]
getTargets ef n = [statusC | Status{..} <- nStats n, ef `elem` statusEfs ]

-- | 'Share's.
getShare :: Ninja -> [Slot]
getShare n@Ninja{..} = [ statusC | Status{..} <- nStats n
                                 , nId /= statusC
                                 , Share `elem` statusEfs 
                                 ]

cEfs :: Slot -> Ninja -> [Effect]
cEfs c n = [ ef | Status{..} <- nStats n, statusC == c, ef <- statusEfs ]

-- LIST COMPREHENSIONS, MOTHERFUCKERS
-- | 'Bleed' sum.
getBleed :: [Class] -> Ninja -> Int
getBleed clas n = sum [ x | Bleed cla x <- nEfs n, cla `elem` clas ]
-- | 'Bless' sum.
getBless :: Ninja -> Int
getBless n = sum [ x | Bless x <- nEfs n ]
-- | 'Build' sum.
getBuild :: Ninja -> Int
getBuild n = sum [ x | Build x <- nEfs n ]
-- | 'Ignore's.
getIgnore :: Ninja -> [Effect]
getIgnore n = [ f cla | cla        <- enums
                      , Status{..} <- nStats n
                      , Ignore f   <- statusEfs
                      ]
-- | 'Invulnerable's.
getImmune :: Ninja -> [Class]
getImmune n = [ x | Invulnerable x <- nEfs n ]
-- | 'Invincible's.
getInvincible :: Ninja -> [Class]
getInvincible n = [ x | Invincible x <- nEfs n ]
-- | 'Link' sum.
getLink :: Slot -> Ninja -> Int
getLink c n = sum [ x | Link x <- cEfs c n ]
-- | 'Snare' sum.
getSnare :: Ninja -> Int
getSnare n = sum [ x | Snare x <- nEfs n ]
-- | 'Stun's.
getStun :: Ninja -> [Class]
getStun n = [ x | Stun x <- nEfs n ]
-- | 'Throttle' sum.
getThrottle :: [Effect] -> Ninja -> Int
getThrottle efs n = sum [ x | Throttle f x <- nEfs n, throttled f ]
  where
    throttled = intersects efs . flip map enums
-- | 'Unreduce' sum.
getUnreduce :: Ninja -> Int
getUnreduce n = sum [ x | Unreduce x <- nEfs n ]
-- | 'Strengthen' sum.
getStrengthen :: [Class] -> Ninja -> Int
getStrengthen clas n = sum [ x | Strengthen cla x <- nEfs n, cla `elem` clas ]
-- | 'Threshold' max.
getThreshold :: Ninja -> Int
getThreshold n = maximumEx $ 0 :| [x | Threshold x <- nEfs n ]
-- | 'Weaken' sum.
getWeaken :: [Class] -> Ninja -> Int
getWeaken clas n = sum [ x | Weaken cla x <- nEfs n, cla `elem` clas ]

-- | 'Boost' sum.
getBoost :: Slot -> Ninja -> Int
getBoost c n@Ninja{..}
  | c == nId   = 1
  | otherwise = product $ 1 : [ x | Boost x <- cEfs c n ]

-- | 'Exhaust' sum.
getExhaust :: [Class] -> Ninja -> Chakras
getExhaust classes n = 0{ rand = length xs }
  where 
    xs = [ x | Exhaust x <- nEfs n, x `elem` classes ]

-- | 'Reduce' sum.
getReduce :: [Class] -> Ninja -> Int
getReduce [Affliction] n = sum [ x | Reduce Affliction x <- nEfs n ]
getReduce clas n = sum [ x | Reduce cla x <- nEfs n, cla `elem` clas ]

prod :: [Rational] -> Rational
prod = product . (1 :)

-- | 'Scale' product.
getScale :: [Class] -> Ninja -> Rational
getScale clas n = prod [ x | Scale cla x <- nEfs n
                           , cla `elem` clas
                           , x >= 1 || Affliction `notElem` clas 
                           ]

-- | 'Ward' product.
getWard :: [Class] -> Ninja -> Rational
getWard [Affliction] n = prod [ 1 - x | Ward Affliction x <- nEfs n ]
getWard clas         n = prod [ 1 - x | Ward cla x <- nEfs n, cla `elem` clas ]

-- | Duration of most recent 'Taunting'.
getTaunting :: Ninja -> Maybe (Int, Status)
getTaunting n = listToMaybe 
    [ (a, status) | status@Status{..} <- nStats n, Taunting a <- statusEfs ]

-- | 'Afflict' sum minus 'Heal' sum.
getNet :: Player -> Game -> Ninja -> Int
getNet player game n = getAfflict player game n - getHeal player game n
-- | 'Afflict' sum.
getAfflict :: Player -> Game -> Ninja -> Int
getAfflict player game n@Ninja{..} = sum
    [ afflict1 player game nId st | st@Status{..} <- nStats n 
                                  , not (is ImmuneSelf n) || statusSrc /= nId
                                  , not $ [All, Affliction] 
                                          `intersects` getInvincible n
                                  ]

-- | 'Heal' sum.
getHeal :: Player -> Game -> Ninja -> Int
getHeal player game n@Ninja{..}
  | not $ is Plague n = sum $ heal1 player game n <$> nStats n
  | otherwise         = 0

afflict1 :: Player -> Game -> Slot -> Status -> Int
afflict1 player game t Status{..}
  | summed /= 0 && alliedP player statusSrc = summed + ext
  | otherwise                             = 0
  where 
    nt     = gameNinja t game
    n      = gameNinja statusSrc game
    summed = sum [afflict | Afflict afflict <- statusEfs]
    ext 
      | t == statusSrc         = 0
      | not $ isAlive n        = getBleed [Affliction, All] nt
      | is (Stun Affliction) n = 0
      | otherwise              = getStrengthen [Affliction, All] n
                                 + getBleed      [Affliction, All] nt
                                 + getLink       statusSrc         nt

heal1 :: Player -> Game -> Ninja -> Status -> Int
heal1 player game n Status{..}
  | summed /= 0 && alliedP player statusSrc = 
      getBoost statusSrc n * summed + getBless (gameNinja statusSrc game)
  | otherwise = 0
  where 
    summed = sum [ heal | Heal heal <- statusEfs]

-- * NINJAS

-- | Transforms all 'gameNinjas'.
alter :: (Seq Ninja -> Seq Ninja) -> Game -> Game 
alter f game@Game{..} = game { gameNinjas = f gameNinjas }

getCd :: Variant -> Seq Int -> Int
getCd = (fromMaybe 0 .) . Seq.lookup . variantCD

getCds :: Ninja -> Seq Int
getCds Ninja{..} = Seq.zipWith copyCd nCopied $
                   Seq.zipWith getCd (head <$> nVariants) nCooldowns
  where 
    isShallow Shallow{} = True
    isShallow _         = False
    copyCd (Just copied) 
        | isShallow . copying $ copiedSkill copied = const 0
    copyCd _ = id

-- ** LIFE AND DEATH

alives :: ∀ o. Mono o Ninja => Player -> o -> [Ninja]
alives = (filter isAlive .) . alliesP

-- | The entire team of a 'Player' is dead, in which case they lose.
dead :: Player -> Game -> Bool
dead p = all (not . isAlive) . playerTeam p

-- | Restricts 'nHealth' within a range.
healthBound :: Int -- ^ Minimum (usually 'minHealth')
            -> Int -- ^ Unbounded health
            -> Int -- ^ Health between minimum and 100
healthBound minhp = min 100 . max minhp

isAlive :: Ninja -> Bool
isAlive = (> 0) . nHealth

-- | 1 if the 'Ninja' is affected by 'Endure', otherwise 0.
minHealth :: Ninja -> Int
minHealth n
  | is Endure n = 1
  | otherwise   = 0

numAlive :: ∀ o. Mono o Ninja => Player -> o -> Int
numAlive = (length .) . alives

playerTeam :: Player -> Game -> [Ninja]
playerTeam p = alliesP p . gameNinjas

-- PLAYERS

getPlayer :: Key User -> Game -> Maybe Player
getPlayer who Game { gamePlayers = (pA, pB) }
  | who == pA = Just PlayerA
  | who == pB = Just PlayerB
  | otherwise = Nothing

getVs :: Key User -> Game -> Maybe (Key User)
getVs who game@Game{..} = do
    player <- getPlayer who game
    return $ out2 (player == PlayerB) gamePlayers

-- | If a 'Game' is over and 'gameVictor' is unset,
-- sets it to 'VictorA', 'VictorB', or 'Tie'.
yieldVictor :: Game -> Game
yieldVictor game@Game{..}
  | isJust gameVictor = game
  | deadA && deadB     = game { gameVictor = Just Tie }
  | deadA             = game { gameVictor = Just VictorB }
  | deadB             = game { gameVictor = Just VictorA }
  | otherwise         = game
  where 
    deadA = dead PlayerA game
    deadB = dead PlayerB game

-- * SKILLS

-- | Adds 'Bloodline', 'Genjutsu', 'Ninjutsu', 'Taijutsu', and 'Random'
-- to the 'classes' of a 'Skill' if they are included in its 'cost'.
chakraClasses :: Skill -> Skill
chakraClasses skill@Skill{..} = skill { classes = f classes }
  where 
    Chakras b g n t r = cost
    f = (b > 0 ? (Bloodline :)) .
        (g > 0 ? (Genjutsu  :)) .
        (n > 0 ? (Ninjutsu  :)) .
        (t > 0 ? (Taijutsu  :)) .
        (r > 0 ? (Random    :))

-- | Replaces an empty string with a 'label'.
defaultL :: Text -> Skill -> Text
defaultL "" Skill{..} = label
defaultL l  _         = l

-- | Converts a function that uses raw 'Int's as indices in a
-- 'Character''s `[[Skill]]` list into one that searches by name. 
-- Passing an empty string for the second argument will select the base (0) 
-- 'Skill' in the lists. Otherwise, the root skill will not be considered.
-- This means that if a skill has a variant with the same name as it, "Skill" ""
-- selects the base variant, while "Skill" "Skill" selects
-- the identically-named variant.
skillSafe :: ∀ a. a -> (Int -> Int -> a) -> Ninja -> Text -> Text -> a
skillSafe a f Ninja{nCharacter = Character{..}} l variant = fromMaybe a $ do
    s <- List.findIndex (any $ match l) $ toList characterSkills
    v <- case variant of
            "" -> pure 0
            _  -> let (_ :| skills) = characterSkills !! s
                  in (+1) <$> List.findIndex (match variant) skills
    pure $ f s v
  where
    match x = (== Text.toCaseFold x) . Text.toCaseFold . label

-- ** COPIED SKILLS

-- | Maximum duration of an effect. 
-- Effects from 'Copied' 'Skill's must not last longer than the 'copyDuration'.
copyDur :: Copying -> Int -> Int
copyDur (Shallow _ d) = absmin d
copyDur (Deep    _ d) = absmin d
copyDur  NotCopied    = id

-- | 'Skill' owner. Determines the folder location of the icon image.
copyRoot :: Skill -> Slot -> Slot
copyRoot = cp . copying
  where 
    cp (Shallow a _) = const a
    cp (Deep    a _) = const a
    cp NotCopied     = id

-- ** MODIFICATION

getSkills :: Ninja -> [Skill]
getSkills n = getSkill n . Left <$> [0..3]

getSkill :: Ninja -> Either Int Skill -> Skill
getSkill n      (Right skill) = usable n Nothing skill
getSkill n@Ninja{..} (Left s) = usable n (Just s) . maybe go copiedSkill . 
                                join $ Seq.lookup s nCopied
    where 
      go = getSkill' n s (if s > 3 then 0 else getVar s n)

getVar :: Int -> Ninja -> Int
getVar s = maybe 0 (variantV . head) . Seq.lookup s . nVariants

-- | Simplified 'getSkill' that ignores 'Copied' 'Skill's 
-- and does not check if the skill is 'usable'.
getSkill' :: Ninja -> Int -> Int -> Skill
getSkill' n@Ninja{..} s v = restrict n . changeSkill n $
                            characterSkills nCharacter !! s !! v

-- ** 'SkillTransform'

-- | Composition.
infixl 1 ••
(••) :: SkillTransform -> SkillTransform -> SkillTransform
(f •• g) n skill = g n $ f n skill
{-# INLINE (••) #-}

addClass :: Class -> SkillTransform
addClass cla _ skill@Skill{..} = skill { classes = cla : classes }

changeSkill :: SkillTransform
changeSkill n skill = skill' 
    { cost = getExhaust (classes skill') n + cost skill' }
  where 
    skill' = chakraClasses $ changes skill n skill

-- | Applies a 'SkillTransform' if 'hasOwn'.
changeWith :: Text -> SkillTransform -> SkillTransform
changeWith l f n@Ninja{..}
  | hasOwn l n || hasDefense l nId n = f n
  | otherwise                       = id

-- | Increases 'cost' per 'numActive'.
costPer :: Text -> [ChakraType] -> SkillTransform
costPer l chaks n skill@Skill{..} = skill 
    { cost = cost + χ chaks * fromInteger (toInteger $ numActive l n) }

changeEffects :: ([(Target, Transform)] -> [(Target, Transform)])
              -> Skill -> Skill
changeEffects f skill@Skill{..} = 
    skill { effects = f effects, start = f start, disrupt = f disrupt } 

-- | Turns AoE effects into single-target effects.
restrict :: SkillTransform
restrict n = is Restrict n ? changeEffects (mapMaybe go)
  where 
    go (XEnemies, _)  = Nothing
    go (REnemy,   _)  = Nothing
    go (Everyone, ef) = Just (Allies, ef)
    go (Enemies, ef)  = Just (Enemy, ef)
    go x              = Just x

-- | Turns single-target effects into AoE effects.
targetAll :: SkillTransform
targetAll = const . changeEffects $ map go
  where
    go (targ, ef) = (go' targ, ef)
    go' Enemy     = Enemies
    go' Ally      = Allies
    go' XAlly     = XAllies
    go' x         = x

-- | Restricts skill effects to a specified list of 'Target's.
targetOnly :: ∀ o. Mono o Target => o -> SkillTransform
targetOnly xs = const . changeEffects $ filter ((`elem` xs) . fst)

setCost :: [ChakraType] -> SkillTransform
setCost chaks _ skill = skill { cost = χ chaks }

-- | Affects enemies instead of allies and allies instead of enemies.
swapSkill :: Status -> Skill -> Skill
swapSkill Status{..} = changeEffects $ map go
  where 
    go (x, y)      = (go' x, y)
    go' Self         = Self
    go' Ally         = Specific statusSrc
    go' XAlly        = Specific statusSrc
    go' RAlly        = REnemy
    go' Allies       = Enemies
    go' XAllies      = Enemies
    go' Enemy        = Self
    go' REnemy       = RAlly
    go' Enemies      = Allies
    go' XEnemies     = XAllies
    go' Everyone     = Everyone
    go' (Specific x) = Specific x

-- ** USABILITY

targetable :: Skill 
           -> Ninja -- ^ 'Skill' owner
           -> Ninja -- ^ User
           -> Ninja -- ^ Target
           -> Bool
targetable Skill{..} nSrc n nt
  | not $ matchRequire require src nt                   = False
  | not (isAlive nt) && Necromancy `notElem` classes    = False
  | isAlive nt && src /= t && Necromancy `elem` classes = False
  | Bypassing `elem` classes                            = True
  | harm && (classes `intersects` getImmune nt)         = False
  | src /= t && not harm && is Seal nt                  = False
  | c /= t && (is Isolate n || dueling || taunted)      = False
  | t `elem` getTargets Block n                         = False
  | otherwise                                           = True
  where 
    src     = nId nSrc
    c       = nId n
    t       = nId nt
    harm    = not $ allied c t && allied src t
    dueling = notIn c $ getTargets Duel nt
    taunted = notIn t $ getTargets Taunt n
    notIn a xs = not (null xs) && a `notElem` xs

noInterrupt :: Channeling -> Bool
noInterrupt Passive   = True
noInterrupt Ongoing{} = True
noInterrupt _         = False

usable :: Ninja 
       -> Maybe Int -- ^ Index in 'characterSkills'
       -> Skill -> Skill
usable n@Ninja{..} s skill@Skill{..}
  | charges > 0 && maybe False (>= charges) (s >>= (nCharges !?)) = unusable
  | maybe False (>0) $ s >>= (getCds n !?) = unusable
  | isNothing s && noInterrupt channel     = skill'
  | classes `intersects` getStun n         = unusable
  | isNothing s                            = skill'
  | Single `notElem` classes               = skill'
  | isChanneling label n                   = unusable
  | has label nId n                        = unusable
  | hasDefense label nId n                 = unusable
  | hasTrap label nId n                    = unusable
  | otherwise                              = skill'
  where 
    unusable = skill { require = Unusable }
    skill'   = skill { require = isUsable require }
    isUsable req@HasI{}
      | isNothing s || matchRequire req nId n = Usable
      | otherwise                            = Unusable
    isUsable x = x

-- ** REQUIREMENTS

matchRequire :: Requirement -> Slot -> Ninja -> Bool
matchRequire Usable     _ _           = True
matchRequire Unusable   _ _           = False
matchRequire (HasU l)   t n@Ninja{..} = t == nId || has l t n || hasTrap l t n
matchRequire (HasI i l) t n@Ninja{..}
  | i > 0     = t /= nId || numActive l n >= i
  | i < 0     = t /= nId || numActive l n < (-i) 
  | otherwise = True

-- * STATUSES

-- ** MODIFICATION

-- | Replicates all 'statusEfs' of a 'Status' by its 'statusCount'.
unfoldStat :: Status -> Status
unfoldStat status@Status{..} = 
        status { statusEfs = statusEfs >>= replicate statusCount }

-- | Decreases 'statusCount' by 1 and deletes if now 0.
deleteStat :: Status -> [Status] -> [Status]
deleteStat x@Status{..} xs
  | statusCount == 1 = xs'
  | otherwise        = x' : xs'
  where 
    x'  = x { statusCount = statusCount - 1 }
    xs' = delete x xs

-- | Decreases the 'statusCount' of a matching 'Status' by some number
-- | and deletes if now 0.
deleteStats :: Int -> (Status -> Bool) -> [Status] -> [Status]
deleteStats i predic xs = case find predic xs of
    Nothing -> xs
    Just x  -> (statusCount x > i ? (x { statusCount = statusCount x - i } :)) $
               delete x xs

nEfs :: Ninja -> [Effect]
nEfs n@Ninja{..} = [ ef | st <- nStatuses
                        , ef <- statusEfs $ unfoldStat st 
                        , ef `notElem` ignore
                        ]
  where
    ignore = getIgnore n

nStats :: Ninja -> [Status]
nStats n@Ninja{..} = unfoldStat . getStat n <$> nStatuses

getStat :: Ninja -> Status -> Status
getStat n@Ninja{..} st = st' { statusEfs = bst }
  where 
    bst  = map (boost (getBoost (statusSrc st) n)) . filter keep $ statusEfs st'
    st'  = rawStat n st
    efs  = concatMap (statusEfs . rawStat n) nStatuses
    keep Enrage   = True
    keep Seal     = True
    keep Invulnerable{} = Expose `notElem` efs
    keep Reduce{} = Expose `notElem` efs
    keep ef 
      | fromSelf n st = True
      | Enrage `elem` efs = helpful ef || sticky ef || isCost ef
      | Seal   `elem` efs = not $ helpful ef
      | otherwise = ef `notElem` [ f cla | Ignore f <- efs, cla <- enums ]

-- | Used by 'getStat' to prevent recursion 
-- when checking for 'Enrage' and 'Seal'.
rawStat :: Ninja -> Status -> Status
rawStat n@Ninja{..} st
  | fromSelf n st = st
  | Enrage `elem` efs = st { statusEfs = enraged }
  | Seal   `elem` efs = st { statusEfs = filter (not . helpful) $ statusEfs st }
  | otherwise         = st
  where 
    efs     = concatMap statusEfs nStatuses
    enraged = [ ef | ef <- statusEfs st, helpful ef || sticky ef || isCost ef ]

-- | 'Exhaust' and 'Unexhaust' are not canceled by immunity.
isCost :: Effect -> Bool
isCost Exhaust{} = True
isCost Unexhaust = True
isCost _         = False

fromSelf :: Ninja -> Status -> Bool
fromSelf Ninja{..} Status{..} = statusSrc == nId || statusC == nId

-- * CHECKING FOR STATUSES ON NINJAS

has :: Text -> Slot -> Ninja -> Bool
has l src = any match . nStatuses
  where 
    match Status{..} = 
        statusL == l 
        && (statusC == src || statusC == src || statusRoot == src)
        && (Unshifted `elem` statusClasses || Shifted `notElem` statusClasses)

hasTrap :: Text -> Slot -> Ninja -> Bool
hasTrap l src = any match . nTraps
    where 
      match Trap{..} = 
          trapL == l && trapSrc == src
          && (Unshifted `elem` trapClasses || Shifted `notElem` trapClasses)

hasOwn :: Text -> Ninja -> Bool
hasOwn l n@Ninja{..} = has l nId n || isChanneling l n || hasDefense l nId n

is :: Effect -> Ninja -> Bool
is ef = (ef `elem`) . nEfs

is' :: (Class -> Effect) -> Ninja -> Bool
is' efs = ((efs <$> enums) `intersects`) . nEfs

numActive :: Text -> Ninja -> Int
numActive l n@Ninja{..}
  | stacks > 0         = stacks
  | hasOwn l n         = 1
  | otherwise          = 0
  where 
    stacks = numStacks l nId n

numStacks :: Text -> Slot -> Ninja -> Int
numStacks l src Ninja{..} = sum . map statusCount . filter (lMatch l src) $ 
                            nStatuses

-- * TRAPS

classTrs :: ∀ f. (Foldable f, Functor f)
         => Bool -> (Class -> Trigger) -> f Class -> Ninja -> Seq TrapTransform
classTrs False _  _       _ = mempty
classTrs True  tr classes n = asum $ fmap classTrigger classes
    where 
      classTrigger :: Class -> Seq TrapTransform
      classTrigger cla = getTraps True (tr cla) n
   
getPerTraps :: Bool -> Trigger -> Int -> Ninja -> Seq (Game -> Game)
getPerTraps False _  _      _         = mempty
getPerTraps True  tr amount Ninja{..} = traps ++ hooks
  where 
    traps = [ trapEf trapTrack nId | Trap{..} <- nTraps, trapTrigger == tr ]
    hooks = Seq.fromList 
            [ fN nId $ f amount | (p, f) <- characterHooks nCharacter, tr == p ]

getTrackTraps :: Bool -> Trigger -> Ninja -> Seq (Game -> Game)
getTrackTraps False _ _ = mempty
getTrackTraps True tr Ninja{..} = [ trapEf trapTrack nId | Trap{..} <- nTraps
                                                         , trapTrigger == tr
                                                         , trapDur <= 2 
                                                         , trapTrack > 0 
                                                         ]

getTraps :: Bool -> Trigger -> Ninja -> Seq TrapTransform
getTraps False _ _         = mempty
getTraps True tr Ninja{..} = [ trapEf | Trap{..} <- nTraps, trapTrigger == tr ]

getTrapsTo :: Bool -> Trigger -> Ninja -> Seq TrapTransform
getTrapsTo False _ _         = mempty
getTrapsTo True tr Ninja{..} = [ trapEf | Trap{..} <- nTraps
                                        , trapTrigger == tr 
                                        , trapType /= TrapFrom
                                        ]

getTrapsFrom :: Trigger -> Ninja -> Ninja -> Seq (Game -> Game)
getTrapsFrom tr Ninja{nTraps} Ninja{nId} = [ trapEf 0 nId | Trap{..} <- nTraps
                                                          , trapTrigger == tr 
                                                          , trapType == TrapFrom
                                                          ]

-- * TRIGGERING EFFECTS

-- | Obtains an 'Effect' and delete its 'Status' from its owner.
getOne :: (Effect -> Bool) -> Ninja -> Maybe (Ninja, Effect, Status)
getOne matches n@Ninja{..} = do
    match        <- find (any matches . statusEfs) nStatuses
    let stats'Del = deleteStat match nStatuses
    case statusEfs match of
        [a] -> return (n { nStatuses = stats'Del }, a, match)
        efs -> do
            a <- find matches efs
            return (n { nStatuses = stats'Del }, a, match)

getOne' :: (Effect -> Bool) -> Ninja -> Maybe Ninja
getOne' = map fst3 . getOne
  where 
    fst3 (Just (n, _, _)) = Just n
    fst3 _                = Nothing

-- | Trigger a 'Copy'.
-- Returns ('statusSrc', 'statusL', 'copyTo', 'copyDuration').
copy :: [Class] -> Ninja -> Bool -> [(Slot, Text, Int, Int)]
copy classes Ninja{..} harm = mapMaybe ifCopy allStatuses
  where 
    allStatuses = filter (any matches . statusEfs) nStatuses
    matches (Copy _ cla _ noharm) = (harm || noharm) && cla `elem` classes
    matches _                     = False
    ifCopy Status{..} = [ (statusSrc, statusL, copyTo, copyDuration) 
                            | Copy{..} <- find matches statusEfs 
                        ]

-- | Trigger a 'Counter'.
counter :: [Class] -> Ninja -> Ninja -> Maybe Ninja
counter classes n nt
  | nocounter = Just nt
  | isJust . find (any matchN . statusEfs) $ nStatuses nt = Just nt
  | otherwise = getOne' match nt
  where 
    nocounter = Uncounterable `notElem` classes 
              && any ((OnCounterAll ==) . trapTrigger) (nTraps n)
    matchN (CounterAll Uncounterable) = True
    matchN (CounterAll cla)           = cla `elem` classes 
                                        && Uncounterable `notElem` classes
    matchN _                          = False
    match (Counter Uncounterable)     = True
    match (Counter cla)               = cla `elem` classes 
                                        && Uncounterable `notElem` classes
    match _ = False
        
-- | Trigger a 'Parry'.
parry :: Skill -> Ninja -> Maybe (Ninja, Status, Transform)
parry skill@Skill{..} n@Ninja{..} = 
    [(n', st, a) | st@Status{..} <- find (any matchN . statusEfs) nStatuses
                 , ParryAll _ a  <- find matchN statusEfs 
                 ] <|>
    [(n'', st, a) | (n'', Parry _ a, st@Status{..}) <- getOne match n]
  where
    n' = n { nParrying = skill : nParrying }
    matchN (ParryAll Uncounterable _) = True
    matchN (ParryAll cla _)           = cla `elem` classes 
                                        && Uncounterable `notElem` classes
    matchN _                          = False
    match (Parry Uncounterable _)     = True
    match (Parry cla _)               = cla `elem` classes 
                                        && Uncounterable `notElem` classes
    match _ = False

-- | Trigger a 'Reapply'.
reapply :: [Class] -> Ninja -> Maybe Slot
reapply classes Ninja{..}
  | Unreflectable `elem` classes = Nothing
  | otherwise = statusC <$> find ((Reapply `elem`) . statusEfs) nStatuses

-- | Trigger a 'Redirect'.
redir :: [Class] -> Ninja -> Maybe Slot
redir classes Ninja{..}
  | Unreflectable `elem` classes = Nothing
  | otherwise = statusC <$> find (any match . statusEfs) nStatuses
  where 
    match (Redirect cla) = cla `elem` classes
    match _              = False

-- | Trigger a 'Reflect'.
reflect :: [Class] -> Ninja -> Ninja -> Maybe Ninja
reflect clas n nt
  | [Mental, Unreflectable] `intersects` clas = Nothing
  | any ((ReflectAll `elem`) . statusEfs) $ nStatuses nt = Just nt
  | any ((OnReflectAll ==) . trapTrigger) $ nTraps n  = Just nt
  | otherwise = getOne' (Reflect ==) nt

-- | Trigger a 'SnareTrap'.
snareTrap :: Skill -> Ninja -> Maybe (Ninja, Int)
snareTrap Skill{..} n@Ninja{..} =
    [ (n', a) | (n', SnareTrap _ a, _) <- getOne match n ]
  where 
    match (SnareTrap cla _) = cla `elem` classes
    match _                 = False

-- | Trigger a 'Swap'.
triggerSwap :: [Class] -> Ninja -> Maybe Status
triggerSwap clas n
  | Unreflectable `elem` clas = Nothing
  | otherwise            = find (any match . statusEfs) $ nStats n
  where 
    match (Swap cla) = cla `elem` clas
    match _          = False
