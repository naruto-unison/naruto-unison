{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Data structures for gameplay.
module Game.Structure
    ( 
    -- * Act
      Act(..), ActPath(..), actFromPath, botActs, Affected(..)
    -- * Chakras
    , Chakras(..), ChakraType (..)
    -- * Channel
    , Channel(..), Channeling(..), ChannelTag(..)
    -- * Character
    , Character(..), Group(..)
    -- * Class
    , Class(..)
    -- * Delay
    , Delay(..)
    -- * Destructible
    , Barrier(..), Defense(..)
    -- * Effect
    , Effect(..), Amount(..), helpful, sticky, boost
    -- * Function types
    , Transform, SkillTransform, TrapTransform
    -- * Game
    , Game(..), newGame, gameNinja, setNinja, fN, mockSlot
    -- * Labeled
    , Labeled(..), lEq, lMatch
    -- * Ninja
    , Ninja(..), Face(..), Flag(..), newNinja, ninjaReset, adjustCd, insertCd
    -- * Player
    , Player(..), Victor(..)
    -- * Skill
    , Skill(..), newSkill, Target(..), Requirement(..), Copied(..)
    -- * Slot
    , Slot, allied, allies, alliesP, alliedP, enemies, enemiesP, spar
    , teamSize, gameSize
    , allSlots, allySlots, enemySlots, opponentSlots
    , bySlot, outSlot, outSlot', choose, skillTargets
    -- * Status
    , Status(..), Bomb(..), Copying(..)
    -- * Trap
    , Trap(..), TrapType(..), Trigger(..)
    -- * TurnBased
    , TurnBased(..), decrTurn, sync
    -- * Variant
    , Variant(..), variantCD, noVariant
    ) where
  
import StandardLibrary

import qualified Data.Text.Read as Read
import qualified Data.Sequence  as Seq
import qualified Data.Text      as Text

--import Yesod --(ToJSON, toJSON, PathPiece, toPathPiece, fromPathPiece)

import Calculus
import Core.Model
import Core.BlackMagic ()

-- Each player controls 3 'Ninja's.
teamSize :: Int
teamSize = 3
-- There are 6 total 'Ninja's in a game (@teamSize * 2).
gameSize :: Int
gameSize = teamSize * 2 

-- Converts from turns to sub-turns.
-- Each turn consists of two sub-turns, one for each player.
sync :: Int -> Int 
sync n
  | n >= 0    = 2 * n
  | otherwise = (-2) * n - 1

-- | The type signature of game actions. Processed into @'Game' -> 'Game'@.
type Transform = Skill -- ^ Skill
               -> Slot  -- ^ Source (Src)
               -> Slot  -- ^ Actor  (C)
               -> Game  -- ^ Before
               -> Slot  -- ^ Target (T)
               -> Game  -- ^ After
                 

-- | The type signature of 'changes'.
type SkillTransform = (Ninja -> Skill -> Skill)

-- | The type signature of 'Trap' actions.
type TrapTransform = Int  -- ^ Amount (optional argument for traps)
                   -> Slot -- ^ Source (optional argument for traps)
                   -> Game -- ^ Before
                   -> Game -- ^ After
                     

-- | Typeclass for structures that expire after a set number of turns.
class TurnBased a where
    -- | Number of turns before expiration. If <= 0, never expires.
    getDur :: a -> Int     
    -- | Updates the remaining number of turns after a turn has passed.
    setDur :: Int -> a -> a  

-- If @'getDur' <= 0, has no effect.
-- If @'getDur' == 1, deletes the structure; 
-- it has reached the end of its duration.
-- Otherwise, decreases the remaining duration by 1.
decrTurn :: ∀ a. TurnBased a => a -> Maybe a
decrTurn a
  | dur == 0  = Just a
  | dur <= 1  = Nothing
  | otherwise = Just $ setDur (dur - 1) a
  where 
    dur = getDur a

-- Typeclass for data structures that have a label 
-- and originate from a specific 'Ninja'. 
-- This is important because two different 'Ninja's might have 'Skill's with
-- the same label, so both label and origin must match in order for a structure
-- to count as theirs.
class Labeled a where
    -- Label
    getL   :: a -> Text
    -- 'Ninja' user
    getSrc :: a -> Slot

-- Equality.
lEq :: ∀ a. Labeled a => a -> a -> Bool
lEq x y = getL x == getL y && getSrc x == getSrc y

-- Matching.
lMatch :: ∀ a. Labeled a => Text -> Slot -> a -> Bool
lMatch l src a = getL a == l && getSrc a == src

-- | Qualifiers of 'Skill's and 'Status'es.
data Class 
    = Invisible
    | InvisibleTraps
    | Soulbound
    -- Tags
    | Bane
    | Summon
    -- Distance
    | Melee
    | Ranged
    -- Type
    | Chakra
    | Physical
    | Mental
    -- Limits
    | Nonstacking
    | Single
    | Extending
    -- Prevention
    | Bypassing
    | Uncounterable
    | Unreflectable
    | Unremovable
    | Necromancy
    -- Fake (Hidden)
    | All
    | Harmful
    | Healing
    | Hidden
    | Affliction
    | NonAffliction
    | NonMental
    | Resource -- ^ Display stacks separately
    | Shifted
    | Unshifted
    | Direct
    | BaseTrap
    | NewRandoms
    -- Chakra (Hidden)
    | Bloodline
    | Genjutsu
    | Ninjutsu
    | Taijutsu
    | Random
    deriving (Enum, Eq, Show, Bounded)

instance ToJSON Class where
    toJSON = toJSON . cJson
      where 
        cJson InvisibleTraps = cJson Invisible
        cJson a              = show a

show' :: Class -> String
show' NonMental       = "Non-mental"
show' NonAffliction   = "Non-affliction"
show' InvisibleTraps  = show Invisible
show' a               = show a

lower :: String -> String
lower = unpack . Text.toLower . pack

data Amount = Flat | Percent deriving (Eq)

-- | Effects of 'Status'es.
data Effect 
    = Afflict      Int               -- ^ Deals damage every turn
    | AntiCounter                    -- ^ Cannot be countered or reflected
    | Bleed        Class Amount Int  -- ^ Adds to damage received
    | Bless        Int               -- ^ Adds to healing 'Skill's
    | Block                          -- ^ Treats source as 'Invulnerable'
    | Boost        Int               -- ^ Scales effects from allies
    | Build        Int               -- ^ Adds to destructible defense 'Skill'
    | Counter      Class             -- ^ Counters the first 'Skill's
    | CounterAll   Class             -- ^ 'Counter's without being removed
    | Duel                           -- ^ 'Invulnerable' to everyone but source
    | Endure                         -- ^ Health cannot go below 1
    | Enrage                         -- ^ Ignore all harmful status effects
    | Exhaust      Class             -- ^ 'Skill's cost 1 additional random chakra
    | Expose                         -- ^ Cannot reduce damage or be 'Invulnerable'
    | Heal         Int               -- ^ Heals every turn
    | Ignore       (Class -> Effect) -- ^ Invulnerable to certain effects
    | Invulnerable Class             -- ^ Invulnerable to enemy 'Skill's
    | ImmuneSelf                     -- ^ Invulnerable to self-caused damage
    | Invincible   Class             -- ^ Like 'Invulnerable', but targetable
    | Isolate                        -- ^ Unable to affect others
    | Link         Int               -- ^ Increases damage and healing from source
    | Parry        Class Transform   -- ^ 'Counter' and trigger an effect
    | ParryAll     Class Transform   -- ^ 'Parry' repeatedly
    | Pierce                         -- ^ Damage skills turn into piercing
    | Plague                         -- ^ Invulnerable to healing and curing
    | Reduce       Class Amount Int  -- ^ Reduces damage by an amount
    | Reapply                        -- ^ Shares harmful skills with source
    | Redirect     Class             -- ^ Transfers harmful 'Skill's
    | Reflect                        -- ^ Reflects the first 'Skill'
    | ReflectAll                     -- ^ 'Reflect' repeatedly
    | Restrict                       -- ^ Forces AoE attacks to be single-target
    | Reveal                         -- ^ Makes 'Invisible' effects visible
    | Seal                           -- ^ Ignore all friendly 'Skill's
    | Share                          -- ^ Shares all harmful non-damage effects
    | Silence                        -- ^ Unable to cause non-damage effects
    | Snapshot     Ninja             -- ^ Saves a snapshot of the current state
    | Snare        Int               -- ^ Increases cooldowns
    | SnareTrap    Class Int         -- ^ Negates next skill and increases cooldown
    | Strengthen   Class Amount Int  -- ^ Adds to all damage dealt
    | Stun         Class             -- ^ Unable to use 'Skill's
    | Swap         Class             -- ^ Target swaps enemies and allies
    | Taunt                          -- ^ Forced to attack the source
    | Taunting     Int               -- ^ Forced to attack their next target
    | Threshold    Int               -- ^ Invulnerable to baseline damage below a threhold
    | Throttle (Class -> Effect) Int -- ^ Applying an effect lasts fewer turns
    | Undefend                       -- ^ Does not benefit from destructible defense
    | Uncounter                      -- ^ Cannot counter or reflect
    | Unexhaust                      -- ^ Decreases chakra costs by 1 random  
    | Unreduce     Int               -- ^ Reduces damage reduction 'Skill's
    | Weaken       Class Amount Int  -- ^ Lessens damage dealt
    -- | Copies a skill into source's skill slot
    | Copy { copyDuration :: Int 
           , copyClass    :: Class
           , copyTo       :: Int   -- ^ Skill index of source to copy into
           , copyNonHarm  :: Bool  -- ^ Include non-harmful 'Skill's
           }
    deriving (Eq)

low :: Class -> String
low = lower . show'
showAmt :: Amount -> Int -> String
showAmt Flat    = show
showAmt Percent = (++ "%") . show

instance Show Effect where
    show (Afflict x) = "Receives " ++ show x ++ " affliction damage each turn."
    show AntiCounter = "Cannot be countered or reflected."
    show (Bleed clas amt x)
      | x >= 0    =  showAmt amt x ++ " additional damage taken from " ++ low clas ++ " skills."
      | otherwise = "Reduces all " ++ low clas ++  " damage received by " ++ showAmt amt (-x) ++ "."
    show (Bless x) = "Healing skills heal 1 additional " ++ show x ++ " health."
    show Block = "Unable to affect the source of this effect."
    show (Boost x) = "Active effects from allies are " ++ show x ++ " times as powerful." 
    show (Build x)
      | x >= 0    = "Destructible skills provide " ++ show x ++ " additional points of defense."
      | otherwise =  "Destructible skills provide " ++ show (-x) ++ " fewer points of defense."
    show (Copy _ clas _ _) = show' clas ++ " skills will be temporarily acquired by the source of this effect."
    show (Counter All)  = "Counters the first skill."
    show (Counter clas) = "Counters the first " ++ low clas ++ "skill."
    show (CounterAll All) = "Counters all skills."
    show (CounterAll clas) = "Counters all " ++ low clas ++ "skills."
    show Undefend = "Unable to benefit from destructible defense"
    show Duel = "Invulnerable to everyone but the source of this effect."
    show Endure = "Health cannot go below 1."
    show Enrage = "Ignores harmful status effects other than chakra cost changes."
    show (Exhaust clas) = show' clas ++ " skills cost 1 additional random chakra."
    show Expose = "Unable to reduce damage or become invulnerable."
    show (Heal x) = "Gains " ++ show x ++ " health each turn."
    show (Ignore _) = "Ignores certain effects."
    show (Invulnerable clas) = "Invulnerable to " ++ low clas ++ " skills."
    show ImmuneSelf = "Invulnerable to self-damage."
    show (Invincible clas) = "Harmful " ++ low clas ++ " skills have no effect."
    show Isolate = "Unable to affect others."
    show (Link x) = "Receives " ++ show x ++ " additional damage from the source of this effect."
    show (Parry All _) = "Counters the first skill."
    show (Parry clas _) = "Counters the first " ++ low clas ++ " skill." 
    show (ParryAll All _) = "Counters all skill."
    show (ParryAll clas _) = "Counters all " ++ low clas ++ " skills." 
    show Pierce = "Non-affliction skills deal piercing damage."
    show Plague = "Cannot be healed or cured."
    show Reapply = "Harmful skills received are also reflected to the source of this effect."
    show (Reduce Affliction amt x)
      | x >= 0    = "Reduces all damage received—including piering and affliction—by " ++ showAmt amt x ++ "."
      | otherwise = "Increases all damage received—including piering and affliction—by " ++ showAmt amt x ++ "."
    show (Reduce clas amt x) 
      | x >= 0    = "Reduces " ++ low clas ++ " damage received by " ++ showAmt amt x ++ ". Does not affect piercing or affliction damage."
      | otherwise = "Increases " ++ low clas ++ " damage received by " ++ showAmt amt (-x) ++ ". Does not affect piercing or affliction damage."
    show (Redirect clas) = "Redirects " ++ low clas  ++ " harmful skills to the source of this effect."
    show Reflect = "Reflects the first harmful non-mental skill."
    show ReflectAll = "Reflects all non-mental skills."
    show Reveal = "Reveals invisible skills to the enemy team. This effect cannot be removed."
    show Restrict = "Skills that normally affect all opponents must be targeted."
    show Seal = "Invulnerable to effects from allies."
    show Share = "If a harmful non-damage effect is received, it is also applied to the source of this effect."
    show Silence = "Unable to cause non-damage effects."
    show (Snare x)
      | x >= 0    = "Cooldowns increased by " ++ show x ++ "."
      | otherwise = "Cooldowns decreased by " ++ show (-x) ++ "."
    show (SnareTrap _ _) = "Next skill used will be negated and go on a longer cooldown."
    show (Snapshot _) = "Will be restored to an earlier state when this effect ends."
    show (Strengthen clas amt x) = show' clas ++ " damaging skills deal " ++ showAmt amt x ++ " additional damage."
    show (Stun Affliction) = "Unable to deal affliction damage."
    show (Stun NonAffliction) = "Unable to deal non-affliction damage."
    show (Stun clas) = "Unable to use " ++ low clas ++ " skills."
    show (Swap clas) = "Next " ++ low clas ++ " skill will target allies instead of enemies and enemies instead of allies."
    show Taunt = "Forced to target the source of this effect."
    show (Taunting x) = "Will be forced to target the next enemy they use a skill on for " ++ show x ++ " turns."
    show (Threshold x) = "Uninjured by attacks that deal " ++ show x ++ " baseline damage or lower."
    show (Throttle _ x) = "Skills will apply " ++ show x ++ " fewer turns of certain effects."
    show Uncounter = "Unable to benefit from counters or reflects."
    show Unexhaust = "All skills cost 1 fewer random chakra."
    show (Unreduce x) = "Damage reduction skills reduce " ++ show x ++ " fewer damage."
    show (Weaken clas amt x) = show' clas ++ " skills deal " ++ showAmt amt x ++ " fewer damage. Does not affect affliction damage."
instance ToJSON Effect where 
    toJSON x = object
      [ "effectDesc"    .= tshow x 
      , "effectHelpful" .= helpful x
      , "effectSticky"  .= sticky x 
      , "effectTrap"    .= False
      ]

helpful :: Effect -> Bool
helpful Afflict{}      = False
helpful AntiCounter    = True
helpful (Bleed _ _ x)  = x < 0
helpful Bless{}        = True
helpful Block          = False
helpful Boost{}        = True
helpful (Build x)      = x >= 0
helpful Copy{}         = False
helpful Counter{}      = True
helpful CounterAll{}   = True
helpful Duel           = True
helpful Endure         = True
helpful Enrage         = True
helpful Exhaust{}      = False
helpful Expose         = False
helpful Heal{}         = True
helpful Invulnerable{} = True
helpful ImmuneSelf     = True
helpful Ignore{}       = True
helpful Invincible{}   = True
helpful Isolate        = False
helpful Link{}         = False
helpful Parry{}        = True
helpful ParryAll {}    = True
helpful Pierce         = True
helpful Plague         = False
helpful Reapply        = False
helpful (Reduce _ _ x) = x >= 0
helpful Redirect{}     = True
helpful Reflect        = True
helpful ReflectAll     = True
helpful Restrict       = False
helpful Reveal         = False
helpful Seal           = False
helpful Share          = False
helpful Silence        = False
helpful Snapshot{}     = True
helpful (Snare x)      = x < 0
helpful SnareTrap{}    = False
helpful Strengthen{}   = True
helpful Stun{}         = False
helpful Swap{}         = False
helpful Taunt          = False
helpful Threshold{}    = True
helpful Throttle{}     = False
helpful Taunting{}     = False
helpful Uncounter      = False
helpful Undefend       = False
helpful Unexhaust      = True
helpful Unreduce{}     = False
helpful Weaken{}       = False

-- | Effect cannot be removed.
sticky :: Effect -> Bool
sticky Block          = True
sticky Copy{}         = True
sticky Counter{}      = True
sticky CounterAll{}   = True
sticky Enrage         = True
sticky Invulnerable{} = True
sticky Invincible{}   = True
sticky Parry{}        = True
sticky ParryAll{}     = True
sticky Redirect{}     = True
sticky Reapply        = True
sticky Reflect        = True
sticky ReflectAll     = True
sticky Restrict       = True
sticky Reveal         = True
sticky Snapshot{}     = True
sticky Swap{}         = True
sticky _              = False


-- | Scales the power of an effect.
boost :: Int -> Effect -> Effect
boost b (Afflict  x) = Afflict  $ x * b
boost b (Build    x) = Build    $ x * b
boost b (Heal     x) = Heal     $ x * b
boost b (Snare    x) = Snare    $ x * b
boost b (Unreduce x) = Unreduce $ x * b
boost b (Bleed      c Flat x) = Bleed      c Flat $ x * b
boost b (Reduce     c Flat x) = Reduce     c Flat $ x * b
boost b (Strengthen c Flat x) = Strengthen c Flat $ x * b
boost b (Weaken     c Flat x) = Weaken     c Flat $ x * b
boost _ ef = ef

four0s :: Seq Int -- ^ [0, 0, 0, 0]
four0s = Seq.replicate 4 0

-- | A single action of a 'Ninja'.
data Act = Act { actC :: Slot               
               -- ^ User index in 'gameNinjas' (0-5)
               , actS :: Either Int Skill
               -- ^ Skill by index in 'nCharacter' 'characterSkills' (0-3)
               , actT :: Slot               
               -- ^ Target index in 'gameNinjas' (0-5)
               } deriving (Eq, Generic, ToJSON)
-- | Builds an 'Act' out of basic types. Used for 'PathPiece'.
data ActPath = ActPath { actPathC :: Int -- ^ 'actC'
                       , actPathS :: Int -- ^ Left 'actS'
                       , actPathT :: Int -- ^ 'actT'
                       } deriving (Eq, Show, Read)
instance PathPiece ActPath where
  toPathPiece ActPath{..} = pack . intercalate "," $
                            show <$> [actPathC, actPathS, actPathT]
  fromPathPiece raw   = case pieces of
      [c, s, t] -> case makeAct c s t of
                       Right act -> Just act
                       Left  _   -> Nothing
      _         -> Nothing
    where 
      pieces        = Text.splitOn "," raw
      makeAct c s t = [ActPath c' s' t' | (c',_) <- Read.decimal c
                                        , (s',_) <- Read.decimal s
                                        , (t',_) <- Read.decimal t
                                        ]
actFromPath :: ActPath -> Act
actFromPath ActPath{..} = Act (Slot actPathC) (Left actPathS) (Slot actPathT)

-- | Keeps track of what caused an action.
data Affected 
    = Applied
    | Channeled
    | Countered
    | Delayed 
    | Disrupted
    | Parrying
    | Redirected
    | Reflected
    | Swapped
    | Trapped
    deriving (Enum, Show, Eq)

-- | Destructible barrier.
data Barrier = Barrier { barrierAmount :: Int
                       , barrierSrc    :: Slot
                       , barrierL      :: Text
                       , barrierWhile  :: Game -> Game
                       , barrierDone   :: Int -> Game -> Game
                       , barrierDur    :: Int
                       } deriving (Eq, Generic, ToJSON)
instance TurnBased Barrier where 
    getDur     = barrierDur
    setDur d x = x { barrierDur = d }
instance Labeled Barrier where 
    getL   = barrierL
    getSrc = barrierSrc
instance Ord Barrier where
    compare = comparing getL

-- | Collection of all chakra types.
data Chakras = Chakras { blood :: Int -- ^ Bloodline
                       , gen   :: Int -- ^ Genjutsu
                       , nin   :: Int -- ^ Ninjutsu
                       , tai   :: Int -- ^ Taijutsu
                       , rand  :: Int -- ^ Random
                       } deriving (Eq, Show, Read, Generic, ToJSON)

chakraF :: (Int -> Int) -> Chakras -> Chakras
chakraF f (Chakras b g n t r) = Chakras (f b) (f g) (f n) (f t) (f r)

chakraF2 :: (Int -> Int -> Int) -> Chakras -> Chakras -> Chakras
chakraF2 f (Chakras b g n t r) (Chakras b' g' n' t' r') =
    Chakras (f b b') (f g g') (f n n') (f t t') (f r r')

instance Num Chakras where
    (+) = chakraF2 (+)
    (-) = chakraF2 (-)
    (*) = chakraF2 (*)
    negate = chakraF negate
    abs = chakraF abs
    signum = chakraF signum
    fromInteger x = Chakras x' x' x' x' x'
      where
        x' = fromInteger x

instance PathPiece Chakras where
  toPathPiece       = tshow
  fromPathPiece raw = case pieces of
      [b, g, n, t] -> case makeChakras b g n t of
                       Right chakras -> Just chakras
                       Left  _   -> Nothing
      _         -> Nothing
    where 
      pieces    = Text.splitOn "," raw
      makeChakras b g n t = [Chakras b' g' n' t' 0 | (b',_) <- Read.decimal b
                                                   , (g',_) <- Read.decimal g
                                                   , (n',_) <- Read.decimal n
                                                   , (t',_) <- Read.decimal t
                                                   ]

-- | Types of chakra in 'Chakras'.
data ChakraType 
    = Blood -- ^ Bloodline
    | Gen   -- ^ Genjutsu
    | Nin   -- ^ Ninjutsu
    | Tai   -- ^ Taijutsu
    | Rand  -- ^ Random
    deriving (Enum, Eq, Show)

-- | An 'Act' channeled over multiple turns.
data Channel = Channel { channelRoot  :: Slot
                       , channelSkill :: Skill
                       , channelT     :: Slot
                       , channelDur   :: Channeling
                       } deriving (Eq, Generic, ToJSON)
instance TurnBased Channel where 
    getDur     = getDur . channelDur
    setDur d x = x { channelDur = setDur d $ channelDur x }

-- | Types of channeling for 'Skill's.
data Channeling = Instant
                | Passive
                | Action Int
                | Control Int
                | Ongoing Int
                deriving (Eq, Show, Generic, ToJSON)
instance TurnBased Channeling where
    getDur Instant     = 0
    getDur Passive     = 0
    getDur (Action d)  = d
    getDur (Control d) = d
    getDur (Ongoing d) = d
    setDur _ Instant   = Instant
    setDur _ Passive   = Passive
    setDur d Action{}  = Action d
    setDur d Control{} = Control d
    setDur d Ongoing{} = Ongoing d

-- | Indicates that a channeled 'Skill' will affect a 'Ninja' next turn.
data ChannelTag = ChannelTag { tagRoot    :: Slot
                             , tagSrc     :: Slot 
                             , tagSkill   :: Skill
                             , tagGhost   :: Bool
                             , tagDur     :: Int
                             } deriving (Eq, Generic, ToJSON)
instance TurnBased ChannelTag where 
    getDur     = tagDur
    setDur d x = x { tagDur = d }
instance Labeled ChannelTag where
    getL   = label . tagSkill
    getSrc = tagRoot

-- | An out-of-game character.
data Character = Character { characterName   :: Text
                           , characterBio    :: Text
                           , characterSkills :: NonEmpty (NonEmpty Skill)
                           , characterHooks  :: [(Trigger, Int -> Ninja -> Ninja)]
                           , characterGroup  :: Group
                           } deriving (Generic, ToJSON)
instance Eq Character where
    (==) = andOn [eqs characterName, eqs characterGroup]

instance Show Character where
    show (Character name _ _ _ Original)   = Text.unpack name
    show (Character name _ _ _ Shippuden)  = Text.unpack name ++ " (S)"
    show (Character name _ _ _ Reanimated) = Text.unpack name ++ " (R)"

-- | A 'Skill' copied from a different character.
data Copied = Copied { copiedSkill :: Skill
                     , copiedDur   :: Int
                     } deriving (Eq, Generic, ToJSON)
instance TurnBased Copied where 
    getDur     = copiedDur
    setDur d x@Copied{..} = x { copiedDur = d
                              , copiedSkill = f $ copying copiedSkill 
                              }
        where 
          f (Shallow b _) = copiedSkill { copying = Shallow b d }
          f (Deep    b _) = copiedSkill { copying = Deep    b d }
          f NotCopied     = copiedSkill

data Copying 
    = Shallow Slot Int -- ^ No cooldown or chakra cost.
    | Deep Slot Int    -- ^ Cooldown and chakra cost.
    | NotCopied
    deriving (Eq, Generic, ToJSON)

-- | Destructible defense.
data Defense = Defense { defenseAmount :: Int
                       , defenseSrc    :: Slot
                       , defenseL      :: Text
                       , defenseDur    :: Int
                       } deriving (Eq, Generic, ToJSON)
instance TurnBased Defense where 
    getDur     = defenseDur
    setDur d x = x { defenseDur = d }
instance Labeled Defense where 
    getL   = defenseL
    getSrc = defenseSrc

-- | Applies an effect after several turns.
data Delay = Delay { delayC     :: Slot
                   , delaySkill :: Skill
                   , delayEf    :: Game -> Game
                   , delayDur   :: Int
                   } deriving (Eq)
instance TurnBased Delay where 
    getDur     = delayDur
    setDur d x = x { delayDur = d }
instance Labeled Delay where 
    getL   = label . delaySkill
    getSrc = delayC

-- | Changes the character icon of a 'Ninja'.
data Face = Face { faceIcon :: Text
                 , faceSrc  :: Slot
                 , faceDur  :: Int
                 } deriving (Eq, Generic, ToJSON)
instance TurnBased Face where 
    getDur     = faceDur
    setDur d x = x { faceDur = d }

data Flag 
    = Acted
    | Harmed
    | Targeted
    deriving (Show, Eq, Ord, Bounded, Enum)
instance Hashable Flag where
    hashWithSalt x = hashWithSalt x . fromEnum

-- | Game state.
data Game = Game { gamePlayers :: (Key User, Key User)
                 , gameNinjas  :: Seq Ninja 
                 , gameChakra  :: (Chakras, Chakras)
                 -- ^ Starts at @('Chakras' 0 0 0 0 0, 'Chakras' 0 0 0 0 0)@.
                 , gameDelays  :: [Delay]
                 -- ^ Starts empty.
                 , gameDrain   :: (Int, Int)
                 -- ^ Starts at @(0, 0)@. Resets every turn to @(0, 0)@.
                 , gameSteal   :: (Int, Int)
                 -- ^ Starts at @(0, 0)@. Resets every turn to @(0, 0)@.
                 , gameTraps   :: Seq (Game -> Game)
                 -- ^ Starts empty.
                 , gamePlaying :: Player 
                 -- ^ Starts at 'PlayerA'.
                 , gameVictor  :: Maybe Victor
                 -- ^ Starts at 'Nothing'.
                 , gameMock    :: Bool
                 -- ^ If True, this is a fake game used for testing purposes 
                 -- and all conditional statements always return True.
                 } deriving (Eq)
-- Obtains the 'Ninja' in a 'Slot'.
gameNinja :: Slot -> Game -> Ninja
gameNinja (Slot i) Game{..} = gameNinjas `Seq.index` i
-- Updates the 'Ninja' in a 'Slot'.
setNinja :: Slot -> Ninja -> Game -> Game
setNinja (Slot i) n game@Game{..} = 
    game { gameNinjas = Seq.update i n gameNinjas }
-- Adjusts the 'Ninja' in a 'Slot'.
fN :: Slot -> (Ninja -> Ninja) -> Game -> Game
fN (Slot i) f game@Game{..} = 
    game { gameNinjas = Seq.adjust' f i gameNinjas }

-- | Constructs a 'Game' with starting values.
newGame :: Key User -> Key User -> [Character] -> Game
newGame a b ns = Game { gamePlayers = (a, b)
                      , gameNinjas  = Seq.fromList $ 
                                      zipWith newNinja ns allSlots
                      , gameChakra  = (0, 0)
                      , gameDelays  = []
                      , gameDrain   = (0, 0)
                      , gameSteal   = (0, 0)
                      , gameTraps   = mempty
                      , gamePlaying = PlayerA
                      , gameVictor  = Nothing
                      , gameMock    = False
                      }

mockSlot :: Slot
mockSlot = Slot 0

-- | 'Original', 'Shippuden', or 'Reanimated'.
data Group 
    = Original 
    | Shippuden 
    | Reanimated 
    deriving (Show, Eq, Ord, Bounded, Enum, Generic, ToJSON)

-- | In-game character, indexed between 0 and 5.
data Ninja = Ninja { nId        :: Slot           -- ^ 'gameNinjas' index (0-5)
                   , nCharacter :: Character
                   , nHealth    :: Int                    -- ^ `Starts at` @100@
                   , nCooldowns :: Seq (Seq Int)          -- ^ Starts empty
                   , nCharges   :: Seq Int                -- ^ Starts at 4 @0@s
                   , nVariants  :: Seq (NonEmpty Variant) -- ^ Starts at 4 @0@s
                   , nCopied    :: Seq (Maybe Copied)     -- ^ Starts at 4 'Nothing's
                   , nDefense   :: [Defense]              -- ^ Starts empty
                   , nBarrier   :: [Barrier]              -- ^ Starts empty
                   , nStatuses  :: [Status]               -- ^ Starts empty
                   , nChannels  :: [Channel]              -- ^ Starts empty
                   , newChans   :: [Channel]              -- ^ Starts empty
                   , nTraps     :: Seq Trap               -- ^ Starts empty
                   , nFace      :: [Face]                 -- ^ Starts empty
                   , nParrying  :: [Skill]                -- ^ Starts empty
                   , nTags      :: [ChannelTag]           -- ^ Starts empty
                   , nLastSkill :: Maybe Skill            -- ^ Starts at 'Nothing'
                   , nFlags     :: HashSet Flag           -- ^ Empty each turn
                   }
instance Eq Ninja where
    -- | Omitted: nParrying, nTags, nLastSkill, nFlags
    (==) = andOn [ eqs nId, eqs nCharacter, eqs nHealth, eqs nCooldowns
                 , eqs nCharges, eqs nVariants, eqs nCopied, eqs nDefense
                 , eqs nBarrier, eqs nStatuses, eqs nChannels, eqs newChans
                 , eqs nTraps, eqs nFace
                 ]

insertCd' :: Int -> Int -> Seq Int -> Seq Int
insertCd' v toCd cds
  | len > v   = Seq.update v toCd cds
  | otherwise = (cds ++ Seq.replicate (v - len) 0) |> toCd
  where 
    len = length cds

insertCd :: Int -- ^ 'Skill' index (0-3).
         -> Int -- ^ 'Variant' index in 'characterSkills' of 'nCharacter'.
         -> Int -- ^ New cooldown.
         -> Seq (Seq Int)
         -> Seq (Seq Int)
insertCd s v toCd cds
  | len > s   = Seq.adjust' (insertCd' v toCd) s cds
  | otherwise = (cds ++ Seq.replicate (s - len) (Seq.singleton 0)) 
                |> insertCd' v toCd mempty
  where 
    len = length cds

adjustCd' :: Int -> (Int -> Int) -> Seq Int -> Seq Int
adjustCd' v f cds
  | len > v   =  Seq.adjust' f v cds
  | otherwise = (cds ++ Seq.replicate (v - len) 0) |> f 0
  where 
    len = length cds

adjustCd :: Int -- ^ 'Skill' index (0-3).
         -> Int -- ^ 'Variant' index in 'characterSkills' of 'nCharacter'.
         -> (Int -> Int) -- ^ Adjustment function.
         -> Seq (Seq Int) -> Seq (Seq Int)
adjustCd s v f cds
  | len > s   = Seq.adjust' (adjustCd' v f) s cds
  | otherwise = (cds ++ Seq.replicate (s - len) (Seq.singleton 0)) 
                |> adjustCd' v f mempty
  where 
    len = length cds

-- | Constructs a 'Ninja' with starting values from a character and an index.
newNinja :: Character -> Slot -> Ninja
newNinja c nId = Ninja { nId        = nId
                       , nHealth    = 100
                       , nCharacter = c
                       , nDefense   = []
                       , nBarrier   = []
                       , nStatuses  = []
                       , nCharges   = four0s
                       , nCooldowns = mempty
                       , nVariants  = Seq.replicate 4 (noVariant:|[])
                       , nCopied    = Seq.replicate 4 Nothing
                       , nChannels  = []
                       , newChans   = []
                       , nTraps     = mempty
                       , nFace      = []
                       , nParrying  = []
                       , nTags      = []
                       , nLastSkill = Nothing
                       , nFlags     = mempty
                       }

-- | Factory resets a 'Ninja' to its starting values.
ninjaReset :: Ninja -> Ninja
ninjaReset Ninja{..} = newNinja nCharacter nId

-- | Player vs. opponent.
data Player = PlayerA | PlayerB deriving (Enum, Show, Eq)
instance ToJSON Player where 
    toJSON = toJSON . fromEnum
data Victor = VictorA | VictorB | Tie deriving (Enum, Show, Eq)
instance ToJSON Victor where 
    toJSON = toJSON . fromEnum

data Requirement 
    = Usable
    | Unusable
    | HasI Int Text
    | HasU Text 
    deriving (Eq, Generic, ToJSON)

-- | A move that a 'Character' can perform.
data Skill = Skill  { label   :: Text -- ^ Name
                    , desc    :: Text -- ^ Description
                    , require :: Requirement   -- ^ Defaults to 'Usable'
                    , classes :: [Class]       -- ^ Defaults to @[]@
                    , cost    :: Chakras       -- ^ Defaults to 'S.empty'
                    , cd      :: Int           -- ^ Defaults to @0@
                    , varicd  :: Bool          -- ^ Defaults to @False@
                    , charges :: Int           -- ^ Defaults to @0@
                    , channel :: Channeling    -- ^ Defaults to 'Instant'
                    , start   :: [(Target, Transform)] -- ^ Defaults to @[]@
                    , effects :: [(Target, Transform)] -- ^ Defaults to @[]@
                    , disrupt :: [(Target, Transform)] -- ^ Defaults to @[]@
                    , copying :: Copying       -- ^ Defaults to 'NotCopied'
                    , skPic   :: Bool          -- ^ Defaults to @False@
                    , changes :: Ninja -> Skill -> Skill -- ^ Defaults to 'id'
                    } deriving (Generic, ToJSON)
instance Eq Skill where
    (==) = andOn [eqs label, eqs desc]

-- | Default values of a 'Skill'. Used as a 'Skill' constructor.
newSkill :: Skill
newSkill = Skill { label   = "Unnamed"
                 , desc    = ""
                 , require = Usable
                 , classes = []
                 , cost    = 0
                 , cd      = 0
                 , varicd  = False
                 , charges = 0
                 , channel = Instant
                 , start   = []
                 , effects = []
                 , disrupt = []
                 , changes = const id
                 , copying = NotCopied
                 , skPic   = False
                 }

-- | Applies 'Transform's when a 'Status' ends.
data Bomb 
    = Done   -- ^ Applied with both 'Expire' and 'Remove'
    | Expire -- ^ Applied when a 'Status' reaches the end of its duration.
    | Remove -- ^ Applied when a 'Status' is removed prematurely
    deriving (Enum, Eq, Show, Generic, ToJSON)

-- | A status effect affecting a 'Ninja'.
data Status = Status { statusCount   :: Int  -- ^ Starts at 1
                     , statusL       :: Text -- ^ Label
                     , statusRoot    :: Slot -- ^ Owner of the 'statusSkill'
                     , statusSrc     :: Slot -- ^ Original user
                     , statusC       :: Slot -- ^ Direct user (e.g. if reflected)
                     , statusSkill   :: Skill
                     , statusEfs     :: [Effect]
                     , statusClasses :: [Class]
                     , statusBombs   :: [(Bomb, Transform)]
                     , statusMaxDur  :: Int
                     , statusDur     :: Int
                     } deriving (Generic, ToJSON)
instance Eq Status where
    (==) = andOn 
          [eqs statusL, eqs statusSrc, eqs statusMaxDur, eqs statusClasses]
instance TurnBased Status where 
    getDur     = statusDur
    setDur d x = x { statusDur = d }
instance Labeled Status where
    getL   = statusL
    getSrc = statusSrc
instance Ord Status where
    compare = comparing getL

-- | Target destinations of 'Skill's.
data Target 
    = Self          -- ^ User of 'Skill'
    | Ally          -- ^ Specific ally
    | Allies        -- ^ All allies
    | RAlly         -- ^ Random ally
    | XAlly         -- ^ Specific ally excluding 'Self'
    | XAllies       -- ^ 'Allies' excluding 'Self'
    | Enemy         -- ^ Specific enemy
    | Enemies       -- ^ All enemies
    | REnemy        -- ^ Random enemy
    | XEnemies      -- ^ Enemies excluding 'Enemy'
    | Everyone      -- ^ All 'Ninja's
    | Specific Slot -- ^ Specific ninja index in 'gameNinjas' (0-5)
    deriving (Eq, Generic, ToJSON)

data TrapType 
    = TrapTo 
    | TrapFrom 
    | TrapPer 
    deriving (Enum, Eq, Generic, Show, ToJSON)

-- | A trap which gets triggered when a 'Ninja' meets the conditions of a 'Trigger'.
data Trap = Trap { trapType    :: TrapType
                 , trapTrigger :: Trigger
                 , trapL       :: Text
                 , trapDesc    :: Text
                 , trapSrc     :: Slot
                 , trapEf      :: TrapTransform
                 , trapClasses :: [Class]
                 , trapTrack   :: Int
                 , trapDur     :: Int
                 } deriving (Generic, ToJSON)
instance Eq Trap where
    (==) = andOn 
           [eqs trapType, eqs trapTrigger, eqs trapL, eqs trapSrc, eqs trapDur]
instance TurnBased Trap where 
    getDur     = trapDur
    setDur d x = x { trapDur = d }
instance Labeled Trap where
    getL   = trapL
    getSrc = trapSrc

-- | Conditions to activate a 'Trap'.
data Trigger 
    = OnAction Class
    | OnNoAction
    | OnBreak Text
    | OnChakra
    | OnCounter Class
    | OnCounterAll
    | OnDamage
    | OnDamaged Class
    | OnDeath
    | OnHarm    
    | OnNoHarm
    | OnHarmed Class
    | OnHealed
    | PerHealed
    | OnHelped
    | OnImmune
    | OnReflectAll
    | OnRes
    | OnStun
    | OnStunned
    | PerDamage
    | PerDamaged
    | TrackDamage
    | TrackDamaged
    deriving (Eq)
instance ToJSON Trigger where
    toJSON = toJSON . tshow
instance Show Trigger where
    show (OnAction  All) = "Trigger: Use any skill"
    show (OnAction  cla) = "Trigger: Use " ++ low cla ++ " skills"
    show (OnBreak   l)   = "Trigger: Lose all destructible defense from '" ++ unpack l ++ "'"
    show OnChakra        = "Trigger: Steal or remove chakra" 
    show (OnCounter All) = "Next harmful skill is countered."
    show (OnCounter Uncounterable) = "Next skill is negated."
    show (OnCounter cla) = "Next harmful " ++ low cla ++ " skill is countered."
    show OnCounterAll    = "All skills are countered."
    show OnDamage        = "Trigger: Deal damage"
    show (OnDamaged All) = "Trigger: Receive damage"
    show (OnDamaged cla) = "Trigger: Receive " ++ low cla ++ " damage"
    show OnDeath         = "Trigger: Die"
    show OnHarm          = "Trigger: Use harmful skill"
    show (OnHarmed All)  = "Trigger: Be affected by a new harmful skill"
    show (OnHarmed cla)  = "Trigger: Be affected by a new " ++ low cla ++ " harmful skill"
    show OnHealed        = "Trigger: Receive healing"
    show OnHelped        = "Trigger: Be affected by a new skill from an ally"
    show OnImmune        = "Trigger: Become invulnerable"
    show OnNoAction      = "Trigger: Do not use a new skill"
    show OnNoHarm        = "Trigger: Do not use a new harmful skill"
    show OnReflectAll    = "All skills are reflected."
    show OnRes           = "Trigger: Reach 0 health"
    show OnStun          = "Trigger: Apply a stun"
    show OnStunned       = "Trigger: Stunned"
    show PerDamage       = show OnDamage
    show PerDamaged      = show (OnDamaged All)
    show PerHealed       = show OnHealed
    show TrackDamage     = show OnDamage
    show TrackDamaged    = show PerDamaged

data Variant = Variant { variantV    :: Int -- ^ Index in 'characterSkills'
                       , variantVCD  :: Bool -- ^ Uses a different cooldown than the baseline 'Skill'
                       , variantL    :: Text
                       , variantFrom :: Bool -- ^ Duration is based on a 'Skill'
                       , variantDur  :: Int
                       } deriving (Eq, Show, Generic, ToJSON)
instance TurnBased Variant where 
    getDur        = variantDur
    setDur x vari = vari { variantDur = x }
variantCD :: Variant -> Int
variantCD Variant{..}
  | variantVCD = variantV
  | otherwise  = 0

noVariant :: Variant
noVariant = Variant 0 False "" False 0

-- * Slots

-- ^ @Int@ newtype for indices in 'gameNinjas'.
newtype Slot = Slot Int deriving (Eq)
instance ToJSON Slot where
  toJSON (Slot i) = toJSON i

-- ^ Parity (modulo 2).
par :: Int -> Int -- ^ ٪ 2
par = (`mod` 2)

-- ^ Applies a function to the first or second in a pair by parity.
bySlot :: ∀ a. Slot -> (a -> a) -> (a, a) -> (a, a)
bySlot (Slot x) = do2 $ even x
-- ^ Obtains the first or second in a pair by parity.
outSlot :: ∀ a. Slot -> (a, a) -> a
outSlot (Slot x) = out2 $ even x
-- ^ Inverse of 'outSlot'.
outSlot' :: ∀ a. Slot -> (a, a) -> a
outSlot' (Slot x) = out2 $ odd x

-- ^ Partition by parity.
spar :: Slot -> Int
spar (Slot x) = x `mod` 2

allied' :: Int -> Int -> Bool
allied' x y = even x == even y
allied :: Slot -> Slot -> Bool
allied (Slot x) (Slot y) = allied' x y
alliedP :: Player -> Slot -> Bool
alliedP p (Slot nId) = allied' (fromEnum p) nId

allies' :: ∀ o. Mono o Ninja => Int -> o -> [Ninja]
allies' p = evens . drop (par p) . toList
allies :: Slot -> Game -> [Ninja]
allies (Slot p) = allies' p . gameNinjas
alliesP :: ∀ o. Mono o Ninja => Player -> o -> [Ninja]
alliesP = allies' . fromEnum
enemies' :: ∀ o. Mono o Ninja => Int -> o -> [Ninja]
enemies' p = evens . drop (1 - par p) . toList
enemies :: Slot -> Game -> [Ninja]
enemies (Slot p) = enemies' p . gameNinjas
enemiesP :: ∀ o. Mono o Ninja => Player -> o -> [Ninja]
enemiesP = enemies' . fromEnum

-- ^ @[0 .. gameSize - 1]@.
allSlots :: [Slot]
allSlots = Slot <$> [ 0 .. gameSize - 1]

allySlots' :: Int -> [Slot]
allySlots' x = Slot <$> [ x',  2 + x' .. gameSize - 1]
  where 
    x' = x `mod` 2
allySlots :: Slot -> [Slot]
allySlots (Slot x) = allySlots' x
enemySlots' :: Int -> [Slot]
enemySlots' x = Slot <$> [1 - x', 3 - x' .. gameSize - 1]
  where 
    x' = x `mod` 2
enemySlots :: Slot -> [Slot]
enemySlots (Slot x) = enemySlots' x

opponentSlots :: Player -> [Slot]
opponentSlots = enemySlots' . fromEnum

-- | Selects every other element from a @List@.
evens :: [a] -> [a]
evens [x]      = [x]
evens (x:_:xs) = x : evens xs
evens []       = []

-- | Second argument if both arguments have the same parity, otherwise @[]@.
ifPar :: Int -> Int -> [Int]
ifPar c t = [ t | c <= gameSize, t <= gameSize, allied' c t ]

-- | Translates a 'Target' into a list of 'Ninja's.
choose :: (Maybe Slot, Maybe Slot) -> Target -> Slot -> Slot -> [Slot]
choose (a, e) targ (Slot c) (Slot t) = Slot <$> choose' (ms a, ms e) targ c t
  where 
    ms (Just (Slot s)) = Just s
    ms Nothing         = Nothing

choose' :: (Maybe Int, Maybe Int) -> Target -> Int -> Int -> [Int]
choose' _      Self     c _ = [c]
choose' _      Ally     c t = ifPar c t
choose' _      Allies   c _ = [par c, 2 + par c .. gameSize - 1]
choose' (r, _) RAlly    _ _ = maybeToList r
choose' _      XAlly    c t = delete c $ ifPar c t
choose' _      Enemy    c t = ifPar (c + 1) t
choose' _      Enemies  c _ = [1 - par c, 3 - par c .. gameSize-1]
choose' (_, r) REnemy   _ _ = maybeToList r
choose' _      Everyone _ _ = [0 .. gameSize - 1]
choose' _      XAllies  c _ = delete c [par c, 2 + par c .. gameSize - 1]  
choose' _      XEnemies c t = delete t [1 - par c, 3 - par c .. gameSize-1]
choose' _ (Specific (Slot x)) _ _ = [x]

-- | Actions of AI in training mode.
botActs :: [Act]
botActs = [ Act (Slot 3) (Left 1) (Slot 2)
          , Act (Slot 5) (Left 1) (Slot 5) 
          , Act (Slot 1) (Left 3) (Slot 1)
          ]

-- | All targets that a 'Skill' from a a specific 'Ninja' affects.
skillTargets :: Skill -> Slot -> [Slot]
skillTargets Skill{..} c = filter target $ Slot <$> [0 .. gameSize - 1]
  where 
    ts = fst <$> (start ++ effects ++ disrupt)
    harm = [Enemy, Enemies, REnemy, XEnemies] `intersects` ts
    target t 
      | Everyone `elem` ts = True
      | not $ allied c t = harm
      | [XAlly, XAllies] `intersects` ts = c /= t
      | [Ally, Allies, RAlly] `intersects` ts = True
      | c == t = not harm
      | otherwise = False
