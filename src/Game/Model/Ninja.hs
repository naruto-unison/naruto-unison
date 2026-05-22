module Game.Model.Ninja
  ( Ninja(..), new
  , numSkills
  , alive, minHealth
  , is, isChanneling
  , has, hasBarrier, hasDefense, hasOwnDefense, hasOwn
  , numActive, numStacks, numHelpful, numHarmful
  , defenseAmount, totalDefense, totalBarrier
  , lastChakraSpent
  , baseSkill
  ) where

import ClassyPrelude

import qualified  Data.List.NonEmpty as NonEmpty

import qualified Class.Labeled as Labeled
import qualified Class.Parity as Parity
import qualified Game.Model.Barrier as Barrier
import           Game.Model.Chakras (Chakras)
import           Game.Model.Class (Class(..))
import           Game.Model.Defense (Defense(Defense))
import qualified Game.Model.Defense as Defense
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Effect as Effect
import           Game.Model.Internal (Ninja(..), Channel(Channel), Character(Character), Skill(Skill), Status(Status))
import qualified Game.Model.Internal
import qualified Game.Model.Internal.Character as Character
import qualified Game.Model.Internal.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Internal.Status as Status
import           Util ((∈), (∉), (!?))

-- | Number of 'Skill' slots. This number is the boundary on quite a few things,
-- most notably action messages from the client (in 'Game.Action.act').
numSkills :: Ninja -> Int
numSkills Ninja{character = Character{skills}} = length skills

-- | Constructs a @Ninja@ with starting values from a character and an index.
new :: Slot -> Character -> Ninja
new slot c@Character{skills} = Ninja
    { slot
    , health     = 100
    , character  = c { Character.skills = (own <$>) <$> skills }
    , defense    = mempty
    , barrier    = mempty
    , statuses   = mempty
    , charges    = mempty
    , cooldowns  = mempty
    , alternates = replicate skillSize 0
    , copies     = replicate skillSize Nothing
    , channels   = mempty
    , newChans   = mempty
    , traps      = mempty
    , delays     = mempty
    , lastSkill  = Nothing
    , triggers   = mempty
    , effects    = mempty
    , acted      = False
    }
  where
    own x     = x { Skill.owner = slot }
    skillSize = length skills

-- | @alive n = health n > 0@
alive :: Ninja -> Bool
alive Ninja{health} = health > 0

-- | Searches 'effects'.
is :: Ninja -> Effect -> Bool
is Ninja{effects} ef = ef ∈ effects

-- | Searches 'channels'.
isChanneling :: Text -- ^ 'Skill.name'.
             -> Ninja -> Bool
isChanneling name n = any matches $ channels n
  where
    matches (Channel Skill{name = skillName} _ _) = name == skillName

-- | Searches 'statuses'.
has :: Text -- ^ 'Status.name'.
    -> Slot -- ^ 'Status.user'.
    -> Ninja -> Bool
has name user Ninja{statuses} = any (Labeled.match name user) statuses

-- | Searches 'barrier'.
hasBarrier :: Text -- ^ 'Barrier.name'.
           -> Slot -- ^ 'Barrier.user'.
           -> Ninja -> Bool
hasBarrier name user Ninja{barrier} = any (Labeled.match name user) barrier

-- | Searches 'defense'.
hasDefense :: Text -- ^ 'Defense.name'.
           -> Slot -- ^ 'Defense.user'.
           -> Ninja -> Bool
hasDefense name user Ninja{defense} = any (Labeled.match name user) defense

-- | Sums 'Defense.amount' of all matching 'defense'.
defenseAmount :: Text -- ^ 'Defense.name'.
              -> Slot -- ^ 'Defense.user'.
              -> Ninja -> Int
defenseAmount name user Ninja{defense} = sum
    [amount | d@Defense{amount} <- defense
            , Labeled.match name user d]

-- | Chakra spent on 'lastSkill'.
lastChakraSpent :: Ninja -> Chakras
lastChakraSpent Ninja{lastSkill = Just Skill{cost}} = cost
lastChakraSpent _                                   = mempty

-- | Sums 'Defense.amount' of all 'defense'.
totalDefense :: Ninja -> Int
totalDefense Ninja{defense} = sum $ Defense.amount <$> defense

-- | Sums 'Barrier.amount' of all 'barrier'.
totalBarrier :: Ninja -> Int
totalBarrier Ninja{barrier} = sum $ Barrier.amount <$> barrier

-- | Matches a 'Defense.Defense'.
hasOwnDefense :: Text -> Ninja -> Bool
hasOwnDefense name n = hasDefense name (slot n) n

-- | Matches a 'Status.Status'.
hasOwn :: Text -> Ninja -> Bool
hasOwn name n = has name (slot n) n

-- | Number of stacks of matching self-applied 'statuses'.
numActive :: Text -- ^ 'Status.name'.
          -> Ninja -> Int
numActive name n
  | stacks > 0           = stacks
  | isChanneling name n  = 1
  | hasOwnDefense name n = 1
  | otherwise            = 0
  where
    stacks = numStacks name (slot n) n

-- | Number of stacks of matching 'statuses'.
numStacks :: Text -- ^ 'Status.name'.
          -> Slot -- ^ 'Status.user'.
          -> Ninja -> Int
numStacks name user Ninja{statuses} = sum
    $ Status.amount <$> filter (Labeled.match name user) statuses

-- | Counts all 'Effect.helpful' effects in 'statuses' from allies.
-- Does not include self-applied or 'Hidden' 'Status.Status'es.
-- Each status counts for @(number of helpful effects) * (Status.amount)@.
numHelpful :: Ninja -> Int
numHelpful n = sum
    [amount | Status{amount, classes, effects, user} <- statuses n
            , slot n /= user
            , Parity.allied n user
            , Hidden ∉ classes
            , ef <- effects
            , Effect.helpful ef]

-- | Counts all non-'Effect.helpful' effects in 'statuses'.
-- Does not include self-applied or 'Hidden' 'Status.Status'es.
-- Each status counts for @(number of harmful effects) * (Status.amount)@.
numHarmful :: Ninja -> Int
numHarmful n = sum
    [amount | Status{amount, classes, effects, user} <- statuses n
            , slot n /= user
            , Hidden ∉ classes
            , ef <- effects
            , not $ Effect.helpful ef]

-- | @1@ if affected by 'Endure', otherwise @0@.
minHealth :: Ninja -> Int
minHealth n
  | n `is` Endure = 1
  | otherwise     = 0

-- | Obtains a @Skill@ from 'skills' by slot index, if it exists.
baseSkill :: Int -> Ninja -> Maybe Skill
baseSkill s Ninja{alternates, character = Character{skills}} = do
    skill     <- skills !:? s
    alternate <- alternates !? s
    skill !:? alternate
  where
    -- (!?) for NonEmpty
    xs !:? i = headMay $ NonEmpty.drop i xs
    {-# INLINE (!:?) #-}
