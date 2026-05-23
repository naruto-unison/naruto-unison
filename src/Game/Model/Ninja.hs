module Game.Model.Ninja
  ( Ninja(..), new
  , numSkills
  , alive, minHealth
  , is, isChanneling
  , has, hasDefense, hasOwn
  , numActive, numStacks, numHelpful, numHarmful
  , defenseAmount, totalDefense, totalBarrier
  , lastChakraSpent
  , baseSkill
  ) where

import ClassyPrelude

import qualified  Data.List.NonEmpty as NonEmpty

import           Class.Labeled (Labeled)
import qualified Class.Labeled as Labeled
import qualified Class.Parity as Parity
import           Game.Model.Chakras (Chakras)
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Effect as Effect
import           Game.Model.Internal (Destructible(..), Ninja(..), Channel(Channel), Character(Character), Skill(Skill), Status(Status))
import qualified Game.Model.Internal
import qualified Game.Model.Internal.Character as Character
import qualified Game.Model.Internal.Destructible as Destructible
import qualified Game.Model.Internal.Skill as Skill
import           Game.Model.Slot (Slot)
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
    matches (Channel Skill{name = skillName} _ False _) = name == skillName
    matches _                                           = False

has' :: ∀ a. Labeled a
     => (Ninja -> [a])
     -> Text -- ^ 'Status.name'.
     -> Slot -- ^ 'Status.user'.
     -> Ninja -> Bool
has' getter name user n = any (Labeled.match name user) $ getter n

-- | Searches 'statuses'.
has :: Text -- ^ 'Status.name'.
    -> Slot -- ^ 'Status.user'.
    -> Ninja -> Bool
has = has' statuses

-- | Searches 'defense'.
hasDefense :: Text -- ^ 'Destructible.name'.
           -> Slot -- ^ 'Destructible.user'.
           -> Ninja -> Bool
hasDefense = has' defense

hasOwn' :: ∀ a. Labeled a
     => (Ninja -> [a])
     -> Text -- ^ 'Status.name'.
     -> Ninja -> Bool
hasOwn' getter name n@Ninja{slot} = has' getter name slot n

-- | Matches a 'Status.Status'.
hasOwn :: Text -> Ninja -> Bool
hasOwn = hasOwn' statuses

-- | Sums 'Destructible.amount' of all matching 'defense'.
defenseAmount :: Text -- ^ 'Destructible.name'.
              -> Slot -- ^ 'Destructible.user'.
              -> Ninja -> Int
defenseAmount name user Ninja{defense} = sum
    [amount | d@Destructible{amount} <- defense
            , Labeled.match name user d]

-- | Chakra spent on 'lastSkill'.
lastChakraSpent :: Ninja -> Chakras
lastChakraSpent Ninja{lastSkill = Just Skill{cost}} = cost
lastChakraSpent _                                   = mempty

-- | Sums 'Destructible.amount' of all 'defense'.
totalDefense :: Ninja -> Int
totalDefense Ninja{defense} = sum $ Destructible.amount <$> defense

-- | Sums 'Destructible.amount' of all 'barrier'.
totalBarrier :: Ninja -> Int
totalBarrier Ninja{barrier} = sum $ Destructible.amount <$> barrier

-- | Number of stacks of matching self-applied 'statuses'.
numActive :: Text -- ^ 'Status.name'.
          -> Ninja -> Int
numActive name n
  | stacks > 0             = stacks
  | isChanneling name n    = 1
  | hasOwn' defense name n = 1
  | otherwise              = 0
  where
    stacks = numStacks name (slot n) n

-- | Number of stacks of matching 'statuses'.
numStacks :: Text -- ^ 'Status.name'.
          -> Slot -- ^ 'Status.user'.
          -> Ninja -> Int
numStacks name user Ninja{statuses} = sum
    [amount | st@Status{amount} <- statuses
            , Labeled.match name user st]

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
