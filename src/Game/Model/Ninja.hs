module Game.Model.Ninja
  ( Ninja(..), new
  , alive
  , is, isChanneling
  , has, has', hasBarrier, hasDefense, hasOwn, hasOwn'
  , numStacks, numHelpful, numHarmful
  , barrierAmount, defenseAmount, totalDefense, totalBarrier
  , lastChakraSpent
  ) where

import ClassyPrelude

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
import           Util ((∈), (∉))

-- | Constructs a @Ninja@ with starting values from a character and an index.
new :: Slot -> Character -> Ninja
new slot c@Character{skills} = Ninja
    { slot
    , health     = 100
    , character  = c { Character.skills = skills' }
    , defense    = mempty
    , barrier    = mempty
    , statuses   = mempty
    , charges    = mempty
    , cooldowns  = mempty
    , skills     = toNullable $ head <$> skills'
    , copies     = replicate skillSize Nothing
    , channels   = mempty
    , traps      = mempty
    , lastSkill  = Nothing
    , triggers   = mempty
    , effects    = mempty
    , face       = Nothing
    , acted      = False
    }
  where
    skills'   = (own <$>) <$> skills
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

-- | Searches 'barrier'.
hasBarrier :: Text -- ^ 'Destructible.name'.
           -> Slot -- ^ 'Destructible.user'.
           -> Ninja -> Bool
hasBarrier = has' barrier

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

-- | Sums 'Destructible.amount' of all matching 'barrier' or 'defense'.
destructibleAmount :: (Ninja -> [Destructible]) -- ^ Getter.
                   -> Text -- ^ 'Destructible.name'.
                   -> Slot -- ^ 'Destructible.user'.
                   -> Ninja -> Int
destructibleAmount getter name user n = sum
    [amount | d@Destructible{amount} <- getter n
            , Labeled.match name user d]

-- | Sums 'Destructible.amount' of all matching 'barrier'.
barrierAmount :: Text -- ^ 'Destructible.name'.
              -> Slot -- ^ 'Destructible.user'.
              -> Ninja -> Int
barrierAmount = destructibleAmount barrier

-- | Sums 'Destructible.amount' of all matching 'defense'.
defenseAmount :: Text -- ^ 'Destructible.name'.
              -> Slot -- ^ 'Destructible.user'.
              -> Ninja -> Int
defenseAmount = destructibleAmount defense

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
