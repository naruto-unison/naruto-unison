module Game.Model.Ninja
  ( Ninja(..), new
  , alive
  , is, isChanneling
  , has, has', hasBarrier, hasDefense, hasTrap
  , numStacks, numHelpful, numHarmful
  , barrierAmount, defenseAmount, totalDefense, totalBarrier
  , lastChakraSpent
  ) where

import ClassyPrelude

import qualified Class.Parity as Parity
import           Game.Model.Chakras (Chakras)
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Effect as Effect
import           Game.Model.ID (HasID, ID)
import qualified Game.Model.ID as ID
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
isChanneling :: ID -- ^ 'Skill.name'.
             -> Ninja -> Bool
isChanneling (ID.fromOwner -> skillID) n = any matches $ channels n
  where
    matches chan@Channel{new = False} = ID.from chan == skillID
    matches _ = False

has' :: ∀ a. HasID a
     => (Ninja -> [a])
     -> ID -- ^ 'Status.name'.
     -> Ninja -> Bool
has' getter effectID n = any ((== effectID) . ID.from) $ getter n

-- | Searches 'statuses'.
has :: ID -- ^ 'Status.name'.
    -> Ninja -> Bool
has = has' statuses

-- | Searches 'barrier'.
hasBarrier :: ID -- ^ 'Destructible.name'.
           -> Ninja -> Bool
hasBarrier = has' barrier

-- | Searches 'defense'.
hasDefense :: ID -- ^ 'Destructible.name'.
           -> Ninja -> Bool
hasDefense = has' defense

-- | Searches 'traps'.
hasTrap :: ID -- ^ 'Trap.name'.
           -> Ninja -> Bool
hasTrap = has' traps

-- | Sums 'Destructible.amount' of all matching 'barrier' or 'defense'.
destructibleAmount :: (Ninja -> [Destructible]) -- ^ Getter.
                   -> ID -- ^ 'Destructible.name'.
                   -> Ninja -> Int
destructibleAmount getter destrID n = sum
    [ amount | d@Destructible{amount} <- getter n,
               ID.from d == destrID ]

-- | Sums 'Destructible.amount' of all matching 'barrier'.
barrierAmount :: ID -- ^ 'Destructible.name'.
              -> Ninja -> Int
barrierAmount = destructibleAmount barrier

-- | Sums 'Destructible.amount' of all matching 'defense'.
defenseAmount :: ID -- ^ 'Destructible.name'.
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
numStacks :: ID -- ^ 'Status.name'.
          -> Ninja -> Int
numStacks statusID Ninja{statuses} = sum
    [ amount | st@Status{amount} <- statuses,
               ID.from st == statusID ]

-- | Counts all 'Effect.helpful' effects in 'statuses' from allies.
-- Does not include self-applied or 'Hidden' 'Status.Status'es.
-- Each status counts for @(number of helpful effects) * (Status.amount)@.
numHelpful :: Ninja -> Int
numHelpful n = sum
    [ amount | Status{amount, classes, effects, user} <- statuses n,
               slot n /= user,
               Parity.allied n user,
               Hidden ∉ classes,
               ef <- effects,
               Effect.helpful ef ]

-- | Counts all non-'Effect.helpful' effects in 'statuses'.
-- Does not include self-applied or 'Hidden' 'Status.Status'es.
-- Each status counts for @(number of harmful effects) * (Status.amount)@.
numHarmful :: Ninja -> Int
numHarmful n = sum
    [ amount | Status{amount, classes, effects, user} <- statuses n,
               slot n /= user,
               Hidden ∉ classes,
               ef <- effects,
               not $ Effect.helpful ef ]
