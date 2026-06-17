module Game.Model.Ninja
  ( Ninja(..), new
  , alive
  , is, isChanneling
  , has, has', hasBarrier, hasDefense, hasTrap
  , numStacks, numAnyStacks, numHelpful, numHarmful
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
import           Game.Model.Internal (Destructible(..), Ninja(..), Channel(Channel), Character, Skill(Skill), Status(Status))
import qualified Game.Model.Internal
import qualified Game.Model.Internal.Character as Character
import qualified Game.Model.Internal.Destructible as Destructible
import qualified Game.Model.Internal.Skill as Skill
import           Game.Model.Slot (Slot)
import           Util ((∈), (∉))

-- | Constructs a @Ninja@ with starting values from a character and an index.
new :: Slot -> Character -> Ninja
new slot c = Ninja
    { slot
    , health     = 100
    , character  = c { Character.skills = (own <$>) <$> c.skills }
    , defense    = mempty
    , barrier    = mempty
    , statuses   = mempty
    , charges    = mempty
    , cooldowns  = mempty
    , skills     = toNullable $ head <$> c.skills
    , copies     = replicate (length c.skills) Nothing
    , channels   = mempty
    , traps      = mempty
    , lastSkill  = Nothing
    , triggers   = mempty
    , effects    = mempty
    , face       = Nothing
    , acted      = False
    }
  where
    own x = x { Skill.owner = slot }

-- | @alive n = health n > 0@
alive :: Ninja -> Bool
alive Ninja{health} = health > 0

-- | Searches 'effects'.
is :: Ninja -> Effect -> Bool
is Ninja{effects} ef = ef ∈ effects

-- | Searches 'channels'.
isChanneling :: ID -- ^ 'Skill.name'.
             -> Ninja -> Bool
isChanneling (ID.fromOwner -> skillID) n = any matches n.channels
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
destructibleAmount getter destrID n = sum [ d.amount | d <- getter n,
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
numStacks statusID Ninja{statuses} = sum [ st.amount | st <- statuses,
                                                       ID.from st == statusID ]

-- | Number of stacks of matching 'statuses' from any source.
numAnyStacks :: Text -- ^ 'Status.name'.
             -> Ninja -> Int
numAnyStacks name Ninja{statuses} = sum [ st.amount | st <- statuses,
                                                      st.name == name ]

-- | Counts all 'Effect.helpful' effects in 'statuses' from allies.
-- Does not include self-applied or 'Hidden' 'Status.Status'es.
-- Each status counts for @(number of helpful effects) * (Status.amount)@.
numHelpful :: Ninja -> Int
numHelpful Ninja{slot, statuses} = sum
    [ amount | Status{amount, classes, effects, user} <- statuses,
               slot /= user,
               Parity.allied slot user,
               Hidden ∉ classes,
               ef <- effects,
               Effect.helpful ef ]

-- | Counts all non-'Effect.helpful' effects in 'statuses'.
-- Does not include self-applied or 'Hidden' 'Status.Status'es.
-- Each status counts for @(number of harmful effects) * (Status.amount)@.
numHarmful :: Ninja -> Int
numHarmful Ninja{slot, statuses} = sum
    [ amount | Status{amount, classes, effects, user} <- statuses,
               slot /= user,
               Hidden ∉ classes,
               ef <- effects,
               not $ Effect.helpful ef ]
