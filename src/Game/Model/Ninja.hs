module Game.Model.Ninja
  ( Ninja(..), new
  , alive
  , is, isChanneling
  , has, has', hasBarrier, hasDefense, hasTrap
  , hasFromAny, hasFromAny'
  , total, amount, amount', amountFromAny', amountFromAny
  , duration, duration'
  , numHelpful, numHarmful
  , lastChakraSpent
  ) where

import ClassyPrelude

import qualified Class.Parity as Parity
import           Class.Stackable (Stackable, getAmount)
import           Class.TurnBased (TurnBased)
import qualified Class.TurnBased as TurnBased
import           Game.Model.Chakras (Chakras)
import           Game.Model.Class (Class(..))
import           Game.Model.Duration (Duration(..))
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Effect as Effect
import           Game.Model.ID (HasID, ID)
import qualified Game.Model.ID as ID
import           Game.Model.Internal (Ninja(..), Channel(Channel), Character, Skill(Skill), Status(Status))
import qualified Game.Model.Internal
import qualified Game.Model.Internal.Character as Character
import qualified Game.Model.Internal.Skill as Skill
import           Game.Model.Slot (Slot)
import           Util ((∈), (∉))

-- | Constructs a @Ninja@ with starting values from a character and an index.
new :: Slot -> Character -> Ninja
new slot c = Ninja
    { slot
    , health     = 100
    , character  = c { Character.skills = owned }
    , defense    = mempty
    , barrier    = mempty
    , statuses   = mempty
    , charges    = mempty
    , cooldowns  = mempty
    , skills     = toNullable $ head <$> owned
    , copies     = replicate (length owned) Nothing
    , channels   = mempty
    , traps      = mempty
    , lastSkill  = Nothing
    , triggers   = mempty
    , negatives  = mempty
    , effects    = mempty
    , face       = Nothing
    }
  where
    own x = x { Skill.owner = slot }
    owned = (own <$>) <$> c.skills
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
has' getter itemID n = any ((== itemID) . ID.from) $ getter n

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

hasFromAny' :: ∀ a. HasID a
            => (Ninja -> [a])
            -> Text
            -> Ninja
            -> Bool
hasFromAny' getter name n = any ((== name) . ID.name . ID.from) $ getter n

hasFromAny :: Text
           -> Ninja
           -> Bool
hasFromAny = hasFromAny' statuses

-- | Chakra spent on 'lastSkill'.
lastChakraSpent :: Ninja -> Chakras
lastChakraSpent Ninja{lastSkill = Just Skill{cost}} = cost
lastChakraSpent _                                   = mempty

-- | Number of stacks of matching 'statuses'.
amount :: ID -- ^ 'Status.name'.
          -> Ninja -> Int
amount = amount' statuses

amount' :: ∀ a. (HasID a, Stackable a) => (Ninja -> [a]) -> ID -> Ninja -> Int
amount' getter itemID n = sum [ getAmount item | item <- getter n,
                                                 ID.from item == itemID ]

-- | Number of stacks of matching 'statuses' from any source.
amountFromAny :: Text -- ^ 'Status.name'.
              -> Ninja -> Int
amountFromAny = amountFromAny' statuses

amountFromAny' :: ∀ a. (HasID a, Stackable a)
               => (Ninja -> [a]) -> Text -> Ninja -> Int
amountFromAny' getter name n = sum
    [ getAmount item | item <- getter n,
                       (ID.from item).name == name ]

duration :: ID -> Ninja -> Maybe Duration
duration = duration' statuses

duration' :: ∀ a. (HasID a, TurnBased a)
          => (Ninja -> [a]) -> ID -> Ninja -> Maybe Duration
duration' getter itemID n = TurnBased.getDur
                            <$> find ((== itemID) . ID.from) (getter n)

total :: ∀ a. Stackable a => (Ninja -> [a]) -> Ninja -> Int
total getter n = sum $ getAmount <$> getter n

-- | Counts all 'Effect.helpful' effects in 'statuses' from allies.
-- Does not include self-applied or 'Hidden' 'Status.Status'es.
-- Each status counts for @(number of helpful effects) * (Status.amount)@.
numHelpful :: Ninja -> Int
numHelpful Ninja{slot, statuses} = sum
    [ stAmount | Status{amount = stAmount, classes, effects, user} <- statuses,
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
    [ stAmount | Status{amount = stAmount, classes, effects, user} <- statuses,
                 slot /= user,
                 Hidden ∉ classes,
                 ef <- effects,
                 not $ Effect.helpful ef ]
