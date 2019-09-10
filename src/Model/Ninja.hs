module Model.Ninja
  ( Ninja(..), new
  , playing
  , alive, minHealth
  , is, isChanneling
  , has, hasOwn, hasDefense, hasTrap
  , numActive, numStacks, numHarmfulStacks, numHelpful
  , defenseAmount
  ) where

import ClassyPrelude --hiding (drop, group, head, init, last, take)

import           Data.List (nubBy)
import           Data.List.NonEmpty (NonEmpty(..))

import           Core.Util ((∈), (∉))
import qualified Class.Parity as Parity
import           Class.Parity (Parity)
import qualified Class.Labeled as Labeled
import           Model.Internal (Ninja(..))
import qualified Model.Channel as Channel
import qualified Model.Character as Character
import           Model.Character (Character)
import           Model.Class (Class(..))
import qualified Model.Defense as Defense
import qualified Model.Effect as Effect
import           Model.Effect (Effect(..))
import qualified Model.Skill as Skill
import           Model.Slot (Slot)
import qualified Model.Status as Status
import qualified Model.Variant as Variant

-- | Constructs a @Ninja@ with starting values from a character and an index.
new :: Slot -> Character -> Ninja
new slot c = Ninja { slot      = slot
                   , health    = 100
                   , character = c
                   , defense   = []
                   , barrier   = []
                   , statuses  = []
                   , charges   = replicate skillSize 0
                   , cooldowns = replicate skillSize mempty
                   , variants  = replicate skillSize $ Variant.none :| []
                   , copies    = replicate skillSize Nothing
                   , channels  = []
                   , newChans  = []
                   , traps     = mempty
                   , face      = []
                   , lastSkill = Nothing
                   , triggers  = mempty
                   , effects   = mempty
                   , acted     = False
                   }
  where
    skillSize = length $ Character.skills c

alive :: Ninja -> Bool
alive n = health n > 0

-- | Whether a @Ninja@ belongs to the currently playing 'Model.Player.Player'.
playing :: ∀ a. Parity a => a -> Ninja -> Bool
playing p n = alive n && Parity.allied p n

is :: Ninja -> Effect -> Bool
is n ef = ef ∈ effects n

isChanneling :: Text -- ^ 'Skill.name'.
             -> Ninja -> Bool
isChanneling name n = any ((name ==) . Skill.name . Channel.skill) $ channels n

has :: Text -- ^ 'Status.name'.
    -> Slot -- ^ 'Status.user'.
    -> Ninja -> Bool
has name user n = any (Labeled.match name user) $ statuses n

hasDefense :: Text -- ^ 'Defense.name'.
           -> Slot -- ^ 'Defense.user'.
           -> Ninja -> Bool
hasDefense name user n = any (Labeled.match name user) $ defense n

defenseAmount :: Text -- ^ 'Defense.name'.
              -> Slot -- ^ 'Defense.user'.
              -> Ninja -> Int
defenseAmount name user n =
    sum [ Defense.amount d | d <- defense n
                           , Defense.user d == user
                           , Defense.name d == name
                           ]

hasTrap :: Text -- ^ 'Trap.name'.
        -> Slot -- ^ 'Trap.user'.
        -> Ninja -- ^ 'traps' owner.
        -> Bool
hasTrap name user n = any (Labeled.match name user) $ traps n

hasOwn :: Text -> Ninja -> Bool
hasOwn name n = has name (slot n) n
                || isChanneling name n
                || hasDefense name (slot n) n

-- | Number of stacks of matching self-applied 'statuses'.
numActive :: Text -- ^ 'Status.name'.
          -> Ninja -> Int
numActive name n
  | stacks > 0         = stacks
  | hasOwn name n      = 1
  | otherwise          = 0
  where
    stacks = numStacks name (slot n) n

-- | Number of stacks of matching 'statuses'.
numStacks :: Text -- ^ 'Status.name'.
          -> Slot -- ^ 'Status.user'.
          -> Ninja -> Int
numStacks name user n = sum . (Status.amount <$>) .
                        filter (Labeled.match name user) $
                        statuses n

-- | Number of stacks of 'statuses' from any non-allied source.
numHarmfulStacks :: Text -- ^ 'Status.name'.
                 -> Ninja -> Int
numHarmfulStacks name n = sum . (Status.amount <$>) . filter harm $ statuses n
  where
    harm st = Status.name st == name && not (Parity.allied n $ Status.user st)

-- | Counts all 'Effect.helpful' 'statuses' from allies.
-- Does not include self-applied 'Status'es.
numHelpful :: Ninja -> Int
numHelpful n = length stats + length defs
  where
    stats = nubBy Labeled.eq [st | st <- statuses n
                                 , any Effect.helpful $ Status.effects st
                                 , slot n /= Status.user st
                                 , Parity.allied n $ Status.user st
                                 , Hidden ∉ Status.classes st]
    defs  = nubBy Labeled.eq [de | de <- defense n
                                 , slot n /= Defense.user de
                                 , Parity.allied n $ Defense.user de]

-- | @1@ if affected by 'Endure', otherwise @0@.
minHealth :: Ninja -> Int
minHealth n
  | n `is` Endure = 1
  | otherwise     = 0
