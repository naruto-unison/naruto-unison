module Game.Model.Ninja
  ( Ninja(..), new
  , numSkills
  , alive, minHealth
  , is, isChanneling
  , has, hasBarrier, hasDefense, hasOwnDefense, hasOwn
  , numActive, numStacks, numAnyStacks, numHelpful, numHarmful
  , defenseAmount, totalDefense, totalBarrier
  , baseSkill
  ) where

import ClassyPrelude

import qualified  Data.List.NonEmpty as NonEmpty

import qualified Class.Labeled as Labeled
import qualified Class.Parity as Parity
import qualified Game.Model.Barrier as Barrier
import qualified Game.Model.Channel as Channel
import           Game.Model.Character (Character, skills)
import           Game.Model.Class (Class(..))
import qualified Game.Model.Defense as Defense
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Effect as Effect
import           Game.Model.Internal (Ninja(..))
import           Game.Model.Skill (Skill(owner))
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Status as Status
import           Util ((<$><$>), (∈), (∉), (!?))

-- | Number of 'Skill' slots. This number is the boundary on quite a few things,
-- most notably action messages from the client (in 'Game.Action.act').
numSkills :: Ninja -> Int
numSkills n = length . skills $ character n

-- | Constructs a @Ninja@ with starting values from a character and an index.
new :: Slot -> Character -> Ninja
new slot c = Ninja { slot
                   , health     = 100
                   , character  = c { skills = own <$><$> skills c }
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
    own x     = x { owner = slot }
    skillSize = length $ skills c

-- | @alive n = health n > 0@
alive :: Ninja -> Bool
alive n = health n > 0

-- | Searches 'effects'.
is :: Ninja -> Effect -> Bool
is n ef = ef ∈ effects n

-- | Searches 'channels'.
isChanneling :: Text -- ^ 'Skill.name'.
             -> Ninja -> Bool
isChanneling name n = any ((== name) . Skill.name . Channel.skill) $ channels n

-- | Searches 'statuses'.
has :: Text -- ^ 'Status.name'.
    -> Slot -- ^ 'Status.user'.
    -> Ninja -> Bool
has name user n = any (Labeled.match name user) $ statuses n

-- | Searches 'barrier'.
hasBarrier :: Text -- ^ 'Barrier.name'.
           -> Slot -- ^ 'Barrier.user'.
           -> Ninja -> Bool
hasBarrier name user n = any (Labeled.match name user) $ barrier n

-- | Searches 'defense'.
hasDefense :: Text -- ^ 'Defense.name'.
           -> Slot -- ^ 'Defense.user'.
           -> Ninja -> Bool
hasDefense name user n = any (Labeled.match name user) $ defense n

-- | Sums 'Defense.amount' of all matching 'defense'.
defenseAmount :: Text -- ^ 'Defense.name'.
              -> Slot -- ^ 'Defense.user'.
              -> Ninja -> Int
defenseAmount name user n =
    sum [Defense.amount d | d <- defense n
                          , Defense.user d == user
                          , Defense.name d == name]

-- | Sums 'Defense.amount' of all 'defense'.
totalDefense :: Ninja -> Int
totalDefense n = sum $ Defense.amount <$> defense n

-- | Sums 'Barrier.amount' of all 'barrier'.
totalBarrier :: Ninja -> Int
totalBarrier n = sum $ Barrier.amount <$> barrier n

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
numStacks name user n =
    sum $ Status.amount <$> filter (Labeled.match name user) (statuses n)

-- | Number of stacks of 'statuses' from any source.
numAnyStacks :: Text -- ^ 'Status.name'.
             -> Ninja -> Int
numAnyStacks name n =
    sum $ Status.amount <$> filter ((== name) . Status.name) (statuses n)

-- | Counts all 'Effect.helpful' effects in 'statuses' from allies.
-- Does not include self-applied or 'Hidden' 'Status.Status'es.
-- Each status counts for @(number of helpful effects) * (Status.amount)@.
numHelpful :: Ninja -> Int
numHelpful n = sum [Status.amount st | st <- statuses n
                                     , let user = Status.user st
                                     , slot n /= user
                                     , Parity.allied n user
                                     , Hidden ∉ Status.classes st
                                     , ef <- Status.effects st
                                     , Effect.helpful ef]

-- | Counts all non-'Effect.helpful' effects in 'statuses'.
-- Does not include self-applied or 'Hidden' 'Status.Status'es.
-- Each status counts for @(number of harmful effects) * (Status.amount)@.
numHarmful :: Ninja -> Int
numHarmful n = sum [Status.amount st | st <- statuses n
                                     , let user = Status.user st
                                     , slot n /= user
                                     , Hidden ∉ Status.classes st
                                     , ef <- Status.effects st
                                     , not $ Effect.helpful ef]

-- | @1@ if affected by 'Endure', otherwise @0@.
minHealth :: Ninja -> Int
minHealth n
  | n `is` Endure = 1
  | otherwise     = 0

-- | Obtains a @Skill@ from 'skills' by slot index, if it exists.
baseSkill :: Int -> Ninja -> Maybe Skill
baseSkill s n = do
    skill     <- skills (character n) !:? s
    alternate <- alternates n !? s
    skill !:? alternate
  where
    -- (!?) for NonEmpty
    xs !:? i = headMay $ NonEmpty.drop i xs
    {-# INLINE (!:?) #-}
