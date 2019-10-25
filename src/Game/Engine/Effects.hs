-- | Calculated totals of 'Effect's on 'Ninja's.
module Game.Engine.Effects
  ( bleed
  , bless
  , block
  , boost
  , build
  , disabled
  , duel
  , exhaust
  , hp
  , invulnerable
  , limit
  , reduce
  , reflect
  , share
  , snare
  , strengthen
  , stun, stunned
  , threshold
  , throttle, throttleCounters
  , taunt
  , unreduce
  , weaken
  ) where

import ClassyPrelude hiding (link)

import Data.Enum.Set.Class (EnumSet)

import qualified Class.Parity as Parity
import           Game.Model.Chakra (Chakras(..))
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Amount(..), Constructor(..), Effect(..))
import qualified Game.Model.Effect as Effect
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Player (Player)
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Game.Model.Status (Status)
import qualified Game.Model.Status as Status
import           Util ((!!), (∈), intersects)

totalBase :: [(Amount, Int)] -> Amount -> Float
totalBase xs Flat    = fromIntegral . sum . map snd $
                       filter ((== Flat) . fst) xs
totalBase xs Percent = product . (1 :) . map ((/ 100) . fromIntegral . snd) $
                       filter ((== Percent) . fst) xs

-- | Adds 'Flat' amounts and multiplies by 'Percent' amounts.
posTotal :: [(Amount, Int)] -> Amount -> Float
posTotal xs Flat    = totalBase xs Flat
posTotal xs Percent = totalBase (second (+ 100) <$> xs) Percent

-- | 'posTotal' for negative effects such as damage reduction.
negTotal :: [(Amount, Int)] -> Amount -> Float
negTotal xs Flat    = totalBase xs Flat
negTotal xs Percent = totalBase (second (100 -) <$> xs) Percent

-- | 'Bleed' sum.
bleed :: EnumSet Class -> Ninja -> Amount -> Float
bleed classes n = posTotal [(amt, x) | Bleed cla amt x <- Ninja.effects n
                                     , cla `intersects` classes]

-- | 'Block' collection.
block :: Ninja -> [Slot]
block n = [slot | Block slot <- Ninja.effects n]

-- | 'Bless' sum.
bless :: Ninja -> Int
bless n = sum [x | Bless x <- Ninja.effects n]

-- | 'Boost' sum from a user.
boost :: Slot -> Ninja -> Int
boost user n
  | user == Ninja.slot n = 1
  | Parity.allied user n = product $ 1 : [x | Boost x <- Ninja.effects n]
  | otherwise            = 1

-- | 'Build' sum.
build :: Ninja -> Int
build n = sum [x | Build x <- Ninja.effects n]

-- | 'Duel' collection.
duel :: Ninja -> [Slot]
duel n = [slot | Duel slot <- Ninja.effects n, slot /= Ninja.slot n]

-- | 'Exhaust' sum.
exhaust :: EnumSet Class -> Ninja -> Chakras
exhaust classes n = 0
    { rand = length [x | Exhaust x <- Ninja.effects n, x `intersects` classes] }

-- | 'Invulnerable' collection.
invulnerable :: Ninja -> EnumSet Class
invulnerable n = setFromList [x | Invulnerable x <- Ninja.effects n]

-- | 'Limit' minimum.
limit :: Ninja -> Maybe Int
limit n = minimumMay [x | Limit x <- Ninja.effects n]

-- | 'Reduce' sum.
reduce :: EnumSet Class -> Ninja -> Amount -> Float
reduce classes n
    | classes == singletonSet Affliction =
        negTotal [(amt, x) | Reduce cla amt x <- Ninja.effects n
                           , Affliction ∈ cla
                           ]
    | otherwise =
        negTotal [(amt, x) | Reduce cla amt x <- Ninja.effects n
                           , Affliction `deleteSet` cla `intersects` classes]

-- | 'Share' collection.
share :: Ninja -> [Slot]
share n = [slot | Share slot <- Ninja.effects n, slot /= Ninja.slot n]

-- | 'Snare' sum.
snare :: Ninja -> Int
snare n = sum [x | Snare x <- Ninja.effects n]

-- | 'Strengthen' sum.
strengthen :: EnumSet Class -> Ninja -> Amount -> Float
strengthen classes n =
    posTotal [(amt, x) | Strengthen cla amt x <- Ninja.effects n
                       , cla `intersects` classes
                       ]

-- | 'Stun' collection.
stun :: Ninja -> EnumSet Class
stun n = setFromList [x | Stun x <- Ninja.effects n]

-- | @not . null . stun@
stunned :: Ninja -> Bool
stunned n = not . null $ stun n

-- | 'Taunt' collection.
taunt :: Ninja -> [Slot]
taunt n = [slot | Taunt slot <- Ninja.effects n, slot /= Ninja.slot n]

-- | 'Threshold' max.
threshold :: Ninja -> Int
threshold n = maximum $ 0 :| [x | Threshold x <- Ninja.effects n]

-- | 'Throttle' sum.
throttle :: [Effect] -> Ninja -> Int
throttle efs n = sum [x | Throttle x f <- Ninja.effects n, throttled f]
  where
    throttled Stuns       = any Effect.isDisable efs
    throttled constructor = any (∈ efs) $ Effect.construct constructor

-- 'Throttle' 'Counters' sum.
throttleCounters :: Ninja -> Int
throttleCounters n = sum [x | Throttle x Counters <- Ninja.effects n]

-- | 'Unreduce' sum.
unreduce :: Ninja -> Int
unreduce n = sum [x | Unreduce x <- Ninja.effects n]

-- | 'Weaken' sum.
weaken :: EnumSet Class -> Ninja -> Amount -> Float
weaken classes n = negTotal [(amt, x) | Weaken cla amt x <- Ninja.effects n
                                      , cla `intersects` classes]

-- | 'Disable' collection.
disabled :: Ninja -> [Effect]
disabled n = [f | Disable con <- Ninja.effects n, f <- Effect.construct con]

reflect :: EnumSet Class -> Ninja -> Bool
reflect classes n =
    n `is` Reflect
    || classes `intersects` setFromList [x | ReflectAll x <- Ninja.effects n]

-- | 'Afflict' sum minus 'Heal' sum.
hp :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
   => Player -> Ninja -> o -> Int
hp player n ninjas = afflict ninjas player n - heal ninjas player n

-- | 'Heal' sum.
heal :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
     => o -> Player -> Ninja -> Int
heal ninjas player n
  | not $ Ninja.alive n || n `is` Plague || n `is` Seal = 0
  | otherwise = sum $ heal1 ninjas player n <$> Ninja.statuses n

-- | Calculates the total 'Heal' of a single @Status@.
heal1 :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
      => o -> Player -> Ninja -> Status -> Int
heal1 ninjas player n st
  | summed == 0 || not (Parity.allied player user) = 0
  | otherwise = boost user n * summed + bless (ninjas !! Slot.toInt user)
  where
    user = Status.user st
    summed = sum [hp' | Heal hp' <- Status.effects st]

afflictClasses :: EnumSet Class
afflictClasses = setFromList [Affliction, All]

-- | 'Afflict' sum.
afflict :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
        => o -> Player -> Ninja -> Int
afflict ninjas player n = sum
    [aff st | st <- Ninja.statuses n
            , not $ afflictClasses `intersects` invulnerable n]
  where
    aff = afflict1 ninjas player (threshold n) $ Ninja.slot n

-- | Calculates the total 'Afflict' of a single @Status@.
afflict1 :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
         => o -> Player -> Int -> Slot -> Status -> Int
afflict1 ninjas player nThreshold t st
  | summed == 0                     = 0
  | not $ Parity.allied player user = 0
  | damage < nThreshold             = 0
  | otherwise                       = damage
  where
    user   = Status.user st
    nt     = ninjas !! Slot.toInt t
    n      = ninjas !! Slot.toInt user
    summed = fromIntegral $ sum [hp' | Afflict hp' <- Status.effects st]
    damage = truncate $ scale * (summed + ext)
    classes
      | Bane ∈ Status.classes st = Bane `insertSet` afflictClasses
      | otherwise                = afflictClasses
    ext
      | t == user              = 0
      | not $ Ninja.alive n    = bleed      classes nt Flat
      | otherwise              = strengthen classes n  Flat
                                 + bleed    classes nt Flat
    scale
      | t == user              = 1
      | not $ Ninja.alive n    = bleed      classes nt Percent
      | otherwise              = strengthen classes n  Percent
                                 * bleed    classes nt Percent

