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
  , redirect
  , reduce
  , reflect
  , share
  , snare
  , strengthen
  , stun, stunned
  , throttle, throttleCounters
  , taunt
  , unreduce
  , weaken
  ) where

import ClassyPrelude hiding (link)

import Data.Enum.Set (EnumSet)

import qualified Class.Parity as Parity
import           Game.Model.Chakras (Chakras(Chakras))
import qualified Game.Model.Chakras
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Amount(..), Constructor(..), Effect(..))
import qualified Game.Model.Effect as Effect
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Player (Player)
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Game.Model.Status (Status(Status))
import qualified Game.Model.Status
import           Util ((!!), (∈), intersects)

-- | Adds 'Flat' amounts and multiplies by 'Percent' amounts.
total :: Amount -> Int -> Float
total Flat x    = fromIntegral x
total Percent x = fromIntegral x / 100

-- | 'Bleed' sum.
bleed :: EnumSet Class -> Ninja -> Amount -> Float
bleed classes Ninja{effects} amount = total amount $ sum
    [x | Bleed cla amt x <- effects
       , amount == amt
       , cla `intersects` classes]

-- | 'Block' collection.
block :: Ninja -> [Slot]
block Ninja{effects} = [slot | Block slot <- effects]

-- | 'Bless' sum.
bless :: Ninja -> Int
bless Ninja{effects} = sum [x | Bless x <- effects]

-- | 'Boost' sum from a user.
boost :: Slot -> Ninja -> Int
boost user n@Ninja{effects, slot}
  | user == slot         = 1
  | Parity.allied user n = product $ 1 : [x | Boost x <- effects]
  | otherwise            = 1

-- | 'Build' sum.
build :: Ninja -> Int
build Ninja{effects} = sum [x | Build x <- effects]

-- | 'Duel' collection.
duel :: Ninja -> [Slot]
duel Ninja{effects, slot} = [x | Duel x <- effects, slot /= x]

-- | 'Exhaust' sum.
exhaust :: EnumSet Class -> Ninja -> Chakras
exhaust classes Ninja{effects} = Chakras
    { rand  = length [x | Exhaust x <- effects, x `intersects` classes]
    , blood = 0
    , gen   = 0
    , nin   = 0
    , tai   = 0
    }

-- | 'Invulnerable' collection.
invulnerable :: Ninja -> EnumSet Class
invulnerable Ninja{effects} = setFromList [x | Invulnerable x <- effects]

-- | 'Limit' minimum.
limit :: Ninja -> Maybe Int
limit Ninja{effects} = minimumMay [x | Limit x <- effects]

-- | First 'Redirect'.
redirect :: Ninja -> Maybe Slot
redirect Ninja{effects} = headMay [slot | Redirect slot <- effects]

-- | 'Reduce' sum.
reduce :: EnumSet Class -> Ninja -> Amount -> Float
reduce classes Ninja{effects} amount
    | classes == singleton Affliction = total amount $ sum
        [x | Reduce cla amt x <- effects
           , amount == amt
           , Affliction ∈ cla]
    | otherwise = total amount $ sum
        [x | Reduce cla amt x <- effects
           , amt == amount
           , deleteSet Affliction cla `intersects` classes]

-- | 'Share' collection.
share :: Ninja -> [Slot]
share Ninja{effects, slot} = [x | Share x <- effects, x /= slot]

-- | 'Snare' sum.
snare :: Ninja -> Int
snare Ninja{effects} = sum [x | Snare x <- effects]

-- | 'Strengthen' sum.
strengthen :: EnumSet Class -> Ninja -> Amount -> Float
strengthen classes Ninja{effects} amount = total amount $ sum
    [x | Strengthen cla amt x <- effects
       , amt == amount
       , cla `intersects` classes]

-- | 'Stun' collection.
stun :: Ninja -> EnumSet Class
stun Ninja{effects} = setFromList [x | Stun x <- effects]

-- | @not . null . stun@
stunned :: Ninja -> Bool
stunned n = not . null $ stun n

-- | 'Taunt' collection.
taunt :: Ninja -> [Slot]
taunt Ninja{effects, slot} = [x | Taunt x <- effects, x /= slot]

-- | 'Throttle' sum.
throttle :: [Effect] -> Ninja -> Int
throttle efs Ninja{effects} = sum [x | Throttle x f <- effects, throttled f]
  where
    throttled Stuns       = any Effect.isDisable efs
    throttled constructor = any (∈ efs) $ Effect.construct constructor

-- 'Throttle' 'Counters' sum.
throttleCounters :: Ninja -> Int
throttleCounters Ninja{effects} = sum [x | Throttle x Counters <- effects]

-- | 'Unreduce' sum.
unreduce :: Ninja -> Int
unreduce Ninja{effects} = sum [x | Unreduce x <- effects]

-- | 'Weaken' sum.
weaken :: EnumSet Class -> Ninja -> Amount -> Float
weaken classes Ninja{effects} amount = total amount $ sum
    [x | Weaken cla amt x <- effects
       , amount == amt
       , cla `intersects` classes]

-- | 'Disable' collection.
disabled :: Ninja -> [Effect]
disabled Ninja{effects} = [f | Disable con <- effects, f <- Effect.construct con]

reflect :: EnumSet Class -> Ninja -> Bool
reflect classes n@Ninja{effects} = n `is` Reflect
    || classes `intersects` setFromList [x | ReflectAll x <- effects]

-- | 'Afflict' sum minus 'Heal' sum.
hp :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
   => Player -> Ninja -> o -> Int
hp player n ninjas
  | N.alive n = afflict ninjas player n - heal ninjas player n
  | otherwise = 0

-- | 'Heal' sum.
heal :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
     => o -> Player -> Ninja -> Int
heal ninjas player n@Ninja{statuses}
  | n `is` Plague || n `is` Seal = 0
  | otherwise = sum $ heal1 ninjas player n <$> statuses

-- | Calculates the total 'Heal' of a single @Status@.
heal1 :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
      => o -> Player -> Ninja -> Status -> Int
heal1 ninjas player n Status{effects, user}
  | summed == 0 || not (Parity.allied player user) = 0
  | otherwise = boost user n * summed + bless (ninjas !! Slot.toInt user)
  where
    summed = sum [hp' | Heal hp' <- effects]

afflictClasses :: EnumSet Class
afflictClasses = setFromList [Affliction, All]

-- | 'Afflict' sum.
afflict :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
        => o -> Player -> Ninja -> Int
afflict ninjas player n@Ninja{slot, statuses} = sum
    [aff st | st@Status{user} <- statuses
            , user == slot || not (afflictClasses `intersects` invulnerable n)]
  where
    aff = afflict1 ninjas player slot

-- | Calculates the total 'Afflict' of a single @Status@.
afflict1 :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
         => o -> Player -> Slot -> Status -> Int
afflict1 ninjas player t Status{classes, effects, user}
  | summed == 0                     = 0
  | not $ Parity.allied player user = 0
  | otherwise                       = damage
  where
    nt     = ninjas !! Slot.toInt t
    n      = ninjas !! Slot.toInt user
    summed = fromIntegral $ sum [hp' | Afflict hp' <- effects]
    damage = truncate $ scale * (summed + ext)
    classes'
      | Bane ∈ classes = insertSet Bane afflictClasses
      | otherwise      = afflictClasses
    ext
      | t == user = 0
      | N.alive n = bleed classes' nt Flat + strengthen classes' n Flat
      | otherwise = bleed classes' nt Flat
    scale :: Float
    scale
      | t == user = 1
      | N.alive n = (1 + strengthen classes' n Percent)
                  * (1 + bleed classes' nt Percent)
      | otherwise = 1 + bleed classes' nt Percent
