-- | Calculated totals of 'Effect's on 'Ninja's.
module Engine.Effects
  ( bleed
  , bless
  , block
  , boost
  , build
  , duel
  , exhaust
  , hp
  , ignore
  , immune
  , reduce
  , replace
  , share
  , snare
  , strengthen
  , stun
  , threshold
  , throttle
  , taunt
  , unreduce
  , weaken
  ) where

import ClassyPrelude hiding (link)

import Data.Enum.Set.Class (EnumSet)
import Data.List.NonEmpty (NonEmpty(..))

import           Core.Util ((!!), (∈), intersects)
import qualified Class.Parity as Parity
import           Model.Chakra (Chakras(..))
import           Model.Class (Class(..))
import           Model.Duration (Duration)
import qualified Model.Effect as Effect
import           Model.Effect (Amount(..), Effect(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja, is)
import           Model.Player (Player)
import qualified Model.Slot as Slot
import           Model.Slot (Slot)
import qualified Model.Status as Status
import           Model.Status (Status)

-- | Adds 'Flat' amounts and multiplies by 'Percent' amounts.
total :: [(Amount, Int)] -> Amount -> Float
total xs Flat    = fromIntegral . sum . map snd $
                   filter ((Flat ==) . fst) xs
total xs Percent = product . (1 :) . map ((/ 100) . fromIntegral . snd) $
                   filter ((Percent ==) . fst) xs

-- | 'total' for negative effects such as damage reduction.
negativeTotal :: [(Amount, Int)] -> Amount -> Float
negativeTotal xs Flat    = total xs Flat
negativeTotal xs Percent = total (second (100 -) <$> xs) Percent

-- | 'Bleed' sum.
bleed :: EnumSet Class -> Ninja -> Amount -> Float
bleed classes n =
    total [(amt, x) | Bleed cla amt x <- Ninja.effects n, cla ∈ classes]

-- | 'Block' collection.
block :: Ninja -> [Slot]
block n = [Status.user st | st <- Ninja.statuses n, Block ∈ Status.effects st]

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
exhaust classes n =
    0 { rand = length [x | Exhaust x <- Ninja.effects n, x ∈ classes] }

-- | 'Ignore' collection.
ignore :: Ninja -> [Effect]
ignore n = [ef | Ignore con <- Ninja.effects n, ef <- Effect.construct con]

-- | 'Invulnerable' collection.
immune :: Ninja -> EnumSet Class
immune n = setFromList [x | Invulnerable x <- Ninja.effects n]

-- | 'Reduce' sum.
reduce :: EnumSet Class -> Ninja -> Amount -> Float
reduce classes n
    | classes == singletonSet Affliction =
        negativeTotal [(amt, x) | Reduce Affliction amt x <- Ninja.effects n]
    | otherwise =
        negativeTotal [(amt, x) | Reduce cla amt x <- Ninja.effects n
                                , cla ∈ classes
                                , cla /= Affliction]

-- | 'Replace' collection.
replace :: Ninja -> [(Duration, Slot, Text)]
replace n = [(dur, slot, name) | Replace dur slot name <- Ninja.effects n]

-- | 'Share' collection.
share :: Ninja -> [Slot]
share n = [slot | Share slot <- Ninja.effects n, slot /= Ninja.slot n]

-- | 'Snare' sum.
snare :: Ninja -> Int
snare n = sum [x | Snare x <- Ninja.effects n]

-- | 'Strengthen' sum.
strengthen :: EnumSet Class -> Ninja -> Amount -> Float
strengthen classes n =
    total [(amt, x) | Strengthen cla amt x <- Ninja.effects n, cla ∈ classes]

-- | 'Stun' collection.
stun :: Ninja -> EnumSet Class
stun n = setFromList [x | Stun x <- Ninja.effects n]

-- | 'Taunt' collection.
taunt :: Ninja -> [Slot]
taunt n = [slot | Taunt slot <- Ninja.effects n, slot /= Ninja.slot n]

-- | 'Threshold' max.
threshold :: Ninja -> Int
threshold n = maximumEx $ 0 :| [x | Threshold x <- Ninja.effects n]

-- | 'Throttle' sum.
throttle :: [Effect] -> Ninja -> Int
throttle efs n = sum [x | Throttle x f <- Ninja.effects n, throttled f]
  where
    throttled constructor = any (∈ efs) $ Effect.construct constructor

-- | 'Unreduce' sum.
unreduce :: Ninja -> Int
unreduce n = sum [x | Unreduce x <- Ninja.effects n]

-- | 'Weaken' sum.
weaken :: EnumSet Class -> Ninja -> Amount -> Float
weaken classes n =
  negativeTotal [(amt, x) | Weaken cla amt x <- Ninja.effects n, cla ∈ classes]

-- | 'Afflict' sum minus 'Heal' sum.
hp :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
   => Player -> Ninja -> o -> Int
hp player n ninjas = afflict ninjas player n - heal ninjas player n

-- | 'Heal' sum.
heal :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
     => o -> Player -> Ninja -> Int
heal ninjas player n
  | not $ Ninja.alive n = 0
  | n `is` Plague       = 0
  | otherwise           = sum $ heal1 ninjas player n <$> Ninja.statuses n

-- | Calculates the total 'Heal' of a single @Status@.
heal1 :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
      => o -> Player -> Ninja -> Status -> Int
heal1 ninjas player n st
  | user /= Ninja.slot n && n `is` Seal      = 0
  | summed /= 0 && Parity.allied player user =
      boost user n * summed + bless (ninjas !! Slot.toInt user)
  | otherwise = 0
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
            , not (n `is` ImmuneSelf) || Status.user st /= Ninja.slot n
            , not $ afflictClasses `intersects` immune n]
  where
    aff = afflict1 ninjas player $ Ninja.slot n

-- | Calculates the total 'Afflict' of a single @Status@.
afflict1 :: ∀ o. (IsSequence o, Ninja ~ Element o, Int ~ Index o)
         => o -> Player -> Slot -> Status -> Int
afflict1 ninjas player t st
  | summed /= 0 && Parity.allied player user = truncate $ scale * (summed + ext)
  | otherwise                                = 0
  where
    user   = Status.user st
    nt     = ninjas !! Slot.toInt t
    n      = ninjas !! Slot.toInt user
    summed = fromIntegral $ sum [hp' | Afflict hp' <- Status.effects st]
    ext
      | t == user              = 0
      | not $ Ninja.alive n    = bleed      afflictClasses nt Flat
      | n `is` Stun Affliction = 0
      | otherwise              = strengthen afflictClasses n  Flat
                                 + bleed    afflictClasses nt Flat
    scale
      | t == user              = 0
      | not $ Ninja.alive n    = bleed      afflictClasses nt Percent
      | n `is` Stun Affliction = 0
      | otherwise              = strengthen afflictClasses n  Percent
                                 * bleed    afflictClasses nt Percent

