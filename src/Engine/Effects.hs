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
  , invincible
  , reduce
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

import ClassyPrelude.Yesod hiding (Status, link, share)
import           Data.List.NonEmpty (NonEmpty(..))

import           Core.Util ((∈), enumerate, intersects)
import qualified Class.Parity as Parity
import           Model.Chakra (Chakras(..))
import           Model.Class (Class(..))
import qualified Model.Game as Game
import           Model.Game (Game)
import           Model.Effect (Amount(..), Effect(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import           Model.Player (Player)
import           Model.Slot (Slot)
import qualified Model.Status as Status
import           Model.Status (Status)

-- | Adds 'Flat' amounts and multiplies by 'Percent' amounts.
total :: [(Amount, Int)] -> Amount -> Rational
total xs Flat    = toRational . sum . map snd $
                   filter ((== Flat) . fst) xs
total xs Percent = product . (1 :) . map ((/ 100) . toRational . snd) $
                   filter ((== Percent) . fst) xs

-- | 'Bleed' sum.
bleed :: [Class] -> Ninja -> Amount -> Rational
bleed classes n =
    total [(amt, x) | Bleed cla amt x <- Ninja.effects n, cla ∈ classes]

-- | 'Block' collection.
block :: Ninja -> [Slot]
block n = [Status.user st | st <- Ninja.statuses n, Block ∈ Status.effects st ]

-- | 'Bless' sum.
bless :: Ninja -> Int
bless n = sum [x | Bless x <- Ninja.effects n]

-- | 'Boost' sum from a user.
boost :: Slot -> Ninja -> Int
boost user n
  | user == Ninja.slot n                    = 1
  | not . Parity.allied user $ Ninja.slot n = 1
  | otherwise = product $ 1 : [x | Boost x <- Ninja.effects n]

-- | 'Build' sum.
build :: Ninja -> Int
build n = sum [x | Build x <- Ninja.effects n]

-- | 'Duel' collection.
duel :: Ninja -> [Slot]
duel n = [slot | Duel slot <- Ninja.effects n, slot /= Ninja.slot n]

-- | 'Exhaust' sum.
exhaust :: [Class] -> Ninja -> Chakras
exhaust classes n =
    0 { rand = length [x | Exhaust x <- Ninja.effects n, x ∈ classes] }

-- | 'Ignore' collection.
ignore :: Ninja -> [Effect]
ignore n = [ef cla | Ignore ef <- Ninja.effects n, cla <- enumerate]

-- | 'Invulnerable' collection.
immune :: Ninja -> [Class]
immune n = [x | Invulnerable x <- Ninja.effects n]

-- | 'Invincible' collection.
invincible :: Ninja -> [Class]
invincible (Ninja.effects -> efs) = [x | Invincible x <- efs]

-- | 'Reduce' sum.
reduce :: [Class] -> Ninja -> Amount -> Rational
reduce [Affliction] n =
    total [(amt, x) | Reduce Affliction amt x <- Ninja.effects n]
reduce classes n =
    total [(amt, x) | Reduce cla amt x <- Ninja.effects n
                    , cla ∈ classes
                    , cla /= Affliction]

-- | 'Share' collection.
share :: Ninja -> [Slot]
share n = [slot | Share slot <- Ninja.effects n, slot /= Ninja.slot n]

-- | 'Snare' sum.
snare :: Ninja -> Int
snare n = sum [x | Snare x <- Ninja.effects n]

-- | 'Strengthen' sum.
strengthen :: [Class] -> Ninja -> Amount -> Rational
strengthen classes n =
    total [(amt, x) | Strengthen cla amt x <- Ninja.effects n, cla ∈ classes]

-- | 'Stun' collection.
stun :: Ninja -> [Class]
stun n = [x | Stun x <- Ninja.effects n]

-- | 'Taunt' collection.
taunt :: Ninja -> [Slot]
taunt n = [slot | Taunt slot <- Ninja.effects n, slot /= Ninja.slot n]

-- | 'Threshold' max.
threshold :: Ninja -> Int
threshold n = maximumEx $ 0 :| [x | Threshold x <- Ninja.effects n]

-- | 'Throttle' sum.
throttle :: [Effect] -> Ninja -> Int
throttle efs n = sum [x | Throttle f x <- Ninja.effects n, throttled f]
  where
    throttled = (efs `intersects`) . (<$> enumerate)

-- | 'Unreduce' sum.
unreduce :: Ninja -> Int
unreduce n = sum [x | Unreduce x <- Ninja.effects n]

-- | 'Weaken' sum.
weaken :: [Class] -> Ninja -> Amount -> Rational
weaken classes n =
  total [(amt, x) | Weaken cla amt x <- Ninja.effects n, cla ∈ classes]

-- | 'Afflict' sum minus 'Heal' sum.
hp :: Player -> Ninja -> Game -> Int
hp player n game =  afflict player game n - heal player game n

-- | 'Heal' sum.
heal :: Player -> Game -> Ninja -> Int
heal player game n
  | Ninja.is Plague n = 0
  | otherwise         = sum $ heal1 player game n <$> Ninja.statuses n

-- | Calculates the total 'Heal' of a single 'Status'.
heal1 :: Player -> Game -> Ninja -> Status -> Int
heal1 player game n st
  | user /= Ninja.slot n && Ninja.is Seal n  = 0
  | summed /= 0 && Parity.allied player user =
      boost user n * summed + bless (Game.ninja user game)
  | otherwise = 0
  where
    user = Status.user st
    summed = sum [hp' | Heal hp' <- Status.effects st]

-- | 'Afflict' sum.
afflict :: Player -> Game -> Ninja -> Int
afflict player game n = sum
    [aff st | st <- Ninja.statuses n
            , not (Ninja.is ImmuneSelf n) || Status.user st /= Ninja.slot n
            , not $ [All, Affliction] `intersects` invincible n]
  where
    aff = afflict1 player game $ Ninja.slot n

-- | Calculates the total 'Afflict' of a single 'Status'.
afflict1 :: Player -> Game -> Slot -> Status -> Int
afflict1 player game t st
  | summed /= 0 && Parity.allied player user =
      truncate $ scale * (summed + ext)
  | otherwise = 0
  where
    user   = Status.user st
    nt     = Game.ninja t game
    n      = Game.ninja user game
    summed = toRational $ sum [hp' | Afflict hp' <- Status.effects st]
    ext
      | t == user                  = 0
      | not $ Ninja.alive n          = bleed [Affliction, All] nt Flat
      | Ninja.is (Stun Affliction) n = 0
      | otherwise                    = strengthen [Affliction, All] n  Flat
                                     + bleed      [Affliction, All] nt Flat
    scale
      | t == user                  = 0
      | not $ Ninja.alive n          = bleed [Affliction, All] nt Percent
      | Ninja.is (Stun Affliction) n = 0
      | otherwise                    = strengthen [Affliction, All] n  Percent
                                     * bleed      [Affliction, All] nt Percent

