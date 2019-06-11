
module Engine.Effects
  ( bleed
  , bless
  , boost
  , build
  , exhaust
  , link
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
  , targets
  , taunting
  , unreduce
  , weaken

  , hp
  ) where

import ClassyPrelude.Yesod hiding (Status, link, share)
import Data.List.NonEmpty (NonEmpty(..))

import           Core.Util ((∈), (∉), enumerate, intersects)
import qualified Class.Parity as Parity
import           Model.Chakra (Chakras(..))
import           Model.Class (Class(..))
import qualified Model.Game as Game
import           Model.Game (Game)
import           Model.Internal (ignore)
import qualified Model.Effect as Effect
import           Model.Effect (Amount(..), Effect(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import           Model.Player (Player)
import           Model.Slot (Slot)
import qualified Model.Status as Status
import           Model.Status (Status)

total :: [(Amount, Int)] -> Amount -> Rational
total xs Percent = product . (1 :) . map ((/ 100) . toRational . snd) $
                   filter ((== Percent) . fst) xs
total xs Flat    = toRational . sum . map snd $
                   filter ((== Flat) . fst) xs

-- | 'Exhaust' and 'Unexhaust' are not canceled by immunity.
noImmunity :: Effect -> Bool
noImmunity Exhaust{} = True
noImmunity Unexhaust = True
noImmunity _         = False

-- | 'Bleed' sum.
bleed :: [Class] -> Ninja -> Amount -> Rational
bleed classes n =
    total [(amt, x) | Bleed cla amt x <- Ninja.effects n, cla ∈ classes]

-- | 'Bless' sum.
bless :: Ninja -> Int
bless n = sum [x | Bless x <- Ninja.effects n]

-- | 'Boost' sum from someone.
boost :: Slot -> Ninja -> Int
boost user n
  | user == Ninja.slot n = 1
  | otherwise = product $ 1 : [x | Boost x <- Ninja.effectsFrom user n]

-- | 'Build' sum.
build :: Ninja -> Int
build n = sum [x | Build x <- Ninja.effects n]

-- | 'Exhaust' sum.
exhaust :: [Class] -> Ninja -> Chakras
exhaust classes n =
    0 { rand = length [x | Exhaust x <- Ninja.effects n, x ∈ classes] }

-- | 'Invulnerable's.
immune :: Ninja -> [Class]
immune n = [x | Invulnerable x <- Ninja.effects n]

-- | 'Invincible's.
invincible :: Ninja -> [Class]
invincible (Ninja.effects -> efs) = [x | Invincible x <- efs]

-- | 'Link' sum from someone.
link :: Slot -> Ninja -> Int
link user n = sum [x | Link x <- Ninja.effectsFrom user n]

-- | 'Reduce' sum.
reduce :: [Class] -> Ninja -> Amount -> Rational
reduce [Affliction] n =
    total [(amt, x) | Reduce Affliction amt x <- Ninja.effects n]
reduce classes n =
    total [(amt, x) | Reduce cla amt x <- Ninja.effects n
                    , cla ∈ classes
                    , cla /= Affliction]

-- | 'Share's.
share :: Ninja -> [Slot]
share n = [Status.user st | st <- modStatuses n
                          , Ninja.slot n /= Status.user st
                          , Share ∈ Status.effects st]

-- | 'Snare' sum.
snare :: Ninja -> Int
snare n = sum [x | Snare x <- Ninja.effects n]

-- | 'Strengthen' sum.
strengthen :: [Class] -> Ninja -> Amount -> Rational
strengthen classes n =
    total [(amt, x) | Strengthen cla amt x <- Ninja.effects n, cla ∈ classes]

-- | 'Stun's.
stun :: Ninja -> [Class]
stun n = [x | Stun x <- Ninja.effects n]

-- | Available targets.
targets :: Effect -> Ninja -> [Slot]
targets ef n = [Status.user st | st <- modStatuses n , ef ∈ Status.effects st]

-- | Duration of most recent 'Taunting'.
taunting :: Ninja -> Maybe (Int, Status)
taunting n =
    listToMaybe [(a, st) | st <- Ninja.statuses n
                         , Taunting a <- Status.effects st]

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

-- | Used by 'modStatus' to prevent recursion
-- when checking for 'Enrage' and 'Seal'.
rawStat :: Ninja -> Status -> Status
rawStat n st
  | Ninja.fromSelf n st = st
  | Enrage ∈ efs   = st { Status.effects = enraged }
  | Seal   ∈ efs   = st { Status.effects = unhelpful }
  | otherwise      = st
  where
    efs       = concatMap Status.effects $ Ninja.statuses n
    unhelpful = filter (not . Effect.helpful) $ Status.effects st
    enraged   = [ef | ef <- Status.effects st
                    , Effect.helpful ef || Effect.sticky ef || noImmunity ef]

modStatus :: Ninja -> Status -> Status
modStatus n st = st' { Status.effects = boostedEfs }
  where
    boostedEfs    = (sourceBoost <$>) . filter keep $ Status.effects st'
    sourceBoost   = Effect.boosted $ boost (Status.source st) n
    st'           = rawStat n st
    efs           = concatMap (Status.effects . rawStat n) $ Ninja.statuses n
    keep Enrage   = True
    keep Seal     = True
    keep Reduce{} = Expose ∉ efs
    keep Invulnerable{} = Expose ∉ efs
    keep ef
      | Ninja.fromSelf n st = True
      | Enrage ∈ efs = Effect.helpful ef || Effect.sticky ef || noImmunity ef
      | Seal   ∈ efs = not $ Effect.helpful ef
      | otherwise    = ef ∉ [f cla | Ignore f <- efs, cla <- enumerate]

modStatuses :: Ninja -> [Status]
modStatuses n = Status.unfold . modStatus n <$> Ninja.statuses n

-- | 'Afflict' sum minus 'Heal' sum.
hp :: Player -> Ninja -> Game -> Int
hp player n game =  afflict player game n - heal player game n

-- | 'Heal' sum.
heal :: Player -> Game -> Ninja -> Int
heal player game n
  | Ninja.is Plague n = 0
  | otherwise         = sum $ heal1 player game n <$> modStatuses n

heal1 :: Player -> Game -> Ninja -> Status -> Int
heal1 player game n st
  | summed /= 0 && Parity.allied player source =
      boost source n * summed + bless (Game.ninja source game)
  | otherwise = 0
  where
    source = Status.source st
    summed = sum [hp' | Heal hp' <- Status.effects st]

-- | 'Afflict' sum.
afflict :: Player -> Game -> Ninja -> Int
afflict player game n = sum
    [aff st | st <- modStatuses n
            , not (Ninja.is ImmuneSelf n) || Status.source st /= Ninja.slot n
            , not $ [All, Affliction] `intersects` invincible n]
  where
    aff = afflict1 player game $ Ninja.slot n

afflict1 :: Player -> Game -> Slot -> Status -> Int
afflict1 player game t st
  | summed /= 0 && Parity.allied player source =
      truncate $ scale * (summed + ext)
  | otherwise = 0
  where
    source = Status.source st
    nt     = Game.ninja t game
    n      = Game.ninja source game
    summed = toRational $ sum [hp' | Afflict hp' <- Status.effects st]
    ext
      | t == source                  = 0
      | not $ Ninja.alive n          = bleed [Affliction, All] nt Flat
      | Ninja.is (Stun Affliction) n = 0
      | otherwise                    = toRational (link source nt)
                                     + strengthen [Affliction, All] n  Flat
                                     + bleed      [Affliction, All] nt Flat
    scale
      | t == source                  = 0
      | not $ Ninja.alive n          = bleed [Affliction, All] nt Percent
      | Ninja.is (Stun Affliction) n = 0
      | otherwise                    = strengthen [Affliction, All] n  Percent
                                     * bleed      [Affliction, All] nt Percent

