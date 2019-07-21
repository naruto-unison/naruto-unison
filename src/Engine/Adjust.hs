-- | 'Ninja' processing.
module Engine.Adjust
  ( skills, skill, skill'
  , apply
  , effects
  ) where

import ClassyPrelude hiding (head)

import           Data.List.NonEmpty ((!!), head)
import           Data.Vector ((!?))

import           Core.Util ((∈), (∉))
import qualified Class.Parity as Parity
import qualified Model.Character as Character
import qualified Model.Copy as Copy
import qualified Model.Effect as Effect
import           Model.Effect (Amount(..), Effect(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Requirement as Requirement
import           Model.Skill (Skill)
import qualified Model.Status as Status
import qualified Model.Variant as Variant
import qualified Engine.Effects as Effects
import qualified Engine.SkillTransform as SkillTransform

-- | Adjusts the 'Skill' slot of a 'Ninja' due to 'Ninja.variants', 'Effect's
-- that modify skills, and the 'Skill.changes' of the 'Skill'.
skill' :: Ninja -> Int -> Int -> Skill
skill' n s v = SkillTransform.change n $ cSkills !! s !! v
  where
    cSkills = Character.skills $ Ninja.character n

-- | Applies 'skill'' to a 'Skill' and further modifies it due to 'Ninja.copies'
-- and 'Skill.require'ments.
skill :: Either Int Skill -> Ninja -> Skill
skill (Right sk) n = Requirement.usable n Nothing sk
skill (Left s)   n = Requirement.usable n (Just s) .
                     maybe (skill' n s v) Copy.skill . join . (!? s) $
                     Ninja.copies n
    where
      v = maybe 0 (Variant.variant . head) . (!? s) $ Ninja.variants n

-- | All four skill slots of a 'Ninja' modified by 'skill'.
skills :: Ninja -> [Skill]
skills n = flip skill n . Left <$> [0..3]

-- | Modifies 'Effect's when they are first added to a 'Ninja' due to 'Effect's
-- already added.
apply :: ∀ f. (Functor f, IsSequence (f Effect), Effect ~ Element (f Effect))
      => Ninja -> f Effect -> f Effect
apply n = map adjustEffect . filter keepEffects
  where
    adjustEffect (Reduce cla Flat x) = Reduce cla Flat $ x - Effects.unreduce n
    adjustEffect f                   = f
    keepEffects Invulnerable{}       = not $ Ninja.is Expose n
    keepEffects _                    = True

-- | Fills 'Ninja.effects' with the effects of 'Ninja.statuses', modified by
-- 'Ignore', 'Seal', 'Boost', and so on.
effects :: Ninja -> Ninja
effects n = n { Ninja.effects = baseStatuses >>= processEffects }
  where
    nSlot         = Ninja.slot n
    baseStatuses  = Ninja.statuses n
    baseEffects   = concatMap Status.effects baseStatuses
    enraged       = Enrage ∈ baseEffects
    sealed        = not enraged && Seal ∈ baseEffects
    ignores
      | sealed    = [ef | status <- baseStatuses
                        , Status.user status == nSlot
                        , Ignore con <- Status.effects status
                        , ef <- Effect.construct con
                        ]
      | otherwise = [ef | Ignore con <- baseEffects, ef <- Effect.construct con]
    boostAmount
      | sealed    = 1
      | otherwise = product $ 1 : [x | Boost x <- baseEffects]
    filtered filt = filter (\ef -> filt ef && ef ∉ ignores) . Status.effects
    processEffects st
      | Status.user st == Ninja.slot n = Status.effects st
      | enraged   = boost <$> filtered Effect.bypassEnrage st
      | sealed    = boost <$> filtered (not . Effect.helpful) st
      | otherwise = boost <$> filtered (const True) st
      where
        boost
          | Parity.allied nSlot $ Status.user st = Effect.boosted boostAmount
          | otherwise                            = id
