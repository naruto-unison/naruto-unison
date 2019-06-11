module Engine.Adjust
  ( effects
  , skill, skill', skills
  ) where

import ClassyPrelude.Yesod hiding (head)
import           Data.List.NonEmpty ((!!), head)
import qualified Data.Sequence as Seq

import qualified Model.Character as Character
import qualified Model.Copy as Copy
import           Model.Effect (Amount(..), Effect(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Requirement as Requirement
import           Model.Skill (Skill)
import qualified Model.Variant as Variant
import qualified Engine.Effects as Effects
import qualified Engine.SkillTransform as SkillTransform

-- | Modifies 'Effect's when they are first added to a 'Ninja'.
effects :: ∀ f. (Functor f, IsSequence (f Effect), Element (f Effect) ~ Effect)
        => Ninja -> f Effect -> f Effect
effects n = map adjustEffect . filter keepEffects
  where
    adjustEffect (Reduce cla Flat x) = Reduce cla Flat $ x - Effects.unreduce n
    adjustEffect f                   = f
    keepEffects Invulnerable{}       = not $ Ninja.is Expose n
    keepEffects _                    = True

skill :: Either Int Skill -> Ninja -> Skill
skill (Right sk) n = Requirement.usable n Nothing sk
skill (Left s)   n = Requirement.usable n (Just s) . maybe f Copy.skill .
                     join . Seq.lookup s $ Ninja.copies n
    where
      f = skill' n s (if s > 3 then 0 else variant s n)

-- | Simplified 'skill' that ignores 'Copy' 'Skill's
-- and does not check whether the skill is 'usable'.
skill' :: Ninja -> Int -> Int -> Skill
skill' n s v = SkillTransform.restrict n . SkillTransform.change n $
               cSkills !! s !! v
  where
    cSkills = Character.skills $ Ninja.character n

skills :: Ninja -> [Skill]
skills n = flip skill n . Left <$> [0..3]

variant :: Int -> Ninja -> Int
variant s = maybe 0 (Variant.variant . head) . Seq.lookup s . Ninja.variants
