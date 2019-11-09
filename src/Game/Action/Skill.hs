-- | Actions that characters can use to affect @Skill@s.
module Game.Action.Skill
  ( -- * Cooldowns and charges
    alterCd
  , reset, resetAll, recharge, rechargeAll
  -- * Changing face icons
  -- * Copying
  , copyAll, copyLast, teach, teachOne
  -- * Alternates
  , alternate, nextAlternate

  -- * Other
  , factory, replaceWith
  ) where

import ClassyPrelude

import           Data.Enum.Set (EnumSet)
import qualified Data.Sequence as Seq

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import           Game.Action.Status (applyWith')
import           Game.Engine (unSoulbound)
import qualified Game.Engine.Cooldown as Cooldown
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import           Game.Model.Copy (Copy(Copy))
import qualified Game.Model.Copy as Copy
import           Game.Model.Duration (Duration(..), Turns, sync)
import           Game.Model.Effect (Effect(..))
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Skill as Skill
import           Util ((!?))

-- | Changes the 'Skill.cooldown' of a @Skill@ by 'Skill.name'.
-- Uses 'Cooldown.alter' internally.
alterCd :: ∀ m. MonadPlay m => Text -> Int -> m ()
alterCd name cd =
    P.unsilenced . P.toTarget . Cooldown.alter name cd . Skill.owner =<< P.skill

-- | Resets 'Ninja.cooldowns' with a matching 'Skill.name' of a @Ninja@.
-- Uses 'Cooldown.reset' internally.
reset :: ∀ m. MonadPlay m => Text -> m ()
reset name =
    P.unsilenced . P.toTarget . Cooldown.reset name . Skill.owner =<< P.skill

-- | Resets all 'Ninja.cooldowns' of a @Ninja@.
-- Uses 'Cooldown.resetAll' internally.
resetAll :: ∀ m. MonadPlay m => m ()
resetAll = P.unsilenced $ P.toTarget Cooldown.resetAll

-- | Resets an element in 'Ninja.charges' of a @Ninja@.
-- Uses 'Ninjas.recharge' internally.
recharge :: ∀ m. MonadPlay m => Text -> m ()
recharge name =
    P.unsilenced . P.toTarget . Ninjas.recharge name . Skill.owner =<< P.skill

-- | Resets all 'Ninja.charges' of a @Ninja@.
-- Uses 'Ninjas.rechargeAll' internally.
rechargeAll :: ∀ m. MonadPlay m => m ()
rechargeAll = P.unsilenced $ P.toTarget Ninjas.rechargeAll

alternateClasses :: EnumSet Class
alternateClasses = setFromList [Hidden, Nonstacking, Unremovable]

-- | Adjusts all 'Ninja.alternates' at once.
alternate :: ∀ m. MonadPlay m
          => [Int] -- ^ Index offsets.
          -> Int   -- ^ Counter added to all 'Ninja.alternates' slots.
          -> m () -- ^ Recalculates every alternate of a target @Ninja@.
alternate loadout i =
    applyWith' alternateClasses "loadout" 0 . catMaybes . zipWith load loadout .
    toList . Character.skills . Ninja.character =<< P.nTarget
  where
    load alt (x:|xs) =
        Alternate (Skill.name x) . Skill.name <$> xs !? (alt + i - 1)

-- | Cycles a skill through its list of alternates.
-- | Uses 'Ninjas.nextAlternate' internally.
nextAlternate :: ∀ m. MonadPlay m => Text -> m ()
nextAlternate name = mapM_ alt . Ninjas.nextAlternate name =<< P.nTarget
  where
    alt x = applyWith' alternateClasses "nextAlternate" 1 [Alternate name x]

-- | Copies all @Skill@s from the target into the user's 'Ninja.copies'.
-- Uses 'Ninjas.copyAll' internally.
copyAll :: ∀ m. MonadPlay m => Turns -> m ()
copyAll (Duration -> dur) = P.uncopied do
    nTarget <- P.nTarget
    user    <- P.user
    P.modify user $ Ninjas.copyAll dur nTarget

-- | Copies the 'Ninja.lastSkill' of the target into a specific skill slot
-- of the user's 'Ninja.copies'. Uses 'Execute.copy' internally.
copyLast :: ∀ m. MonadPlay m => Turns -> m ()
copyLast (Duration -> dur) = P.uncopied do
    name   <- Skill.name <$> P.skill
    user   <- P.user
    mapM_ (P.modify user . Ninjas.copy dur name) . Ninja.lastSkill =<< P.nTarget

-- | Copies a @Skill@ from the user into all of the target's 'Ninja.copies'
-- skill slots.
teach :: ∀ m. MonadPlay m
      => Turns -- ^ 'Copy.dur'.
      -> Int -- ^ User's skill slot of the @Skill@ to copy.
      -> m ()
teach = teacher $ map . const

-- | Copies a @Skill@ from the user into a specific skill slot of the target's
-- 'Ninja.copies'.
teachOne :: ∀ m. MonadPlay m
         => Turns -- ^ 'Copy.dur'.
         -> Int -- ^ Target's skill slot to copy into.
         -> Int -- ^ User's skill slot of the @Skill@ to copy.
         -> m ()
teachOne dur s = teacher (Seq.update s) dur

-- | Copies a @Skill@ from the user into the target.
teacher :: ∀ m. MonadPlay m
        => (Maybe Copy -> Seq (Maybe Copy) -> Seq (Maybe Copy))
        -- ^ Determines how to modify the target's 'Ninja.copies' skill slots.
        -> Turns -- ^ 'Copy.dur'.
        -> Int -- ^ User's skill slot of the @Skill@ to copy.
        -> m ()
teacher f (sync . Duration -> dur) s = P.uncopied do
    nUser  <- P.nUser
    target <- P.target
    let copied = Just $ Copy { skill = Ninjas.getSkill (Left s) nUser
                             , dur   = dur + dur `rem` 2
                             }
    P.modify target \n -> n { Ninja.copies = f copied $ Ninja.copies n }

-- | Resets a 'Ninja.Ninja' to their initial state.
-- Uses 'Ninjas.factory' internally.
factory :: ∀ m. MonadPlay m => m ()
factory = do
    target <- P.target
    P.toTarget Ninjas.factory
    P.modifyAll $ unSoulbound target

-- | Restores a target to an earlier state. Charges are preserved.
replaceWith :: ∀ m. MonadPlay m => Ninja -> m ()
replaceWith n = P.toTarget \n' -> n { Ninja.charges = Ninja.charges n' }
