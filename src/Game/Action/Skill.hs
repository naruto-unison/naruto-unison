-- | Actions that characters can use to affect @Skill@s.
module Game.Action.Skill
  ( -- * Cooldowns and charges
    alterCd
  , reset, resetAll, recharge, rechargeAll
  -- * Changing face icons
  -- * Copying
  , copyAll, copyLast, teach
  -- * Alternates
  , alternate, nextAlternate

  -- * Other
  , factory, replaceWith
  ) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Enum.Set (EnumSet)
import Data.List (findIndex)

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import           Game.Action.Status (applyWith')
import           Game.Engine (unSoulbound)
import qualified Game.Engine.Cooldown as Cooldown
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import           Game.Model.Duration (Duration(..))
import           Game.Model.Effect (Effect(..))
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as N
import qualified Game.Model.Skill as Skill
import           Game.Model.Trigger (Trigger(..))
import           Util ((!?))

-- | Changes the 'Skill.cooldown' of a @Skill@ by 'Skill.name'.
-- Uses 'Cooldown.alter' internally.
alterCd :: ∀ m. MonadPlay m => Text -> Int -> m ()
alterCd name cd =
    P.unsilenced . P.toTarget . Cooldown.alter name cd . Skill.owner =<< P.skill

-- | Resets 'N.cooldowns' with a matching 'Skill.name' of a @Ninja@.
-- Uses 'Cooldown.reset' internally.
reset :: ∀ m. MonadPlay m => Text -> m ()
reset name =
    P.unsilenced . P.toTarget . Cooldown.reset name . Skill.owner =<< P.skill

-- | Resets all 'N.cooldowns' of a @Ninja@.
-- Uses 'Cooldown.resetAll' internally.
resetAll :: ∀ m. MonadPlay m => m ()
resetAll = P.unsilenced $ P.toTarget Cooldown.resetAll

-- | Resets an element in 'N.charges' of a @Ninja@.
-- Uses 'Ninjas.recharge' internally.
recharge :: ∀ m. MonadPlay m => Text -> m ()
recharge name =
    P.unsilenced . P.toTarget . Ninjas.recharge name . Skill.owner =<< P.skill

-- | Resets all 'N.charges' of a @Ninja@.
-- Uses 'Ninjas.rechargeAll' internally.
rechargeAll :: ∀ m. MonadPlay m => m ()
rechargeAll = P.unsilenced $ P.toTarget Ninjas.rechargeAll

alternateClasses :: EnumSet Class
alternateClasses = setFromList [Hidden, Nonstacking, Unremovable]

-- | Adjusts all 'N.alternates' at once.
alternate :: ∀ m. MonadPlay m
          => [Int] -- ^ Index offsets.
          -> Int   -- ^ Counter added to all 'N.alternates' slots.
          -> m () -- ^ Recalculates every alternate of a target @Ninja@.
alternate loadout i = applyWith' alternateClasses "loadout" Permanent
    . catMaybes . zipWith load loadout . toList . Character.skills
    . N.character =<< P.nTarget
  where
    load alt (x:|xs) =
        Alternate (Skill.name x) . Skill.name <$> xs !? (alt + i - 1)

-- | Cycles a skill through its list of alternates.
-- | Uses 'Ninjas.nextAlternate' internally.
nextAlternate :: ∀ m. MonadPlay m => Text -> m ()
nextAlternate name = mapM_ alt . Ninjas.nextAlternate name =<< P.nTarget
  where
    alt x = applyWith' alternateClasses "nextAlternate" 1 [Alternate name x]

-- | Copies all @Skill@s from the target into the user's 'N.copies'.
-- Uses 'Ninjas.copyAll' internally.
copyAll :: ∀ m. MonadPlay m => Duration -> m ()
copyAll dur = P.uncopied do
    nTarget <- P.nTarget
    user    <- P.user
    P.modify user $ Ninjas.copyAll dur nTarget

-- | Copies the 'N.lastSkill' of the target into a specific skill slot
-- of the user's 'N.copies'. Uses 'Execute.copy' internally.
copyLast :: ∀ m. MonadPlay m => Duration -> m ()
copyLast (succ -> dur) = P.uncopied . void $ runMaybeT do
    name  <- Skill.name <$> P.skill
    s     <- MaybeT $ findIndex (any $ (== name) . Skill.name) . toList
           . Character.skills . N.character <$> P.nUser
    skill <- MaybeT $ N.lastSkill <$> P.nTarget
    user  <- P.user
    P.modify user $ Ninjas.copy dur [s] skill

teach :: ∀ m. MonadPlay m
       => Duration -- ^ 'Copy.dur'.
       -> Text
       -> [Int]
       -> m ()
teach dur name slots = mapM_ (P.toTarget . Ninjas.copy dur slots)
    . find ((== name) . Skill.name) . concatMap toList . Character.skills
    . N.character =<< P.nUser

-- | Resets a 'N.Ninja' to their initial state.
-- Uses 'Ninjas.factory' internally.
factory :: ∀ m. MonadPlay m => m ()
factory = do
    target <- P.target
    alive  <- N.alive <$> P.nTarget
    P.toTarget Ninjas.factory
    P.modifyAll $ unSoulbound target
    alive' <- N.alive <$> P.nTarget
    when (alive' && not alive) do
        user <- P.user
        P.trigger user [OnHeal]

-- | Restores a target to an earlier state. Charges are preserved.
replaceWith :: ∀ m. MonadPlay m => Ninja -> m ()
replaceWith n = P.toTarget \n' -> n { N.charges = N.charges n' }
