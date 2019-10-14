-- | Actions that characters can use to affect @Skill@s.
module Game.Action.Skill
  ( -- * Cooldowns and charges
    alterCd
  , reset, resetAll, resetCharges
  -- * Changing face icons
  -- * Copying
  , copyAll, copyLast, teach, teachOne
  -- * Alternates
  , alternate, nextAlternate

  -- * Other
  , factory
  ) where

import ClassyPrelude

import           Data.Enum.Set.Class (EnumSet)
import qualified Data.Sequence as Seq

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import           Game.Action.Status (applyWith')
import qualified Game.Engine.Cooldown as Cooldown
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Skills as Skills
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import           Game.Model.Copy (Copy(Copy), Copying(..))
import qualified Game.Model.Copy as Copy
import           Game.Model.Duration (Duration(..), Turns, sync)
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Util ((!?))


-- | Changes the 'Skill.cooldown' of a @Skill@ by base 'Skill.name' and variant
-- 'Skill.name'.
-- Uses 'Cooldown.alter' internally.
alterCd :: ∀ m. MonadPlay m => Text -> Text -> Int -> m ()
alterCd s v cd = do
    nUser <- P.nUser
    Skills.safe (return ()) (unsafeAlterCd cd) nUser s v

-- | Changes the 'Skill.cooldown' of a @Skill@ by skill and variant index within
-- 'Character.skills'.
unsafeAlterCd :: ∀ m. MonadPlay m => Int -> Int -> Int -> m ()
unsafeAlterCd cd s v = P.unsilenced . P.toTarget $ Cooldown.alter s v cd

-- | Resets 'Ninja.cooldowns' with a matching 'Skill.name' of a @Ninja@.
-- Uses 'Cooldown.reset' internally.
reset :: ∀ m. MonadPlay m => Text -> Text -> m ()
reset name v = P.unsilenced . P.toTarget $ Cooldown.reset name v

-- | Resets all 'Ninja.cooldowns' of a @Ninja@.
-- Uses 'Cooldown.resetAll' internally.
resetAll :: ∀ m. MonadPlay m => m ()
resetAll = P.unsilenced $ P.toTarget Cooldown.resetAll

-- | Resets all 'Ninja.charges' of a @Ninja@.
-- Uses 'Ninjas.resetCharges' internally.
resetCharges :: ∀ m. MonadPlay m => m ()
resetCharges = P.unsilenced $ P.toTarget Ninjas.resetCharges

alternateClasses :: EnumSet Class
alternateClasses = setFromList [Hidden, Nonstacking, Unremovable]

-- | Adjusts all 'Ninja.alternates' at once.
alternate :: ∀ m. (MonadPlay m, MonadRandom m)
          => [Int] -- ^ Index offsets.
          -> Int   -- ^ Counter added to all 'Ninja.alternates' slots.
          -> m () -- ^ Recalculates every alternate of a target @Ninja@.
alternate loadout i =
    applyWith' alternateClasses "loadout" 0 . catMaybes . zipWith load loadout .
    toList . Character.skills . Ninja.character =<< P.nTarget
  where
    load alt (x:|xs) =
        Alternate (Skill.name x) . Skill.name <$> xs !? (alt + i - 1)

nextAlternate :: ∀ m. (MonadPlay m, MonadRandom m) => Text -> m ()
nextAlternate name = mapM_ alt . Ninjas.nextAlternate name =<< P.nTarget
  where
    alt x = applyWith' alternateClasses "nextAlternate" 1 [Alternate name x]

-- | Performs an action only if the skill being used is not copied from
-- someone else.
uncopied :: ∀ m. MonadPlay m => m () -> m ()
uncopied f = do
    skill <- P.skill
    case Skill.copying skill of
        NotCopied -> f
        _         -> return ()

-- | Copies all @Skill@s from the target into the user's 'Ninja.copies'.
-- Uses 'Ninjas.copyAll' internally.
copyAll :: ∀ m. MonadPlay m => Turns -> m ()
copyAll (Duration -> dur) = uncopied do
    nTarget <- P.nTarget
    user    <- P.user
    P.modify user $ Ninjas.copyAll dur nTarget

-- | Copies the 'Ninja.lastSkill' of the target into a specific skill slot
-- of the user's 'Ninja.copies'. Uses 'Execute.copy' internally.
copyLast :: ∀ m. MonadPlay m => Turns -> m ()
copyLast (Duration -> dur) = uncopied do
    name   <- Skill.name <$> P.skill
    user   <- P.user
    target <- P.target
    mapM_ (P.modify user . Ninjas.copy dur name Copy.Shallow target) .
          Ninja.lastSkill =<< P.nTarget

-- | Copies a @Skill@ from the user into all of the target's 'Ninja.copies'
-- skill slots.
teach :: ∀ m. MonadPlay m
      => Turns -- ^ 'Copy.dur'.
      -> (Slot -> Int -> Copying) -- ^ Either 'Copy.Deep' or 'Copy.Shallow'.
      -> Int -- ^ User's skill slot of the @Skill@ to copy.
      -> m ()
teach = teacher $ map . const

-- | Copies a @Skill@ from the user into a specific skill slot of the target's
-- 'Ninja.copies'.
teachOne :: ∀ m. MonadPlay m
         => Turns -- ^ 'Copy.dur'.
         -> Int -- ^ Target's skill slot to copy into.
         -> (Slot -> Int -> Copying) -- ^ Either 'Copy.Deep' or 'Copy.Shallow'.
         -> Int -- ^ User's skill slot of the @Skill@ to copy.
         -> m ()
teachOne dur s = teacher (Seq.update s) dur

-- | Copies a @Skill@ from the user into the target.
teacher :: ∀ m. MonadPlay m
        => (Maybe Copy -> Seq (Maybe Copy) -> Seq (Maybe Copy))
        -- ^ Determines how to modify the target's 'Ninja.copies' skill slots.
        -> Turns -- ^ 'Copy.dur'.
        -> (Slot -> Int -> Copying) -- ^ Either 'Copy.Deep' or 'Copy.Shallow'.
        -> Int -- ^ User's skill slot of the @Skill@ to copy.
        -> m ()
teacher f (Duration -> dur) cop s = uncopied do
    user   <- P.user
    target <- P.target
    nUser  <- P.nUser
    let skill  = (Ninjas.skill (Left s) nUser)
                 { Skill.copying = cop user $ sync dur - 1 }
        copied = Just $ Copy { Copy.skill = skill
                             , Copy.dur   = dur'
                             }
    P.modify target \n -> n { Ninja.copies = f copied $ Ninja.copies n }
  where
    synced = sync dur
    dur'   = synced + synced `rem` 2

-- | Resets a 'Ninja.Ninja' to their initial state.
-- Uses 'Ninjas.factory' internally.
factory :: ∀ m. MonadPlay m => m ()
factory = P.toTarget Ninjas.factory
