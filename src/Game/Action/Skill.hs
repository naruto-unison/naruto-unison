-- | Actions that characters can use to affect @Skill@s.
module Game.Action.Skill
  ( -- * Cooldowns and charges
    alterCd
  , reset, resetAll, resetCharges
  -- * Changing face icons
  , setFace, setFace'
  -- * Copying
  , copyAll, copyLast, teach, teachOne
  -- * Variants
  , vary, vary', varyLoadout, varyNext
  -- * Other
  , factory
  ) where

import ClassyPrelude

import           Data.List (findIndex)
import qualified Data.Sequence as Seq

import qualified Class.Play as P
import           Class.Play (MonadPlay)
import qualified Game.Model.Channel as Channel
import           Game.Model.Channel (Channeling(..))
import qualified Game.Model.Character as Character
import qualified Game.Model.Copy as Copy
import           Game.Model.Copy (Copy(Copy), Copying(..))
import           Game.Model.Duration (Duration(..), Turns, incr, sync)
import qualified Game.Model.Face as Face
import           Game.Model.Face (Face(Face))
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Variant as Variant
import           Game.Model.Variant (Varying)
import qualified Game.Engine.Cooldown as Cooldown
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Skills as Skills

-- | Changes the 'Skill.cooldown' of a @Skill@.
-- Uses 'Cooldown.alter' internally.
alterCd :: ∀ m. MonadPlay m => Int -> Int -> Int -> m ()
alterCd s v cd = P.unsilenced . P.toTarget $ Cooldown.alter s v cd

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

-- | Adds a 'Face.Face' to 'Ninja.face' with a 'Face.dur' that
-- depends on the 'Skill.dur' of the @Skill@ that performs the action.
-- If the @Skill@ is interrupted, the 'Face.Face' immediately ends.
setFace :: ∀ m. MonadPlay m => m ()
setFace = do
    skill <- P.skill
    case Skill.dur skill of
        Instant -> setFace' 0
        (Channel.turnDur -> Duration (-1)) -> return ()
        _ -> setFaceFull . Variant.FromSkill $ Skill.name skill

-- | Adds a 'Face.Face' to 'Ninja.face' with a fixed 'Face.dur'.
setFace' :: ∀ m. MonadPlay m => Turns -> m ()
setFace' (Duration -> dur) = do
    copying <- Skill.copying <$> P.skill
    setFaceFull . Variant.Duration . Copy.maxDur copying $ sync dur

-- | Adds a 'Face.Face' to the 'Ninja.face' of a @Ninja@, changing their in-game
-- icon.
setFaceFull :: ∀ m. MonadPlay m => Varying -> m ()
setFaceFull dur = do
    skill <- P.skill
    user  <- P.user
    let face = Face { Face.icon = Skill.name skill
                    , Face.user = user
                    , Face.dur  = dur
                    }
    P.toTarget \n -> n { Ninja.face = face : Ninja.face n }

-- | Adds a 'Variant.Variant' to 'Ninja.variants' with a 'Variant.dur' that
-- depends on the 'Skill.dur' of the @Skill@ that performs the action.
-- If the @Skill@ is interrupted, the 'Variant.Variant' immediately ends.
vary :: ∀ m. MonadPlay m
     => Text -- ^ 'Skill.name' of root skill.
     -> Text -- ^ 'Skill.name' of variant skill.
     -> m ()
vary name variant = do
    skill <- P.skill
    case Skill.dur skill of
        Instant -> vary' 0 name variant
        (Channel.turnDur -> Duration (-1)) -> return ()
        _ -> varyFull (Variant.FromSkill $ Skill.name skill) name variant

-- | Adds a 'Variant.Variant' to 'Ninja.variants' with a fixed 'Variant.dur'.
vary' :: ∀ m. MonadPlay m
      => Turns -- Custom 'Variant.dur'.
      -> Text -- ^ 'Skill.name' of root skill.
      -> Text -- ^ 'Skill.name' of variant skill.
      -> m ()
vary' (Duration -> dur) name variant = do
    copying <- Skill.copying <$> P.skill
    varyFull (Variant.Duration . Copy.maxDur copying . sync $ incr dur)
             name variant

-- | Adds a 'Variant.Variant' to 'Ninja.variants' by base 'Skill.name' and
-- variant 'Skill.name'.
varyFull :: ∀ m. MonadPlay m => Varying -> Text -> Text -> m ()
varyFull dur name variant = do
    nUser <- P.nUser
    Skills.safe (return ()) (unsafeVary dur) nUser name variant

-- | Adds a 'Variant.Variant' to 'Ninja.variants' by skill and variant index
-- within 'Character.skills'.
-- Uses 'Ninjas.vary' internally.
unsafeVary :: ∀ m. MonadPlay m => Varying -> Int -> Int -> m ()
unsafeVary dur s v = unlessM (shallow . Skill.copying <$> P.skill) do
    target <- P.target
    P.modify target $ Ninjas.vary dur s v
  where
    shallow Copy.Shallow{} = True
    shallow _              = False

-- | Adjusts all 'Ninja.variants' at once.
varyLoadout :: ∀ m. MonadPlay m
            => [Int]-- ^ 'Variant.Variant' offsets
            -> Int  -- ^ Counter added to all 'Variant.Variant' slots.
            -> m () -- ^ Recalculates every 'Variant.Variant' of a target @Ninja@.
varyLoadout els i = traverse_ f $ zip [0..] els
  where
    f (slot, offset) = unsafeVary (Variant.Duration 0) slot $ i + offset

-- | Increments the 'Variant.variant' of a 'Ninja.variants' with matching
-- 'Skill.name'.
varyNext :: ∀ m. MonadPlay m => Text -> m ()
varyNext name = do
    target <- P.target
    mapM_ (P.modify target . adjVariant) .
        findIndex (any match) . toList . Character.skills .
        Ninja.character =<< P.nTarget
  where
    adjVariant s n = n { Ninja.variants = Seq.adjust' adj s $ Ninja.variants n }
    match skill = name == Skill.name skill
    adj vs@(x:|xs)
      | variant <= 0 = vs
      | otherwise    = x { Variant.variant = 1 + variant } :| xs
      where
        variant = Variant.variant x

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
