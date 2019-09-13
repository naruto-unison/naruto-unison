-- | Actions that characters can use to affect @Skill@s.
module Action.Skill
  ( -- * Cooldowns and charges
    alterCd
  , reset, resetAll, resetCharges
  -- * Copying
  , copyAll, copyLast, teach, teachOne
  -- * Variants
  , vary, vary', varyLoadout, varyNext
  ) where

import ClassyPrelude

import           Data.List (findIndex)
import qualified Data.Sequence as Seq

import qualified Class.Play as P
import           Class.Play (MonadPlay)
import qualified Model.Channel as Channel
import           Model.Channel (Channeling(..))
import qualified Model.Character as Character
import qualified Model.Copy as Copy
import           Model.Copy (Copy(Copy), Copying)
import           Model.Duration (Duration(..), Turns, incr, sync)
import qualified Model.Ninja as Ninja
import qualified Model.Skill as Skill
import           Model.Slot (Slot)
import qualified Model.Variant as Variant
import           Model.Variant (Varying)
import qualified Engine.Cooldown as Cooldown
import qualified Engine.Execute as Execute
import qualified Engine.Ninjas as Ninjas
import qualified Engine.Skills as Skills

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

-- | Copies all @Skill@s from the target into the user's 'Ninja.copies'.
-- Uses 'Ninjas.copyAll' internally.
copyAll :: ∀ m. MonadPlay m => Turns -> m ()
copyAll (Duration -> dur) = do
    nTarget <- P.nTarget
    user    <- P.user
    P.modify user $ Ninjas.copyAll dur nTarget

-- | Copies the 'Ninja.lastSkill' of the target into a specific skill slot
-- of the user's 'Ninja.copies'. Uses 'Execute.copy' internally.
copyLast :: ∀ m. MonadPlay m => Turns -> m ()
copyLast (Duration -> dur) = do
    skill      <- P.skill
    user       <- P.user
    target     <- P.target
    mLastSkill <- Ninja.lastSkill <$> P.nTarget
    forM_ mLastSkill \lastSkill ->
        Execute.copy Copy.Shallow target lastSkill (dur, user, Skill.name skill)

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
teacher f (Duration -> dur) cop s = do
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
