-- | Actions that characters can use to affect 'Skill.Skill's.
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

import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Sequence as Seq

import qualified Class.Play as P
import           Class.Play (MonadPlay)
import qualified Model.Channel as Channel
import           Model.Channel (Channeling(..))
import qualified Model.Character as Character
import qualified Model.Copy as Copy
import           Model.Copy (Copy, Copying)
import           Model.Duration (Duration(..), Turns, incr, sync)
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Model.Skill as Skill
import           Model.Slot (Slot)
import qualified Model.Variant as Variant
import qualified Engine.Adjust as Adjust
import qualified Engine.Cooldown as Cooldown
import qualified Engine.Execute as Execute
import qualified Engine.SkillTransform as SkillTransform

-- | Changes the 'Skill.cooldown' of a 'Skill.Skill.'
-- Uses 'Cooldown.alter' internally.
alterCd :: ∀ m. MonadPlay m => Int -> Int -> Int -> m ()
alterCd s v = P.toTarget . Cooldown.alter s v

-- | Resets 'Ninja.cooldowns' with a matching 'Skill.name' of a 'Ninja.Ninja'.
-- Uses 'Cooldown.reset' internally.
reset :: ∀ m. MonadPlay m => Text -> Text -> m ()
reset name = P.toTarget . Cooldown.reset name

-- | Resets all 'Ninja.cooldowns' of a 'Ninja.Ninja'.
-- Uses 'Cooldown.resetAll' internally.
resetAll :: ∀ m. MonadPlay m => m ()
resetAll = P.toTarget Cooldown.resetAll

-- | Resets all 'Ninja.charges' of a 'Ninja.Ninja'.
-- Uses 'Ninja.resetCharges' internally.
resetCharges :: ∀ m. MonadPlay m => m ()
resetCharges = P.toTarget Ninja.resetCharges

-- | Adds a 'Variant.Variant' to 'Ninja.variants' with a 'Variant.dur' that
-- depends on the 'Skill.dur' of the 'Skill.Skill' that performs the action.
-- If the 'Skill.Skill' is interrupted, the 'Variant.Variant' immediately ends.
vary :: ∀ m. MonadPlay m
     => Text -- ^ 'Skill.name' of root skill.
     -> Text -- ^ 'Skill.name' of variant skill.
     -> m ()
vary name variant = do
    dur <- Channel.turnDur . Skill.channel <$> P.skill
    unless (dur == Duration (-1)) $ varyFull True dur name variant

-- | Adds a 'Variant.Variant' to 'Ninja.variants' with a fixed 'Variant.dur'.
vary' :: ∀ m. MonadPlay m
      => Turns -- Custom 'Variant.dur'.
      -> Text -- ^ 'Skill.name' of root skill.
      -> Text -- ^ 'Skill.name' of variant skill.
      -> m ()
vary' = varyFull False . Duration

-- | Adds a 'Variant.Variant' to 'Ninja.variants' by base 'Skill.name' and
-- variant 'Skill.name'.
varyFull :: ∀ m. MonadPlay m => Bool -> Duration -> Text -> Text -> m ()
varyFull from dur name variant = do
    nUser <- P.nUser
    SkillTransform.safe (return ()) (unsafeVary from dur) nUser name variant

-- | Adds a 'Variant.Variant' to 'Ninja.variants' by skill and variant index
-- within 'Character.skills'.
unsafeVary :: ∀ m. MonadPlay m => Bool -> Duration -> Int -> Int -> m ()
unsafeVary fromSkill dur s v = do
    skill      <- P.skill
    nUser      <- P.nUser
    let copying = Skill.copying skill
    unless (shallow copying) do
        target     <- P.target
        let dur'    = Copy.maxDur copying . sync $ incr dur
            variant = Variant.Variant
                { Variant.variant   = v
                , Variant.ownCd     = Skill.varicd $ Adjust.skill' nUser s v
                , Variant.name      = case Skill.channel skill of
                    Instant -> ""
                    _       -> Skill.name skill
                , Variant.fromSkill = fromSkill
                , Variant.dur       = dur'
                }
            adjust
              | dur' <= 0 = Seq.update s $ variant :| []
              | otherwise = Seq.adjust' (cons variant) s
        P.modify $ Game.adjust target \n ->
            n { Ninja.variants = adjust $ Ninja.variants n }
  where
    shallow Copy.Shallow{} = True
    shallow _              = False

-- | Adjusts all 'Ninja.variants' at once.
varyLoadout :: ∀ m. MonadPlay m
            => (Int, Int, Int, Bool) -- ^ 'Variant.Variant' offsets for first, second, and third slots, along with whether or not to affect the fourth slot.
            -> Int  -- ^ Counter added to all 'Variant.Variant' slots.
            -> m () -- ^ Recalculates every 'Variant.Variant' of a target 'Ninja.Ninja'.
varyLoadout (a, b, c, affectsFourth) i = do
    unsafeVary False 0 0 $ i + a
    unsafeVary False 0 1 $ i + b
    unsafeVary False 0 2 $ i + c
    when affectsFourth $ unsafeVary False 0 3 i

-- | Increments the 'Variant.variant' of a 'Ninja.variants' with matching
-- 'Skill.name'.
varyNext :: ∀ m. MonadPlay m => Text -> m ()
varyNext name = do
    target <- P.target
    maybeS <- List.findIndex (any match) .
              toList . Character.skills . Ninja.character <$> P.nTarget
    case maybeS of
        Nothing -> return ()
        Just s  -> P.modify $ Game.adjust target \n ->
            n { Ninja.variants = Seq.adjust' adj s $ Ninja.variants n }
  where
    match = (== toCaseFold name) . toCaseFold . Skill.name
    adj vs@(x:|xs)
      | variant <= 0 = vs
      | otherwise    = x { Variant.variant = 1 + variant } :| xs
      where
        variant = Variant.variant x

-- | Copies all 'Skill.Skill's from the target into the user's 'Ninja.copies'.
copyAll :: ∀ m. MonadPlay m => Turns -> m ()
copyAll (Duration -> dur) = do
    user    <- P.user
    target  <- P.target
    nTarget <- P.nTarget
    let copy skill = Just $ Copy.Copy { Copy.dur   = dur'
                                      , Copy.skill = copying skill target
                                      }
    P.modify $ Game.adjust user \n ->
        n { Ninja.copies = fromList $ copy <$> Adjust.skills nTarget }
  where
    synced = sync dur
    dur'   = synced + synced `rem` 2
    copying skill target = skill
        { Skill.copying = Copy.Deep (Copy.source skill target) $ sync dur - 1 }

-- | Copies the 'Ninja.lastSkill' of the target into a specific skill slot
-- of the user's 'Ninja.copies'. Uses 'Execute.copy' internally.
copyLast :: ∀ m. MonadPlay m => Turns -> Int -> m ()
copyLast (Duration -> dur) s = do
    skill      <- P.skill
    user       <- P.user
    target     <- P.target
    mLastSkill <- Ninja.lastSkill <$> P.nTarget
    case mLastSkill of
        Nothing -> return ()
        Just lastSkill -> P.modify $
            Execute.copy False Copy.Shallow target lastSkill
            (user, Skill.name skill, s, dur)

-- | Copies a 'Skill.Skill' from the user into all four of the target's
-- 'Ninja.copies' skill slots.
teach :: ∀ m. MonadPlay m
      => Turns -- ^ 'Copy.dur'.
      -> (Slot -> Int -> Copying) -- ^ Either 'Copy.Deep' or 'Copy.Shallow'.
      -> Int -- ^ User's skill slot of the 'Skill.Skill' to copy.
      -> m ()
teach = teacher $ const . replicate 4

-- | Copies a 'Skill.Skill' from the user into a specific skill slot of the
-- target's 'Ninja.copies'.
teachOne :: ∀ m. MonadPlay m
         => Turns -- ^ 'Copy.dur'.
         -> Int -- ^ Target's skill slot to copy into.
         -> (Slot -> Int -> Copying) -- ^ Either 'Copy.Deep' or 'Copy.Shallow'.
         -> Int -- ^ User's skill slot of the 'Skill.Skill' to copy.
         -> m ()
teachOne dur s' = teacher (Seq.update s') dur

-- | Copies a 'Skill.Skill' from the user into the target.
teacher :: ∀ m. MonadPlay m
        => (Maybe Copy -> Seq (Maybe Copy) -> Seq (Maybe Copy))
        -- ^ Determines how to modify the target's 'Ninja.copies' skill slots.
        -> Turns -- ^ 'Copy.dur'.
        -> (Slot -> Int -> Copying) -- ^ Either 'Copy.Deep' or 'Copy.Shallow'.
        -> Int -- ^ User's skill slot of the 'Skill.Skill' to copy.
        -> m ()
teacher f (Duration -> dur) cop s = do
    user   <- P.user
    target <- P.target
    nUser  <- P.nUser
    let skill  = (Adjust.skill (Left s) nUser)
                 { Skill.copying = cop user $ sync dur - 1 }
        copied = Just $ Copy.Copy { Copy.skill = skill
                                  , Copy.dur   = dur'
                                  }
    P.modify $ Game.adjust target \n ->
        n { Ninja.copies = f copied $ Ninja.copies n }
  where
    synced = sync dur
    dur'   = synced + synced `rem` 2
