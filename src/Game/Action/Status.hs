-- | Actions that characters can use to affect @Status@es.
module Game.Action.Status
  ( -- * Applying statuses
    apply, applyWith
  , tag, tagWith
  , flag
  , hide
  , addStack, addStacks, addStacks', applyStacks
  , control, control'
    -- * Applying bombs
  , bomb, bombWith
  -- * Adjusting statuses
  , refresh, prolong, hasten
  -- * Removing statuses
  , cureAll, cureBane, cureStun, purge, remove, removeStack, removeStacks
  -- * Specialized
  , commandeer
  ) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Enum.Set (EnumSet)

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import           Class.Stackable ((.++))
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Statuses as Statuses
import           Game.Model.Channel (Channeling(..))
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Duration (Duration(..))
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Effect as Effect
import qualified Game.Model.Ninja as N
import           Game.Model.Runnable (Runnable)
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Game.Model.Status (Bomb(..), Status(Status))
import qualified Game.Model.Status as Status
import           Util ((∈))

-- | Refreshes the 'Status.dur' of 'N.statuses' with matching 'Status.name'
-- to 'Status.maxDur'.
-- Uses 'Ninjas.refresh' internally.
refresh :: ∀ m. MonadPlay m => Text -> m ()
refresh name = P.unsilenced
    $ P.toTargetFromUser Ninjas.refresh name

-- | Increases the 'Status.dur' of 'N.statuses' with matching 'Status.name'.
-- Uses 'Ninjas.prolong' internally.
prolong :: ∀ m. MonadPlay m => Duration -> Text -> m ()
prolong dur name = P.unsilenced
    $ P.toTargetFromUser (Ninjas.prolong dur) name

-- | Reduces the 'Status.dur' of 'N.statuses' with matching 'Status.name'.
-- Uses 'Ninjas.prolong' internally.
hasten :: ∀ m. MonadPlay m => Duration -> Text -> m ()
hasten dur name = P.unsilenced
    $ P.toTargetFromUser (Ninjas.prolong $ negate dur) name

-- | Adds a @Status@ to 'N.statuses'.
apply :: ∀ m. MonadPlay m => Duration -> Text -> [Effect] -> m ()
apply = applyWith mempty

-- | 'apply' with extra 'Status.classes'.
applyWith :: ∀ m. MonadPlay m
           => EnumSet Class -> Duration -> Text -> [Effect] -> m ()
applyWith classes turns name efs = P.unsilenced
    $ Statuses.apply 1 classes [] turns name efs

-- | Adds a simple @Status@ with no 'Status.effects' or 'Status.dur'
-- 'N.statuses'. Stacks are unremovable.
addStack :: ∀ m. MonadPlay m => Text -> m ()
addStack name = addStacks' Permanent name 1

-- | 'addStack' with a 'Status.name' and 'Status.amount'.
addStacks :: ∀ m. MonadPlay m => Text -> Int -> m ()
addStacks = addStacks' Permanent

-- | 'addStack' with a 'Status.dur', 'Status.name', and 'Status.amount'.
-- Uses 'Ninjas.addStatus' internally.
addStacks' :: ∀ m. MonadPlay m => Duration -> Text -> Int -> m ()
addStacks' dur name i = do
    Context{skill, target, user} <- P.context
    P.modify target $ Ninjas.addStatus (Status.new user dur skill)
        { Status.name    = Skill.provideName skill name
        , Status.amount  = i
        , Status.user    = user
        , Status.classes = deleteSet Nonstacking . deleteSet Continues
                        $ insertSet Unremovable skill.classes
        }

-- | Adds a hidden @Status@ with no effects that immediately expires.
flag :: ∀ m. MonadPlay m => Text -> m ()
flag name = do
    Context{skill, target, user} <- P.context
    P.modify target $ Ninjas.addStatus (Status.new user 0 skill)
        { Status.name    = if null name then toLower skill.name else name
        , Status.classes = skill.classes ++ extraClasses
        }
  where
    extraClasses = setFromList [Hidden, Unremovable, Nonstacking]

-- | Applies a @Status@ with no effects, used as a marker for other
-- 'Skill.Skill's.
tag :: ∀ m. MonadPlay m => Duration -> Text -> m ()
tag = tagWith mempty
-- | 'tag' with extra classes.
tagWith :: ∀ m. MonadPlay m => EnumSet Class -> Duration -> Text -> m ()
tagWith classes dur name = applyWith (classes ++ extraClasses) dur name []
  where
    extraClasses = setFromList [Unremovable, Nonstacking]

-- | Applies a 'Hidden' and 'Unremovable' @Status@.
hide :: ∀ m. MonadPlay m => Duration -> Text -> [Effect] -> m ()
hide = applyWith $ setFromList [Unremovable, Hidden]

controlWith :: ∀ m. MonadPlay m => EnumSet Class -> [Effect] -> m ()
controlWith classes effects = P.unsilenced $ void $ runMaybeT do
    context@Context{target, skill = Skill{dur = Control i}} <- P.context
    nUser   <- P.nUser
    nTarget <- P.nTarget
    let status = Statuses.makeStatus Statuses.StatusParams
            { context
            , amount = 1
            , nUser
            , nTarget
            , classes = Controlled `insertSet` classes
            , name = ""
            , dur = i
            , effects
            , bombs = mempty
            }
    P.modify target $ Ninjas.addStatus status
    Statuses.triggerStatusApplied status.effects

control :: ∀ m. MonadPlay m => [Effect] -> m ()
control = controlWith mempty

control' :: ∀ m. MonadPlay m => [Effect] -> m ()
control' = controlWith $ setFromList [Hidden, Unremovable]

-- | Adds a @Status@ with 'Status.bombs' to 'N.statuses'.
-- @Bomb@s apply an effect when the @Status@ ends. If the @Bomb@ type is
-- 'Status.Expire', the bomb activates if the @Status@ naturally reaches the end
-- of its 'Status.dur'. If the @Bomb@ type is 'Status.Remove', the @Bomb@
-- activates if the @Status@ is removed before naturally reaching the end of its
-- 'Status.dur'. If the @Bomb@ type is 'Status.Done', the bomb activates in both
-- situations.
bomb :: ∀ m. MonadPlay m
      => Duration -> Text -> [Effect] -> [Runnable Bomb] -> m ()
bomb = bombWith mempty
-- | @Bomb@ with extra 'Status.classes'.
bombWith :: ∀ m. MonadPlay m
          => EnumSet Class -> Duration -> Text -> [Effect] -> [Runnable Bomb]
          -> m ()
bombWith classes dur name effects bombs = P.unsilenced
    $ Statuses.apply 1 classes bombs dur name effects

-- | 'addStacks' with 'Status.effect's.
applyStacks :: ∀ m. MonadPlay m
            => Text -> Int -> [Effect]
            -> m ()
applyStacks name amount =
    Statuses.apply amount (singleton Unremovable) mempty Permanent name

-- | Removes non-'Effect.helpful' effects in 'N.statuses' that match a
-- predicate.
-- Uses 'Ninjas.cure' internally.
cure :: ∀ m. MonadPlay m => (Effect -> Bool) -> m ()
cure match = P.unsilenced . P.toTarget $ Ninjas.cure match

-- | Removes all non-'Effect.helpful' 'effects in 'N.statuses'.
-- Uses 'Ninjas.cure' internally.
cureAll :: ∀ m. MonadPlay m => m ()
cureAll = P.unsilenced . cure $ const True

-- | Removes all 'N.statuses' with 'Bane' in their 'Status.classes'.
-- Uses 'Ninjas.cureBane' internally.
cureBane :: ∀ m. MonadPlay m => m ()
cureBane = P.unsilenced $ P.toTarget Ninjas.cureBane

-- | Cures all 'Stun' effects from 'N.statuses'.
-- Uses 'Ninjas.cure' internally.
cureStun :: ∀ m. MonadPlay m => m ()
cureStun = P.unsilenced $ cure Effect.isDisable

-- | Cures all 'Effect.helpful' effects from 'N.statuses'.
-- Uses 'Ninjas.purge' internally.
purge :: ∀ m. MonadPlay m => m ()
purge = P.unsilenced $ P.toTarget Ninjas.purge

-- | Removes all @Status@es with matching 'Status.name' and whose 'Status.user'
-- is the one performing the action.
-- Uses 'Ninjas.clear' internally.
remove :: ∀ m. MonadPlay m => Text -> m ()
remove name = P.toTargetFromUser Ninjas.clear name

-- | Decreases the 'Status.amount' of a @Status@ with matching 'Status.name' by
-- 1, removing it if it reaches 0.
-- Uses 'Ninjas.removeStacks' internally.
removeStack :: ∀ m. MonadPlay m => Text -> m ()
removeStack name = removeStacks name 1

-- | Decreases the 'Status.amount' of a @Status@ with matching 'Status.name' and
-- whose 'Status.user is the one performing the action by some amount, removing
-- it if it reaches 0.
-- Uses 'Ninjas.removeStacks' internally.
removeStacks :: ∀ m. MonadPlay m => Text -> Int -> m ()
removeStacks name i = P.toTargetFromUser (Ninjas.removeStacks i) name

-- | Steals all of the target's 'Effect.helpful' 'Effect's.
commandeer :: ∀ m. MonadPlay m => m ()
commandeer = P.unsilenced do
    nUser   <- P.nUser
    nTarget <- P.nTarget

    P.modify nUser.slot $ Ninjas.modifyStatuses
        (mapMaybe gainHelpful nTarget.statuses ++) . \n ->
            n { N.defense = nTarget.defense .++ n.defense
              , N.barrier = mempty
              }
    P.modify nTarget.slot $ Ninjas.modifyStatuses
        (mapMaybe loseHelpful) . \n ->
            n { N.defense = mempty
              , N.barrier = nUser.barrier
              }
  where
    stealable ef = Effect.helpful ef && not (Effect.sticky ef)

    loseHelpful st@Status{classes, effects}
      | null effects          = Just st
      | Unremovable ∈ classes = Just st
      | null kept             = Nothing
      | otherwise             = Just st { Status.effects = kept }
      where
        kept = filter (not . stealable) effects

    gainHelpful st@Status{classes, effects}
      | null effects          = Nothing
      | Unremovable ∈ classes = Nothing
      | null stolen           = Nothing
      | otherwise             = Just st { Status.effects = stolen }
      where
        stolen = filter stealable effects
