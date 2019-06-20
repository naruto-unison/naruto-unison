-- | Monadic constraints for manipulating game state.
module Class.Play
  ( -- * Monads
    MonadGame(..), MonadPlay(..)
    -- * Actions stored in data structures
  , Play(..), PlayConstraint, SavedPlay, play, launch
    -- * Context
    -- ** From monad
  , skill
  , user, target
    -- ** From game
  , nUser, nTarget
  , player
  -- * Transformation
  , withContext
  , withSkill, withTarget, withTargets
  -- * Lifting
  , toTarget, fromSource
  ) where

import ClassyPrelude.Yesod hiding (Vector)

import           Class.Random (MonadRandom)
import           Model.Internal (MonadGame(..), Play(..), PlayConstraint, MonadPlay(..), SavedPlay)
import qualified Model.Context as Context
import           Model.Context (Context)
import qualified Model.Game as Game
import           Model.Ninja (Ninja)
import           Model.Player (Player)
import           Model.Skill (Skill)
import           Model.Slot (Slot)

-- | Unwraps a 'Play' object, revealing its inner monadic function.
-- While wrapped in the 'Play' newtype, game functions may be passed around
-- freely as objects and stored in data structures without exposing their
-- impredicative innards. They must be unwrapped before they can be applied.
play :: ∀ a. Play a -> PlayConstraint a
play (Play a) = a

-- | Alters the focus of the environment to a new 'Context'.
withContext :: ∀ m a. Context -> ReaderT Context m a -> m a
withContext ctx f = runReaderT f ctx

-- | Runs a stored 'Play' function with its associated stored 'Context'.
launch :: ∀ m. (MonadGame m, MonadRandom m) => SavedPlay -> m ()
launch (ctx, Play a) = runReaderT a ctx

-- | 'Skill' being used to perform an action.
skill :: ∀ m. MonadPlay m => m Skill
skill = Context.skill <$> context

-- | User of the action.
user :: ∀ m. MonadPlay m => m Slot
user = Context.user <$> context

-- | Target of the action. When an action affects multiple 'Ninja's, the
-- @target@ field is the only part of the 'Context' that changes.
target :: ∀ m. MonadPlay m => m Slot
target = Context.target <$> context

-- | The 'Game.ninja' indexed by 'user'.
nUser :: ∀ m. MonadPlay m => m Ninja
nUser = Game.ninja <$> user <*> game

-- | The 'Game.ninja' indexed by 'target'.
nTarget :: ∀ m. MonadPlay m => m Ninja
nTarget = Game.ninja <$> target <*> game

-- | The 'Player' whose turn it is.
player :: ∀ m. MonadGame m => m Player
player = Game.playing <$> game

-- | Runs an action in a localized state where 'skill' is replaced.
withSkill :: ∀ m a. MonadPlay m => Skill -> m a -> m a
withSkill x = with \ctx -> ctx { Context.skill = x }

-- | Runs an action in a localized state where 'target' is replaced.
withTarget :: ∀ m a. MonadPlay m => Slot -> m a -> m a
withTarget x = with \ctx -> ctx { Context.target = x }

-- | Runs an action against each 'target'.
withTargets :: ∀ m. MonadPlay m => [Slot] -> m () -> m ()
withTargets xs f = traverse_ (`withTarget` f) xs

-- | Applies a 'Ninja' transformation to the 'target'.
toTarget :: ∀ m. MonadPlay m => (Ninja -> Ninja) -> m ()
toTarget f = target >>= modify . flip Game.adjust f

-- | Applies a 'Ninja' transformation to the 'target', passing it the 'user'
-- as an argument.
fromSource :: ∀ m. MonadPlay m => (Slot -> Ninja -> Ninja) -> m ()
fromSource f = do
    t   <- target
    src <- user
    modify . Game.adjust t $ f src
