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
  , new
    -- ** From game
  , nUser, nTarget
  , player
  -- * Transformation
  , old
  , withContext
  , withTarget, withTargets
  , unsilenced
  -- * Lifting
  , toTarget, fromSource
  -- * Other
  , trigger
  , zipWith
  , yieldVictor, forfeit
  ) where

import ClassyPrelude hiding (zipWith)

import qualified Class.Parity as Parity
import           Class.Random (MonadRandom)
import           Model.Internal (MonadGame(..), Play(..), PlayConstraint, MonadPlay(..), SavedPlay)
import qualified Model.Context as Context
import           Model.Context (Context)
import           Model.Effect (Effect(..))
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Player as Player
import           Model.Player (Player)
import           Model.Skill (Skill)
import qualified Model.Slot as Slot
import           Model.Slot (Slot)
import           Model.Trap (Trigger)

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

-- | When new actions are used, they can trigger traps and counters.
-- All other actions, such as channeled actions past the first turn, delays,
-- and effects of traps, cannot.
new :: ∀ m. MonadPlay m => m Bool
new = Context.new <$> context

-- | The 'Game.ninja' indexed by 'user'.
nUser :: ∀ m. MonadPlay m => m Ninja
nUser = ninja =<< user

-- | The 'Game.ninja' indexed by 'target'.
nTarget :: ∀ m. MonadPlay m => m Ninja
nTarget = ninja =<< target

-- | The 'Player' whose turn it is.
player :: ∀ m. MonadGame m => m Player
player = Game.playing <$> game

-- | Runs an action in a localized state where 'Context.new' is 'False'.
old :: ∀ m a. MonadPlay m => m a -> m a
old f = do
    isNew <- new
    if isNew then with (\ctx -> ctx { Context.new = False }) f else f

-- | Runs an action in a localized state where 'target' is replaced.
withTarget :: ∀ m a. MonadPlay m => Slot -> m a -> m a
withTarget x = with \ctx -> ctx { Context.target = x }

-- | Runs an action against each 'target'.
withTargets :: ∀ m. MonadPlay m => [Slot] -> m () -> m ()
withTargets xs f = traverse_ (`withTarget` f) xs

-- | Forbid actions if the user is 'Silence'd.
unsilenced :: ∀ m. MonadPlay m => m () -> m ()
unsilenced f = do
    ctx <- context
    if Context.user ctx == Context.target ctx then
        f
    else
        whenM (Ninja.is Silence <$> nUser) f

-- | Applies a 'Ninja' transformation to the 'target'.
toTarget :: ∀ m. MonadPlay m => (Ninja -> Ninja) -> m ()
toTarget f = flip modify f =<< target

-- | Applies a 'Ninja' transformation to the 'target', passing it the 'user'
-- as an argument.
fromSource :: ∀ m. MonadPlay m => (Slot -> Ninja -> Ninja) -> m ()
fromSource f = do
    t   <- target
    src <- user
    modify t $ f src

zipWith :: ∀ m o. (MonadGame m, MonoFoldable o, Element o ~ Ninja)
        => (Ninja -> Ninja -> Ninja) -> o -> m ()
zipWith f = traverse_ (uncurry g) . zip Slot.all . toList
  where
    g i = modify i . f

-- | Adds a 'Flag' if 'Context.user' is not 'Context.target' and 'Context.new' is True.
trigger :: ∀ m o. (MonadPlay m, MonoFoldable o, Element o ~ Trigger)
        => Slot -> o -> m ()
trigger i xs = whenM (valid <$> context) $ modify i \n ->
    n { Ninja.triggers = foldl' (flip insertSet) (Ninja.triggers n) xs }
  where
    valid ctx = Context.new ctx && Context.user ctx /= Context.target ctx

yieldVictor :: ∀ m. MonadGame m => m ()
yieldVictor = whenM (null . Game.victor <$> game) do
    ns <- ninjas
    alter \g -> g { Game.victor = mVictor ns }
  where
    mVictor ns = filter (dead ns . Player.opponent) [minBound..maxBound]

-- | The entire team of a 'Player' is dead, resulting in defeat.
dead :: Vector Ninja -> Player -> Bool
dead ns p = not $ any (Ninja.playing p) ns

forfeit :: ∀ m. MonadGame m => Player -> m ()
forfeit p = whenM (null . Game.victor <$> game) do
    modifyAll suicide
    alter \g -> g { Game.victor = [Player.opponent p] }
  where
    suicide n
      | Parity.allied p $ Ninja.slot n = n { Ninja.health = 0 }
      | otherwise                      = n
