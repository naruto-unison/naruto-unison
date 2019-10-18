-- | Monadic constraints for manipulating game state.
module Class.Play
  ( -- * Monads
    MonadGame(..), MonadPlay(..)
    -- * Actions stored in data structures
  , launch
    -- * Context
    -- ** From monad
  , skill
  , user, target
  , new
    -- ** From game
  , nUser, nTarget
  , player
  , allies
  , enemies
  -- * Transformation
  , withContext
  , withTarget, withTargets
  , uncopied, unsilenced
  -- * Lifting
  , toTarget, fromUser
  -- * Other
  , trigger
  , zipWith
  , yieldVictor, forfeit
  ) where

import ClassyPrelude hiding (zipWith)

import Control.Monad (zipWithM_)

import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Class.Random (MonadRandom)
import           Game.Model.Context (Context)
import qualified Game.Model.Context as Context
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Game as Game
import           Game.Model.Internal (MonadGame(..), MonadPlay(..))
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import           Game.Model.Runnable (Runnable)
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Game.Model.Trigger (Trigger(..))

-- | Alters the focus of the environment to a new @Context@.
withContext :: ∀ m a. Context -> ReaderT Context m a -> m a
withContext ctx f = runReaderT f ctx

-- | Runs a @Runnable@ with its associated @Context@.
launch :: ∀ m. (MonadGame m, MonadRandom m) => Runnable Context -> m ()
launch x = runReaderT (Runnable.run x) $ Runnable.target x

-- | @Skill@ used to perform an action.
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

-- | The @Player@ whose turn it is.
player :: ∀ m. MonadGame m => m Player
player = Game.playing <$> game

-- | Returns the half of 'ninjas' allied with an argument.
allies :: ∀ p m. (MonadGame m, Parity p) => p -> m [Ninja]
allies p = Parity.half p <$> ninjas

-- | Returns the half of 'ninjas' not allied with an argument.
enemies :: ∀ p m. (MonadGame m, Parity p) => p -> m [Ninja]
enemies p = allies . not $ Parity.even p

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
        unlessM ((`is` Silence) <$> nUser) f

-- | Performs an action only if the skill being used is not copied from
-- someone else.
uncopied :: ∀ m. MonadPlay m => m () -> m ()
uncopied f = do
    sk  <- skill
    usr <- user
    when (Skill.owner sk == usr) f

-- | Applies a @Ninja@ transformation to the 'target'.
toTarget :: ∀ m. MonadPlay m => (Ninja -> Ninja) -> m ()
toTarget f = flip modify f =<< target

-- | Applies a @Ninja@ transformation to the 'target', passing it the 'user' as
-- an argument.
fromUser :: ∀ m. MonadPlay m => (Slot -> Ninja -> Ninja) -> m ()
fromUser f = do
    t   <- target
    usr <- user
    modify t $ f usr

zipWith :: ∀ m. (MonadGame m)
        => (Ninja -> Ninja -> Ninja) -> [Ninja] -> m ()
zipWith f = zipWithM_ (\i -> modify i . f) Slot.all

-- | Adds to 'Ninja.triggers' if 'Context.user' is not 'Context.target' and
-- 'Context.new' is @True@.
trigger :: ∀ m. MonadPlay m => Slot -> [Trigger] -> m ()
trigger i xs = whenM new $ modify i \n ->
    n { Ninja.triggers = foldl' (flip insertSet) (Ninja.triggers n) xs }

-- | Updates 'Game.victor'.
yieldVictor :: ∀ m. MonadGame m => m ()
yieldVictor = whenM (null . Game.victor <$> game) do
    ns <- ninjas
    let splitNs = splitAt (length ns `quot` 2) ns
    alter \g -> g { Game.victor = filter (victor splitNs) [Player.A, Player.B] }
  where
    victor (_, ns) Player.A = not $ any Ninja.alive ns
    victor (ns, _) Player.B = not $ any Ninja.alive ns

forfeit :: ∀ m. MonadGame m => Player -> m ()
forfeit p = whenM (null . Game.victor <$> game) do
    modifyAll suicide
    alter \g -> g { Game.victor = [Player.opponent p] }
  where
    suicide n
      | Parity.allied p n = n { Ninja.health = 0 }
      | otherwise         = n
