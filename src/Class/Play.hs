-- | Monadic constraints for manipulating game state.
module Class.Play
  ( -- * Monads
    MonadGame(..), MonadPlay(..)
    -- * Actions stored in data structures
  , launch
    -- * Context
    -- ** From game
  , nUser, nTarget
  , allies
  , enemies
  -- * Transformation
  , withContext
  , withTarget, withTargets
  , withContinues
  , uncopied, unsilenced
  -- * Lifting
  , toTarget, toUser, fromUser
  -- * Other
  , createID
  , trigger
  ) where

import ClassyPrelude hiding (zipWith)

import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Class.Random (MonadRandom)
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Effect (Effect(..))
import           Game.Model.ID (ID(ID))
import qualified Game.Model.ID
import           Game.Model.Internal (MonadGame(..), MonadPlay(..))
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as N
import           Game.Model.Runnable (Runnable(To))
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill
import           Game.Model.Slot (Slot)
import           Game.Model.Trigger (Trigger(..))

-- | Alters the focus of the environment to a new @Context@.
withContext :: ∀ m a. Context -> ReaderT Context m a -> m a
withContext ctx f = runReaderT f ctx

-- | Runs a @Runnable@ with its associated @Context@.
launch :: ∀ m. (MonadGame m, MonadRandom m) => Runnable Context -> m ()
launch (To runTarget run) = runReaderT run runTarget

-- | The 'Game.ninja' indexed by 'user'.
nUser :: ∀ m. MonadPlay m => m Ninja
nUser = ninja =<< Context.user <$> context

-- | The 'Game.ninja' indexed by 'target'.
nTarget :: ∀ m. MonadPlay m => m Ninja
nTarget = ninja =<< Context.target <$> context

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
withTargets xs f = mapM_ (`withTarget` f) xs

-- | Sets 'Context.continues' to @True@.
withContinues :: ∀ m. MonadPlay m => m () -> m ()
withContinues = with \ctx -> ctx { Context.continues = True }

-- | Forbid actions if the user is 'Silence'd.
unsilenced :: ∀ m. MonadPlay m => m () -> m ()
unsilenced = whenM (isUnsilenced =<< context)
  where
    isUnsilenced Context{target, user}
        | target == user = return True
        | otherwise      = not . (`is` Silence) <$> nUser

-- | Performs an action only if the skill being used is not copied from
-- someone else.
uncopied :: ∀ m. MonadPlay m => m () -> m ()
uncopied = whenM (isUncopied <$> context)
  where
    isUncopied Context{user, skill = Skill{owner}} = owner == user

-- | Applies a @Ninja@ transformation to the 'target'.
toTarget :: ∀ m. MonadPlay m => (Ninja -> Ninja) -> m ()
toTarget f = do
    Context{target} <- context
    modify target f

-- | Applies a @Ninja@ transformation to the 'user'.
toUser :: ∀ m. MonadPlay m => (Ninja -> Ninja) -> m ()
toUser f = do
    Context{user} <- context
    modify user f

createID :: ∀ m. MonadPlay m => Text -> m ID
createID name = create <$> context
  where
    create Context{user, skill = Skill{owner, name = skillName}} = ID
        { user
        , owner
        , name = if null name then skillName else name
        }

-- | Applies a @Ninja@ transformation to the 'target', passing it the 'user' as
-- an argument.
fromUser :: ∀ m. MonadPlay m => (ID -> Ninja -> Ninja) -> Text -> m ()
fromUser f name = toTarget . f =<< createID name

-- | Adds to 'N.triggers' if 'Context.user' is not 'Context.target' and
-- 'Context.new' is @True@.
trigger :: ∀ m. MonadPlay m => Slot -> [Trigger] -> m ()
trigger i xs = whenM (Context.new <$> context)
    $ modify i \n ->
        n { N.triggers = foldl' (flip insertSet) (N.triggers n) xs }
