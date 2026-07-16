-- | Monadic constraints for manipulating game state.
module Class.Play
  ( -- * Monads
    MonadGame(..), MonadPlay(..)
    -- * Context
    -- ** From game
  , nUser, nTarget
  , allies
  , enemies
  , numAlive
  -- * Transformation
  , withContext
  , withTarget, withTargets
  , withContinues
  , uncopied, unsilenced
  -- * Lifting
  , toTarget, toTargetFromUser, toUser, toUserFromUser
  -- * Other
  , createID
  , trigger
  ) where

import ClassyPrelude hiding (zipWith)

import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Effect (Effect(..))
import           Game.Model.ID (ID)
import qualified Game.Model.ID as ID
import           Game.Model.Internal (MonadGame(..), MonadPlay(..))
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as N
import qualified Game.Model.Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Game.Model.Trigger (Trigger(..))
import qualified Game.Model.Trigger as Trigger

-- | Alters the focus of the environment to a new @Context@.
withContext :: ∀ a m. Context -> ReaderT Context m a -> m a
withContext ctx f = runReaderT f ctx

-- | The 'Game.ninja' indexed by 'user'.
nUser :: ∀ m. MonadPlay m => m Ninja
nUser = ninja =<< Context.user <$> context

-- | The 'Game.ninja' indexed by 'target'.
nTarget :: ∀ m. MonadPlay m => m Ninja
nTarget = ninja =<< Context.target <$> context

-- | Returns the half of 'ninjas' allied with an argument.
allies :: ∀ p m. (MonadGame m, Parity p) => p -> m (Vector Ninja)
allies p = splitHalf Slot.teamSize <$> ninjas
  where
    splitHalf
      | Parity.even p = take
      | otherwise     = drop

-- | Returns the half of 'ninjas' not allied with an argument.
enemies :: ∀ p m. (MonadGame m, Parity p) => p -> m (Vector Ninja)
enemies p = allies $ Parity.opponent p

-- | Returns the half of 'ninjas' allied with an argument that are alive.
numAlive :: ∀ p m. (MonadGame m, Parity p) => p -> m Int
numAlive p = foldM go 0 $ Slot.allies p
  where
    go :: Int -> Slot -> m Int
    go acc slot = do
        n <- ninja slot
        return if N.alive n then
            acc + 1
        else
            acc

-- | Runs an action in a localized state where 'target' is replaced.
withTarget :: ∀ a m. MonadPlay m => Slot -> m a -> m a
withTarget x = with \ctx -> ctx { Context.target = x }

-- | Runs an action against each 'target'.
withTargets :: ∀ o m. (MonadPlay m, MonoFoldable o, Slot ~ Element o)
            => o -> m () -> m ()
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
    isUncopied Context{user, skill} = skill.owner == user

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
createID name
  | null name = ID.from <$> context
  | otherwise = ID.withName name . ID.from <$> context

-- | Applies a @Ninja@ transformation to the 'target', passing it the 'user' as
-- an argument.
toTargetFromUser :: ∀ m. MonadPlay m => (ID -> Ninja -> Ninja) -> Text -> m ()
toTargetFromUser f name = toTarget . f =<< createID name

-- | Applies a @Ninja@ transformation to the 'user', passing it the 'user' as
-- an argument.
toUserFromUser :: ∀ m. MonadPlay m => (ID -> Ninja -> Ninja) -> Text -> m ()
toUserFromUser f name = toUser . f =<< createID name

-- | Adds to 'N.triggers' if 'Context.user' is not 'Context.target' and
-- 'Context.new' is @True@.
trigger :: ∀ m. MonadPlay m => Slot -> [Trigger] -> m ()
trigger _ [] = return ()
trigger i xs = do
    Context{new} <- context
    if new then
        modify i \n -> addNegatives
            n { N.triggers = foldl' (flip insertSet) n.triggers xs }
    else if not $ null negatives then
        modify i addNegatives
    else
        return ()
  where
    negatives = setFromList $ mapMaybe Trigger.toNegative xs
    addNegatives n
      | null negatives = n
      | otherwise = n { N.negatives = negatives ++ n.negatives }
