module Class.Play
  ( GameT(..), PlayT(..)
  , Play(..), PlayConstraint, play, launch
  , skill
  , source, user, target
  , nSource, nUser, nTarget
  , player
  , withContext
  , withSkill, withTarget, withTargets
  , toTarget, fromSource
  ) where

import ClassyPrelude.Yesod hiding (Vector)

import           Class.Random (RandomT)
import           Model.Internal (GameT(..), Play(..), PlayConstraint, PlayT(..))
import qualified Model.Context as Context
import           Model.Context (Context)
import qualified Model.Game as Game
import           Model.Ninja (Ninja)
import           Model.Player (Player)
import           Model.Skill (Skill)
import           Model.Slot (Slot)

play :: ∀ a. Play a -> PlayConstraint a
play (Play a) = a

withContext :: ∀ m a. Context -> ReaderT Context m a -> m a
withContext ctx f = runReaderT f ctx

launch :: ∀ m. (GameT m, RandomT m) => (Context, Play ()) -> m ()
launch (ctx, Play a) = runReaderT a ctx

player :: ∀ m. GameT m => m Player
player = Game.playing <$> game

skill :: ∀ m. PlayT m => m Skill
skill = Context.skill <$> context

source :: ∀ m. PlayT m => m Slot
source = Context.source <$> context

user :: ∀ m. PlayT m => m Slot
user = Context.user <$> context

target :: ∀ m. PlayT m => m Slot
target = Context.target <$> context

nSource :: ∀ m. PlayT m => m Ninja
nSource = Game.ninja <$> source <*> game

nUser :: ∀ m. PlayT m => m Ninja
nUser = Game.ninja <$> user <*> game

nTarget :: ∀ m. PlayT m => m Ninja
nTarget = Game.ninja <$> target <*> game


withSkill :: ∀ m a. PlayT m => Skill -> m a -> m a
withSkill x = with \ctx -> ctx { Context.skill = x }

withTarget :: ∀ m a. PlayT m => Slot -> m a -> m a
withTarget x = with \ctx -> ctx { Context.target = x }

withTargets :: ∀ m. PlayT m => [Slot] -> m () -> m ()
withTargets xs f = ofold <$> traverse (`withTarget` f) xs

toTarget :: ∀ m. PlayT m => (Ninja -> Ninja) -> m ()
toTarget f = target >>= modify . flip Game.adjust f

fromSource :: ∀ m. PlayT m
           => (Slot -> Ninja -> Ninja) -> m ()
fromSource f = do
    t   <- target
    src <- source
    modify . Game.adjust t $ f src
