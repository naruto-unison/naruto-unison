-- | Actions that characters can use to affect 'Channel's.
module Game.Action.Channel
  ( cancelChannel, cancelChannel'
  , prolongChannel
  , interrupt
  , renameChannels
  ) where

import ClassyPrelude
import qualified Class.Labeled as Labeled
import           Class.Play (MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Game.Action as Action
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Model.Channel as Channel
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import           Game.Model.Duration (Duration)
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import qualified Game.Model.Skill as Skill

-- | Cancels 'N.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.cancelChannel' internally.
cancelChannel :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
cancelChannel = cancelChannel' ""

-- | Cancels 'N.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.cancelChannel' internally.
cancelChannel' :: ∀ m. (MonadPlay m, MonadRandom m) => Text -> m ()
cancelChannel' name = do
    Context{user, skill} <- P.context
    let name' = Skill.defaultName name skill
    (yays, nays) <- getCancelledChannels name' <$> P.nUser
    P.modify user \n -> n { N.channels = nays }
    mapM_ (Action.runInterruptions user) yays
  where
    getCancelledChannels name' Ninja{channels} =
        partition ((== name') . Labeled.name) channels

-- | Prematurely ends a channeled action.
interrupt :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
interrupt = P.unsilenced do
    Ninja{channels, slot} <- P.nTarget
    let (yay, nay) = partition Channel.interruptible channels
    P.modify slot \n -> n { N.channels = nay }
    mapM_ (Action.runInterruptions slot) yay

-- | Increases the duration of 'N.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.prolongChannel' internally.
prolongChannel :: ∀ m. MonadPlay m => Duration -> Text -> m ()
prolongChannel dur name = P.toTarget $ Ninjas.prolongChannel dur name

-- | Modify all channel names.
renameChannels :: ∀ m. MonadPlay m => (Text -> Text) -> m ()
renameChannels rename = P.toTarget $ Ninjas.renameChannels rename
