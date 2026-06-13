-- | Actions that characters can use to affect 'Channel's.
module Game.Action.Channel
  ( cancelChannel, cancelChannel'
  , replaceChannel
  , prolongChannel, prolongChannel'
  , interrupt
  , renameChannels
  ) where

import ClassyPrelude
import           Class.Play (MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Game.Action as Action
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Channel (Channel)
import qualified Game.Model.Channel as Channel
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import           Game.Model.Duration (Duration)
import           Game.Model.ID (ID(ID))
import qualified Game.Model.ID as ID
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import           Game.Model.Slot (Slot)

replaceChannel :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
replaceChannel = do
    channelID@ID{user} <- ID.from <$> P.context
    P.modify user $ Ninjas.cancelOldChannel channelID

-- | Cancels 'N.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.cancelChannel' internally.
cancelChannel :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
cancelChannel = cancelChannel' ""

takeChannels :: ∀ m. (MonadPlay m) => Slot -> (Channel -> Bool) -> m [Channel]
takeChannels slot f = do
    Ninja{channels} <- P.ninja slot
    let (yays, nays) = partition f channels
    P.modify slot \n -> n { N.channels = nays }
    return yays

-- | Cancels 'N.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.cancelChannel' internally.
cancelChannel' :: ∀ m. (MonadPlay m, MonadRandom m) => Text -> m ()
cancelChannel' name = P.uncopied do
    channelID@ID{user} <- P.createID name
    cancelled <- takeChannels user $ (== ID.fromOwner channelID) . ID.from
    mapM_ (Action.runInterruptions user) cancelled

-- | Prematurely ends a channeled action.
interrupt :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
interrupt = P.unsilenced do
    Context{target} <- P.context
    cancelled <- takeChannels target Channel.interruptible
    mapM_ (Action.runInterruptions target) cancelled

-- | Increases the duration of 'N.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.prolongChannel' internally.
prolongChannel' :: ∀ m. MonadPlay m => Text -> Duration -> m ()
prolongChannel' name dur = P.uncopied do
    channelID@ID{user} <- P.createID name
    P.modify user $ Ninjas.prolongChannel dur channelID

-- | Increases the duration of the current channeled skill.
prolongChannel :: ∀ m. MonadPlay m => Duration -> m ()
prolongChannel = prolongChannel' ""

-- | Modify all channel names.
renameChannels :: ∀ m. MonadPlay m => (Text -> Text) -> m ()
renameChannels rename = P.uncopied . P.toUser $ Ninjas.renameChannels rename
