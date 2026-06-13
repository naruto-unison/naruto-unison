-- | Actions that characters can use to affect 'Channel's.
module Game.Action.Channel
  ( cancelChannel, cancelChannel'
  , replaceChannel
  , prolongChannel
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
import qualified Game.Model.ID
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)

replaceChannel :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
replaceChannel = do
    Context{user, skill = Skill{name, owner}} <- P.context
    P.modify user $ Ninjas.cancelOldChannel ID { user = owner, owner, name }

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
    Context{user, skill} <- P.context
    let name' = Skill.defaultName name skill
    cancelled <- takeChannels user $ (== name') . Channel.name
    mapM_ (Action.runInterruptions user) cancelled

-- | Prematurely ends a channeled action.
interrupt :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
interrupt = P.unsilenced do
    Context{target} <- P.context
    cancelled <- takeChannels target Channel.interruptible
    mapM_ (Action.runInterruptions target) cancelled

-- | Increases the duration of 'N.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.prolongChannel' internally.
prolongChannel :: ∀ m. MonadPlay m => Duration -> Text -> m ()
prolongChannel dur name = P.uncopied . P.toUser $ Ninjas.prolongChannel dur name

-- | Modify all channel names.
renameChannels :: ∀ m. MonadPlay m => (Text -> Text) -> m ()
renameChannels rename = P.uncopied . P.toUser $ Ninjas.renameChannels rename
