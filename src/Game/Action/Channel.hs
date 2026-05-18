-- | Actions that characters can use to affect 'Channel's.
module Game.Action.Channel
  ( cancelChannel
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
import           Game.Model.Channel (Channel(Channel))
import qualified Game.Model.Channel as Channel
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import           Game.Model.Duration (Duration)
import qualified Game.Model.Ninja as N

-- | Cancels 'N.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.cancelChannel' internally.
cancelChannel :: ∀ m. MonadPlay m => Text -> m ()
cancelChannel name = do
    Context{user} <- P.context
    P.modify user $ Ninjas.cancelChannel name

-- | Prematurely ends a channeled action.
interrupt :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
interrupt = P.unsilenced do
    Context{target} <- P.context
    (yay, nay) <- partition Channel.interruptible . N.channels <$> P.nTarget
    traverse_ onInterrupt yay
    P.modify target \n -> n { N.channels = nay }

-- | Triggers 'Skill.interrupt' effects of a @Channel@.
onInterrupt :: ∀ m. (MonadPlay m, MonadRandom m) => Channel -> m ()
onInterrupt (Channel skill target _) = P.with ctx
    $ Action.run =<< Action.chooseTargets (Action.interruptions skill)
  where
    ctx Context{target = user} = Context
        { skill
        , user
        , target
        , new       = False
        , continues = False
        }

-- | Increases the duration of 'N.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.prolongChannel' internally.
prolongChannel :: ∀ m. MonadPlay m => Duration -> Text -> m ()
prolongChannel dur name = P.toTarget $ Ninjas.prolongChannel dur name

-- | Modify all channel names.
renameChannels :: ∀ m. MonadPlay m => (Text -> Text) -> m ()
renameChannels rename = P.toTarget $ Ninjas.renameChannels rename
