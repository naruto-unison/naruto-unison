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
import           Game.Model.Channel (Channel)
import qualified Game.Model.Channel as Channel
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Duration (Duration)
import qualified Game.Model.Ninja as Ninja

-- | Cancels 'Ninja.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.cancelChannel' internally.
cancelChannel :: ∀ m. MonadPlay m => Text -> m ()
cancelChannel name = do
    user <- P.user
    P.modify user $ Ninjas.cancelChannel name

-- | Prematurely ends a channeled action.
interrupt :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
interrupt = P.unsilenced do
    (yay, nay) <- partition Channel.interruptible . Ninja.channels <$> P.nTarget
    traverse_ onInterrupt yay
    target <- P.target
    P.modify target \n -> n { Ninja.channels = nay }

-- | Triggers 'Skill.interrupt' effects of a @Channel@.
onInterrupt :: ∀ m. (MonadPlay m, MonadRandom m) => Channel -> m ()
onInterrupt chan =
    P.with ctx $ Action.run
    =<< Action.chooseTargets (Action.interruptions $ Channel.skill chan)
  where
    ctx context = Context { skill     = Channel.skill chan
                          , user      = Context.target context
                          , target    = Channel.target chan
                          , new       = False
                          , continues = False
                          }

-- | Increases the duration of 'Ninja.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.prolongChannel' internally.
prolongChannel :: ∀ m. MonadPlay m => Duration -> Text -> m ()
prolongChannel dur name =
    P.toTarget $ Ninjas.prolongChannel dur name

-- | Modify all channel names.
renameChannels :: ∀ m. MonadPlay m => (Text -> Text) -> m ()
renameChannels rename = P.toTarget $ Ninjas.renameChannels rename
