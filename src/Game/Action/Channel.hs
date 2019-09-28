-- | Actions that characters can use to affect 'Channel's.
module Game.Action.Channel
  ( cancelChannel
  , prolongChannel
  , interrupt
  ) where

import ClassyPrelude

import qualified Class.Play as P
import           Class.Play (MonadPlay)
import           Class.Random (MonadRandom)
import qualified Game.Model.Channel as Channel
import           Game.Model.Channel (Channel)
import qualified Game.Model.Context as Context
import           Game.Model.Context (Context(Context))
import           Game.Model.Duration (Duration(..), Turns)
import qualified Game.Model.Ninja as Ninja
import qualified Game.Engine.Execute as Execute
import           Game.Engine.Execute (Affected(..))
import qualified Game.Engine.Ninjas as Ninjas

-- | Cancels 'Ninja.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.cancelChannel' internally.
cancelChannel :: ∀ m. MonadPlay m => Text -> m ()
cancelChannel name = do
    user <- P.user
    P.modify user $ Ninjas.cancelChannel name

interrupt :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
interrupt = P.unsilenced do
    (yay, nay) <- partition Channel.interruptible . Ninja.channels <$> P.nTarget
    traverse_ onInterrupt yay
    target <- P.target
    P.modify target \n -> n { Ninja.channels = nay }

-- | Triggers 'Skill.interrupt' effects of a @Channel@.
onInterrupt :: ∀ m. (MonadPlay m, MonadRandom m) => Channel -> m ()
onInterrupt chan = P.with chanContext $
    Execute.effects (setFromList [Channeled, Interrupted]) =<<
    Execute.chooseTargets (Execute.interruptions $ Channel.skill chan)
  where
    chanContext ctx = Context { Context.skill  = Channel.skill chan
                              , Context.user   = Context.target ctx
                              , Context.target = Channel.target chan
                              , Context.new    = False
                              }

-- | Increases the duration of 'Ninja.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.prolongChannel' internally.
prolongChannel :: ∀ m. MonadPlay m => Turns -> Text -> m ()
prolongChannel (Duration -> dur) name =
    P.toTarget $ Ninjas.prolongChannel dur name
