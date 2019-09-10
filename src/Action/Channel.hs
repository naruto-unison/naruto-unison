-- | Actions that characters can use to affect 'Channel's.
module Action.Channel
  ( cancelChannel
  , prolongChannel
  , interrupt
  ) where

import ClassyPrelude

import qualified Class.Play as P
import           Class.Play (MonadPlay)
import           Class.Random (MonadRandom)
import qualified Model.Channel as Channel
import           Model.Channel (Channel)
import qualified Model.Context as Context
import           Model.Context (Context(Context))
import           Model.Duration (Duration(..), Turns)
import qualified Model.Ninja as Ninja
import qualified Engine.Execute as Execute
import           Engine.Execute (Affected(..))
import qualified Engine.Ninjas as Ninjas

-- | Cancels 'Ninja.channels' with a matching 'Channel.name'.
-- Uses 'Ninjas.cancelChannel' internally.
cancelChannel :: ∀ m. MonadPlay m => Text -> m ()
cancelChannel name = do
    user <- P.user
    P.modify user $ Ninjas.cancelChannel name

-- | Interrupts all 'Channel.interruptible' 'Ninja.channels'.
-- Triggers 'onInterrupts' for affected @Channel@s.
interrupt :: ∀ m. (MonadPlay m, MonadRandom m) => (Channel -> Bool) -> m ()
interrupt interrupting = P.unsilenced do
    (interrupted, kept) <- partition interrupts . Ninja.channels <$> P.nTarget
    traverse_ onInterrupt interrupted
    target <- P.target
    P.modify target \n -> n { Ninja.channels = kept }
  where
    interrupts x = Channel.interruptible x && interrupting x

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
