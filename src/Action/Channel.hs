-- | Actions that characters can use to affect 'Channel's.
module Action.Channel
  ( cancelChannel
  , prolongChannel
  , interrupt
  , onInterrupts
  ) where

import ClassyPrelude

import           Core.Util ((∈))
import qualified Class.Play as P
import           Class.Play (MonadPlay)
import           Class.Random (MonadRandom)
import qualified Model.Channel as Channel
import           Model.Channel (Channel)
import           Model.Class (Class(..))
import qualified Model.Context as Context
import           Model.Duration (Duration(..), Turns)
import           Model.Effect (Effect(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja, is)
import qualified Model.Skill as Skill
import           Model.Skill (Target(..))
import qualified Engine.Effects as Effects
import qualified Engine.Execute as Execute
import           Engine.Execute (Affected(..))

-- | Cancels 'Ninja.channels' with a matching 'Channel.name'.
-- Uses 'Ninja.cancelChannel' internally.
cancelChannel :: ∀ m. MonadPlay m => Text -> m ()
cancelChannel = P.toTarget . Ninja.cancelChannel

-- | Interrupts all 'Channel.interruptible' 'Ninja.channels'.
-- Triggers 'onInterrupts' for affected 'Channel's.
interrupt :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
interrupt = P.unsilenced do
    target  <- P.target
    onInterrupts Channel.interruptible
    P.modify target cancelChannels
  where
    keep             = not . Channel.interruptible
    cancelChannels n = n { Ninja.channels = filter keep $ Ninja.channels n }

getInterrupts :: (Channel -> Bool) -> Ninja -> [Channel]
getInterrupts interrupting n
  | n `is` Enrage || Stun All ∈ Effects.ignore n = []
  | otherwise = filter interrupting $ Ninja.channels n

-- | Triggers 'Skill.interrupt' effects of all 'Ninja.channels' that match a
-- predicate.
onInterrupts :: ∀ m. (MonadPlay m, MonadRandom m) => (Channel -> Bool) -> m ()
onInterrupts match = traverse_ onInterrupt . getInterrupts match =<< P.nTarget

-- | Triggers 'Skill.interrupt' effects of a 'Channel'.
onInterrupt :: ∀ m. (MonadPlay m, MonadRandom m) => Channel -> m ()
onInterrupt chan = P.with chanContext $
        traverse_ (Execute.effect $ setFromList [Channeled, Interrupted]) disr
  where
    name = Skill.name $ Channel.skill chan
    disr = (Self,  P.toTarget $ Ninja.clearVariants name)
         : (Enemy, P.fromSource $ Ninja.clear name)
         : (second P.play <$> Skill.interrupt (Channel.skill chan))
    chanContext ctx = Context.Context { Context.skill  = Channel.skill chan
                                      , Context.user   = Context.target ctx
                                      , Context.target = Channel.target chan
                                      , Context.new    = False
                                      }

-- | Increases the duration of 'Ninja.channels' with a matching 'Channel.name'.
prolongChannel :: ∀ m. MonadPlay m => Turns -> Text -> m ()
prolongChannel (Duration -> dur) = P.toTarget . Ninja.prolongChannel dur
