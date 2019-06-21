-- | Actions that characters can use to affect 'Channel's.
module Action.Channel
  ( cancelChannel
  , prolongChannel
  , interrupt
  , onInterrupts
  ) where

import ClassyPrelude hiding ((\\))

import           Core.Util ((∈))
import qualified Class.Play as P
import           Class.Play (MonadPlay)
import           Class.Random (MonadRandom)
import qualified Class.TurnBased as TurnBased
import qualified Model.Channel as Channel
import           Model.Channel (Channel)
import           Model.Class (Class(..))
import qualified Model.Context as Context
import qualified Model.Copy as Copy
import           Model.Duration (Duration(..), Turns, sync)
import           Model.Effect (Effect(..))
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
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
interrupt = unlessM (Ninja.is Enrage <$> P.nTarget) do
    target  <- P.target
    onInterrupts Channel.interruptible
    P.modify $ Game.adjust target cancelChannels
  where
    keep             = not . Channel.interruptible
    cancelChannels n = n { Ninja.channels = filter keep $ Ninja.channels n }

-- | Triggers 'Skill.interrupt' effects of all 'Ninja.channels' that match a
-- predicate.
onInterrupts :: ∀ m. (MonadPlay m, MonadRandom m) => (Channel -> Bool) -> m ()
onInterrupts interrupting = do
    nTarget <- P.nTarget
    unless (Ninja.is Enrage nTarget || Stun All ∈ Effects.ignore nTarget) .
        traverse_ onInterrupt . filter interrupting $ Ninja.channels nTarget

-- | Triggers 'Skill.interrupt' effects of a 'Channel'.
onInterrupt :: ∀ m. (MonadPlay m, MonadRandom m) => Channel -> m ()
onInterrupt chan = do
    target <- P.target
    let ctx = Context.Context { Context.skill  = Channel.skill chan
                              , Context.user   = target
                              , Context.target = Channel.target chan
                              }
    P.with (const ctx) $
        traverse_ (Execute.effect [Channeled, Interrupted]) disr
  where
    name = Skill.name $ Channel.skill chan
    disr = (Self,  P.toTarget $ Ninja.clearVariants name)
         : (Enemy, P.fromSource $ Ninja.clear name)
         : (second P.play <$> Skill.interrupt (Channel.skill chan))

-- | Increases the duration of 'Ninja.channels' with a matching 'Channel.name'.
prolongChannel :: ∀ m. MonadPlay m => Turns -> Text -> m ()
prolongChannel (Duration -> dur) name = P.toTarget \n ->
    n { Ninja.channels = f <$> Ninja.channels n }
  where
    f chan
      | TurnBased.getDur chan <= 0              = chan
      | Skill.name (Channel.skill chan) /= name = chan
      | otherwise = flip TurnBased.setDur chan .
                    Copy.maxDur (Skill.copying $ Channel.skill chan) $
                    TurnBased.getDur chan + sync dur
