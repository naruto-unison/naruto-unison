-- | Actions that characters can use to affect 'Channel's.
module Action.Channel
  ( cancelChannel
  , prolongChannel
  , interrupt
  , breakControls
  ) where

import ClassyPrelude

import qualified Class.Play as P
import           Class.Play (MonadGame, MonadPlay)
import           Class.Random (MonadRandom)
import qualified Model.Channel as Channel
import           Model.Channel (Channel)
import qualified Model.Context as Context
import           Model.Context (Context(Context))
import           Model.Duration (Duration(..), Turns)
import qualified Model.Ninja as Ninja
import qualified Model.Runnable as Runnable
import           Model.Runnable (Runnable(To))
import qualified Model.Skill as Skill
import           Model.Skill (Skill, Target(..))
import           Model.Slot (Slot)
import qualified Engine.Execute as Execute
import           Engine.Execute (Affected(..))

-- | Cancels 'Ninja.channels' with a matching 'Channel.name'.
-- Uses 'Ninja.cancelChannel' internally.
cancelChannel :: ∀ m. MonadPlay m => Text -> m ()
cancelChannel name = do
    user <- P.user
    P.modify user $ Ninja.cancelChannel name

-- | Interrupts all 'Channel.interruptible' 'Ninja.channels'.
-- Triggers 'onInterrupts' for affected @Channel@s.
interrupt :: ∀ m. (MonadPlay m, MonadRandom m) => (Channel -> Bool) -> m ()
interrupt interrupting = P.unsilenced do
    target <- P.target
    (interrupted, kept) <- partition interrupts . Ninja.channels <$> P.nTarget
    traverse_ onInterrupt interrupted
    P.modify target \n -> n { Ninja.channels = kept }
  where
    interrupts x = Channel.interruptible x && interrupting x

interruptions :: Skill -> [Runnable Target]
interruptions skill = (To Enemy clear) : (To Ally clear) : Skill.interrupt skill
  where
    clear :: ∀ m. MonadPlay m => m ()
    clear = P.fromSource . Ninja.clear $ Skill.name skill

-- | Triggers 'Skill.interrupt' effects of a @Channel@.
onInterrupt :: ∀ m. (MonadPlay m, MonadRandom m) => Channel -> m ()
onInterrupt chan = P.with chanContext $
        Execute.effects (setFromList [Channeled, Interrupted]) =<<
        Execute.chooseTargets (interruptions $ Channel.skill chan)
  where
    chanContext ctx = Context { Context.skill  = Channel.skill chan
                              , Context.user   = Context.target ctx
                              , Context.target = Channel.target chan
                              , Context.new    = False
                              }

nonRandom :: Target -> Bool
nonRandom RAlly  = False
nonRandom REnemy = False
nonRandom _      = True

breakControl :: ∀ m. (MonadGame m, MonadRandom m) => Slot -> Channel -> m ()
breakControl user chan = P.withContext chanContext do
    targets <- Execute.chooseTargets . filter (nonRandom . Runnable.target) $
               Skill.effects skill
    when (any null targets) do
        interruptTargets <- Execute.chooseTargets $ interruptions skill
        Execute.effects (setFromList [Channeled, Interrupted]) interruptTargets
        cancelChannel $ Skill.name skill
  where
    skill       = Channel.skill chan
    chanContext = Context { Context.skill  = skill
                          , Context.user   = user
                          , Context.target = Channel.target chan
                          , Context.new    = False
                          }

-- | Ends all Control channels without valid targets.
-- For example, if a Control skill targets an enemy, the channel will end
-- if the target becomes invulnerable or dies.
breakControls :: ∀ m. (MonadGame m, MonadRandom m) => m ()
breakControls = traverse_ breakN =<< P.ninjas
  where
    breakN n = traverse_ (breakControl $ Ninja.slot n) $ Ninja.channels n

-- | Increases the duration of 'Ninja.channels' with a matching 'Channel.name'.
prolongChannel :: ∀ m. MonadPlay m => Turns -> Text -> m ()
prolongChannel (Duration -> dur) = P.toTarget . Ninja.prolongChannel dur
