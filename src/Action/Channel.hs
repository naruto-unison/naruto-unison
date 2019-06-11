module Action.Channel
  ( doDisrupts
  , prolongChannel
  , cancelChannel
  , interrupt
  ) where

import ClassyPrelude.Yesod hiding ((\\))

import           Core.Util ((∈), sync)
import qualified Class.Play as P
import           Class.Play (PlayT)
import           Class.Random (RandomT)
import qualified Class.TurnBased as TurnBased
import qualified Model.Channel as Channel
import           Model.Channel (Channel)
import           Model.Class (Class(..))
import qualified Model.Context as Context
import qualified Model.Copy as Copy
import           Model.Effect (Effect(..))
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Model.Skill as Skill
import           Model.Skill (Target(..))
import qualified Engine.Effects as Effects
import qualified Engine.Execute as Execute
import           Engine.Execute (Affected(..))

-- | 'N.cancelChannel'
cancelChannel :: ∀ m. PlayT m => Text -> m ()
cancelChannel = P.toTarget . Ninja.cancelChannel

interrupt :: ∀ m. (PlayT m, RandomT m) => m ()
interrupt = unlessM (Ninja.is Enrage <$> P.nTarget) do
    target  <- P.target
    doDisrupts Channel.disrupts
    P.modify $ Game.adjust target cancelChannels
  where
    cancelChannels n = n
        { Ninja.channels = filter (not . Channel.disrupts) $ Ninja.channels n }

doDisrupts :: ∀ m. (PlayT m, RandomT m) => (Channel -> Bool) -> m ()
doDisrupts disrupt = do
    nTarget <- P.nTarget
    unless (Ninja.is Enrage nTarget || Stun All ∈ Effects.ignore nTarget) .
        traverse_ doDisrupt . filter disrupt $ Ninja.channels nTarget

doDisrupt :: ∀ m. (PlayT m, RandomT m) => Channel -> m ()
doDisrupt chan = do
    target <- P.target
    let chanContext ctx = ctx { Context.skill  = Channel.skill chan
                              , Context.source = target
                              , Context.user   = target
                              , Context.target = Channel.target chan
                              }
    P.with chanContext $
        traverse_ (Execute.effect [Channeled, Disrupted]) disr
  where
    name = Skill.name $ Channel.skill chan
    disr = (Self,  P.toTarget $ Ninja.clearVariants name)
         : (Enemy, P.fromSource $ Ninja.clear name)
         : (second P.play <$> Skill.disrupt (Channel.skill chan))

prolongChannel :: ∀ m. PlayT m => Int -> Text -> m ()
prolongChannel dur name = P.toTarget \n ->
    n { Ninja.channels = f <$> Ninja.channels n }
  where
    f chan
      | TurnBased.getDur chan <= 0 = chan
      | Skill.name (Channel.skill chan) /= name = chan
      | otherwise = flip TurnBased.setDur chan .
                    Copy.maxDur (Skill.copying $ Channel.skill chan) $
                    TurnBased.getDur chan + sync dur
