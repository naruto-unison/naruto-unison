-- | Actions that characters can use to affect @Trap@s.
module Game.Action.Trap
  ( trap, trap', trapFrom, trapFrom', trapPer, trapPer', trapWith
  , onBreak, onBreak'
  , removeTrap
  , delay
  ) where
import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Enum.Set (EnumSet)

import qualified Class.Classed as Classed
import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Action as Action
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import qualified Game.Model.Delay as Delay
import           Game.Model.Duration (Duration(..))
import qualified Game.Model.Duration as Duration
import           Game.Model.Effect (Constructor(..), Effect(..))
import           Game.Model.Ninja (is)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Runnable (Runnable(..), RunConstraint)
import qualified Game.Model.Skill as Skill
import           Game.Model.Trap (Trap(Trap))
import qualified Game.Model.Trap as Trap
import           Game.Model.Trigger (Trigger(..))
import qualified Game.Model.Trigger as Trigger
import           Util ((∉))

-- | Adds a @Trap@ to 'Ninja.traps' that targets the person it was used on.
trap :: ∀ m. MonadPlay m => Duration -> Trigger -> RunConstraint () -> m ()
trap = trapConst Trap.Toward mempty
-- | 'Hidden' 'trap'.
trap' :: ∀ m. MonadPlay m => Duration -> Trigger -> RunConstraint () -> m ()
trap' = trapConst Trap.Toward $ setFromList [Bypassing, Hidden]

-- | Adds a @Trap@ to 'Ninja.traps' that targets the person who triggers it.
trapFrom :: ∀ m. MonadPlay m => Duration -> Trigger -> RunConstraint () -> m ()
trapFrom = trapConst Trap.From mempty
-- | 'Hidden' 'trapFrom'.
trapFrom' :: ∀ m. MonadPlay m => Duration -> Trigger -> RunConstraint () -> m ()
trapFrom' = trapConst Trap.From $ setFromList [Bypassing, Hidden]

-- | Adds a @Trap@ to 'Ninja.traps' with additional 'Class'es.
trapWith :: ∀ m. MonadPlay m
         => EnumSet Class -> Duration -> Trigger -> RunConstraint () -> m ()
trapWith = trapConst Trap.Toward

-- | Adds a @Trap@ to 'Ninja.traps' with an effect that depends on a number
-- accumulated while the trap is in play and tracked with its 'Trap.tracker'.
trapPer  :: ∀ m. MonadPlay m
         => Duration -> Trigger -> (Int -> RunConstraint ()) -> m ()
trapPer  = trapFull Trap.Per mempty
-- | 'Hidden' 'trapPer'.
trapPer' :: ∀ m. MonadPlay m
         => Duration -> Trigger -> (Int -> RunConstraint ()) -> m ()
trapPer' = trapFull Trap.Per $ setFromList [Bypassing, Hidden]

-- | Adds an 'OnBreak' @Trap@ for the used 'Skill.Skill' to 'Ninja.traps'.
-- @OnBreak@ traps are triggered when a 'Defense.Defense' with the same
-- 'Defense.name' is broken.
onBreak :: ∀ m. MonadPlay m => RunConstraint () -> m ()
onBreak f = do
    name    <- Skill.name <$> P.skill
    user    <- P.user
    nTarget <- P.nTarget
    when (Ninja.hasDefense name user nTarget) $
        trapFrom' Permanent (OnBreak name) do
            f
            user' <- P.user
            P.modify user' . Ninjas.clearTraps $ OnBreak name

-- | Default 'onBreak': remove 'Model.Status.Status'es and
-- 'Model.Channel.Channel's that match 'Defense.name'. This is useful for
-- 'Defense.Defense's that apply an effect or empower some action while active.
onBreak' :: ∀ m. MonadPlay m => m ()
onBreak' = do
    user <- P.user
    name <- Skill.name <$> P.skill
    onBreak do
        P.modify user $ Ninjas.cancelChannel name
        P.modifyAll $ Ninjas.clear name user . Ninjas.clear (toLower name) user

-- | Adds a @Trap@ to 'Ninja.traps'.
trapConst :: ∀ m. MonadPlay m
         => Trap.Direction -> EnumSet Class -> Duration -> Trigger
         -> RunConstraint () -> m ()
trapConst trapType clas dur tr f = trapFull trapType clas dur tr $ const f

-- | Trap engine.
trapFull :: ∀ m. MonadPlay m
         => Trap.Direction -> EnumSet Class -> Duration -> Trigger
         -> (Int -> RunConstraint ()) -> m ()
trapFull direction classes unthrottled trigger f =
    void $ runMaybeT do
        context <- P.context
        target  <- P.target
        nUser   <- P.nUser
        nTarget <- P.nTarget
        dur     <- if not $ Context.new context then return unthrottled else
                   MaybeT . return $ throttle nUser
        let tr = makeTrap context direction classes dur trigger f
        guard $ tr ∉ Ninja.traps nTarget
        guard . not $ isCounter && nUser `is` Disable Counters
        P.modify target \n ->
            n { Ninja.traps = Classed.nonStack tr tr $ Ninja.traps n }
  where
    isCounter = Trigger.isCounter trigger
    throttle n
      | isCounter = Duration.throttle (Effects.throttleCounters n) unthrottled
      | otherwise = Just unthrottled

makeTrap :: Context -> Trap.Direction -> EnumSet Class -> Duration
         -> Trigger -> (Int -> RunConstraint ()) -> Trap
makeTrap Context{skill, user, target, continues, new}
         direction classes dur trigger f =
    Trap
    { trigger
    , direction
    , skill   = skill'
    , user
    , name    = Skill.name skill
    , effect  = \i -> To { target = context, run = Action.wrap $ f i }
    , classes = classes'
    , tracker = 0
    , dur     = succ dur
    }
  where
    modClasses
      | continues && dur <= 1 = insertSet Continues
      | continues || new      = deleteSet Continues
      | otherwise             = deleteSet Continues . deleteSet Invisible
    classes' = modClasses $ classes ++ Skill.classes skill
    skill'   = skill { Skill.classes = classes' }
    context  = Context { user
                       , target
                       , skill     = skill'
                       , continues = False
                       , new       = False
                       }

-- | Saves an effect to a 'Delay.Delay', which is stored in 'Game.delays' and
-- triggered when it expires.
delay :: ∀ m. MonadPlay m => Duration -> RunConstraint () -> m ()
delay Permanent _ = return () -- A Delay that lasts forever would be pointless!
delay dur f = do
    context  <- P.context
    let user  = Context.user context
        del   = Delay.new context { Context.continues = False } dur $
                Action.wrap f
    P.modify user \n -> n { Ninja.delays = del : Ninja.delays n }

-- | Removes 'Ninja.traps' with matching 'Trap.name'.
-- Uses 'Ninjas.clearTrap' internally.
removeTrap :: ∀ m. MonadPlay m => Text -> m ()
removeTrap name = P.fromUser $ Ninjas.clearTrap name
