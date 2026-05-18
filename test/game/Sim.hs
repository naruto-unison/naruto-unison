module Sim
  ( targetSlot
  , describeCategory
  , act
  , enemies
  , turns
  , as, at, use
  , targetIsExposed
  , statusDur
  , withClass, withClasses
  , targets
  , simOf, simAt
  ) where

import ClassyPrelude

import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Enum.Set (EnumSet)
import Test.Hspec hiding (context)

import           Class.Hook (MonadHook)
import qualified Class.Parity as Parity
import           Class.Play (MonadGame, MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Game.Action as Action
import           Game.Action.Status (apply)
import qualified Game.Characters as Characters
import qualified Game.Engine as Engine
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Skills as Skills
import           Game.Model.Character (Category(..), Character)
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Duration (Duration(..), sync)
import           Game.Model.Effect (Effect(..))
import           Game.Model.Game (Game(Game))
import qualified Game.Model.Game
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as N
import           Game.Model.Runnable (Runnable(To), RunConstraint)
import           Game.Model.Skill (Target(..))
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import qualified Game.Model.Status as Status
import           Handler.Play.Wrapper (Wrapper)
import           Util ((!!))

import qualified Blank

describeCategory :: HasCallStack
                 => Category -> Text -> (SpecWith Character) -> SpecWith ()
describeCategory category name specs =
    describe (unpack name) case find matchChar Characters.list of
        Nothing   -> it "exists in the database" False
        Just char -> before (return char) $ parallel specs
  where
    matchChar x = Character.name x == name && Character.category x == category

use :: ∀ m. (HasCallStack, MonadHook m, MonadPlay m, MonadRandom m)
    => Text -> m ()
use name = do
    ninjas <- P.ninjas
    case find ((== name) . Skill.name) . Ninjas.skills $ unsafeHead ninjas of
        Nothing -> error $ "invalid skill: " ++ unpack name
        Just skill -> actWith skill

at :: ∀ m a. MonadPlay m => Target -> m a -> m a
at target = P.withTarget $ targetSlot target

targetSlot :: Target -> Slot
targetSlot Self     = Slot.all !! 0
targetSlot Ally     = Slot.all !! 1
targetSlot Allies   = Slot.all !! 1
targetSlot RAlly    = Slot.all !! 1
targetSlot RXAlly   = Slot.all !! 2
targetSlot XAlly    = Slot.all !! 2
targetSlot XAllies  = Slot.all !! 2
targetSlot Enemy    = Slot.all !! 3
targetSlot Enemies  = Slot.all !! 3
targetSlot REnemy   = Slot.all !! 3
targetSlot XEnemies = Slot.all !! 4
targetSlot Everyone = Slot.all !! 0

targets :: ∀ m. MonadGame m => Target -> m Ninja
targets target = P.ninja $ targetSlot target

act :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m) => m ()
act = do
    Context{skill} <- P.context
    nUser <- P.nUser
    actWith $ Skills.change nUser skill

actWith :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m) => Skill -> m ()
actWith skill = do
    Context{user, target} <- P.context
    Game{playing = player} <- P.game
    unless (Parity.allied user player) $ Engine.processTurn $ return ()
    Engine.processTurn $ Action.act
        Context { new = True, user, target, skill = skill, continues = False }
    P.modify user \n -> n { N.cooldowns = mempty }

enemies :: ∀ m a. (MonadPlay m) => (Ninja -> a) -> m [a]
enemies f = do
    Context{user} <- P.context
    ninjas <- P.enemies user
    return $ f <$> ninjas

turns :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => Int -> m ()
turns (fromIntegral -> i) = do
    Game{playing = player} <- P.game
    replicateM_ (sync i + 1 - fromEnum player) . Engine.processTurn $ return ()

as :: ∀ m. (MonadPlay m, MonadHook m, MonadRandom m)
   => Target -> RunConstraint () -> m ()
as simUser f = P.with (createContext (targetSlot simUser) f) do
    Context{skill} <- P.context
    actWith skill

createContext :: Slot -> RunConstraint () -> Context -> Context
createContext simUser f Context{target, user, skill = Skill{classes}} = Context
    { user      = simUser
    , target    = if target == simUser then user else target
    , new       = True
    , continues = False
    , skill     = Skill.new { Skill.classes = classes `difference` removeClasses
                            , Skill.effects = effects
                            }
    }
  where
    removeClasses = setFromList
                    [Bypassing, Uncounterable, Unreflectable, Unremovable]
    effects
        | target == simUser && user == simUser = [To Self f]
        | otherwise                            = [To XAlly f, To Enemy f]

simOf :: ∀ a. Wrapper -> Target -> ReaderT Context (StateT Wrapper Identity) a
      -> a
simOf game target action =
    runIdentity $ evalStateT (runReaderT action targeted) game
  where
    targeted = Blank.context { Context.target = Sim.targetSlot target }

simAt :: ∀ a. Target -> ReaderT Context (StateT Wrapper Identity) a -> a
simAt = simOf Blank.game

targetIsExposed :: ∀ m. MonadPlay m => m Bool
targetIsExposed = do
    P.with (\context -> context { Context.user = Context.target context })
        $ apply Permanent [Invulnerable All]
    null . Effects.invulnerable <$> P.nTarget

withClass :: ∀ m. MonadPlay m => Class -> m () -> m ()
withClass cla = withClasses $ singletonSet cla

withClasses :: ∀ m. MonadPlay m => EnumSet Class -> m () -> m ()
withClasses classes = P.with ctx
  where
    ctx context  = context { Context.skill = withSkill $ Context.skill context }
    withSkill sk = sk { Skill.classes = insertSet All classes }

statusDur :: Text -> Ninja -> Duration
statusDur name n = maybe Permanent Status.dur . find ((== name) . Status.name)
    $ N.statuses n
