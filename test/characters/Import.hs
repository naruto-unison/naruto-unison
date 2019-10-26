{-# LANGUAGE OverloadedLists #-}

module Import
  ( module X
  , describeCategory
  , act
  , turns
  , as
  , targetIsExposed
  , allyOf
  , withClass
  , channel
  ) where

import ClassyPrelude as X hiding ((\\), fromList, toList)
import Game.Action.Chakra as X
import Game.Action.Channel as X
import Game.Action.Combat as X
import Game.Action.Skill as X
import Game.Action.Status as X
import Game.Action.Trap as X
import Game.Characters.Import as X (self, targetHas, userHas)
import Game.Model.Chakra as X (Chakra(..))
import Game.Model.Character as X (Category(..), Character)
import Game.Model.Class as X (Class(..))
import Game.Model.Effect as X (Amount(..), Effect(..))
import Game.Model.Ninja as X (is, totalDefense)
import Game.Model.Skill as X (Target(..))
import Game.Model.Trigger as X (Trigger(..))
import GHC.Exts as X (fromList, toList)
import Test.Hspec as X hiding (context)

import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import           Class.Hook (MonadHook)
import qualified Class.Parity as Parity
import           Class.Play (MonadGame, MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Game.Action as Action
import qualified Game.Characters as Characters
import qualified Game.Engine as Engine
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Skills as Skills
import qualified Game.Engine.Traps as Traps
import qualified Game.Engine.Trigger as Trigger
import           Game.Model.Channel (Channel(Channel), Channeling(..))
import qualified Game.Model.Channel
import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character as Character
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Duration (Duration(..), Turns, sync)
import qualified Game.Model.Game as Game
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import           Game.Model.Runnable (Runnable(..), RunConstraint)
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Handler.Play.Wrapper (Wrapper(Wrapper))
import qualified Handler.Play.Wrapper as Wrapper
import           Util ((!!), (∈))

-- Because MonadGame and MonadRandom do not actually require IO,
-- all tests are completely pure, and can comfortably run in parallel.
type TestRun = Target
               -> Text
               -> ReaderT Context (StateT Wrapper Identity) (SpecWith ())
               -> SpecWith ()

describeCategory :: Category -> Text -> (TestRun -> SpecWith ()) -> SpecWith ()
describeCategory category name specs =
    describe (unpack name) case find matchChar Characters.list of
        Nothing   -> it "exists in the database" False
        Just char -> parallel . specs $ useSkill char
  where
    matchChar x = Character.name x == name && Character.category x == category

useSkill :: Character -> TestRun
useSkill char target skillName f =
    describe (unpack skillName) case findSkill skillName char of
        Nothing  -> it "exists in the database" False
        Just (ctx -> context) ->
            runIdentity $ evalStateT (runReaderT f context) $ testGame char
  where
    findSkill x = find ((== x) . Skill.name) . join . Character.skills
    ctx skill   = Context { Context.skill     = skill
                          , Context.user      = unsafeHead Slot.all
                          , Context.target    = targetSlot target
                          , Context.new       = True
                          , Context.continues = False
                          }

targetSlot :: Target -> Slot
targetSlot Self = Slot.all !! 0
targetSlot Ally = Slot.all !! 1
targetSlot Allies = Slot.all !! 1
targetSlot RAlly = Slot.all !! 1
targetSlot XAlly = Slot.all !! 2
targetSlot XAllies = Slot.all !! 2
targetSlot Enemy = Slot.all !! 3
targetSlot Enemies = Slot.all !! 3
targetSlot REnemy = Slot.all !! 3
targetSlot XEnemies = Slot.all !! 4
targetSlot Everyone = Slot.all !! 0

testBase :: Wrapper
testBase = Wrapper
    { Wrapper.progress = []
    , Wrapper.game     = Game.new
    , Wrapper.ninjas   = fromList $ testNinja <$> unsafeTail Slot.all
    }

testGame :: Character -> Wrapper
testGame char =
    testBase { Wrapper.ninjas = charNinja `cons` Wrapper.ninjas testBase }
  where
    charNinja = Ninja.new (unsafeHead Slot.all) char

testNinja :: Slot -> Ninja
testNinja slot = Ninja.new slot $ Character
    { Character.name     = "Ninja " ++ tshow slot
    , Character.bio      = ""
    , Character.skills   = [[Skill.new], [Skill.new], [Skill.new], [Skill.new]]
    , Character.category = Original
    , Character.groups   = []
    , Character.price    = 0
    }

-- | A mock engine based on 'Game.Action.act'.
wrap :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m) => Player -> m ()
wrap player = do
    user       <- P.user
    nUser      <- P.nUser
    skill      <- Skills.change nUser <$> P.skill
    let classes = Skill.classes skill
    P.with (\context -> context { Context.skill = skill }) do
        P.trigger user $ OnAction <$> toList (Skill.classes skill)
        startEfs   <- Action.chooseTargets $ Skill.start skill
        midEfs     <- Action.chooseTargets $ Skill.effects skill
        let allEfs  = startEfs ++ midEfs
        countering <- Action.filterCounters allEfs . toList <$> P.enemies user
        let harm     = not $ null countering
            counters =
                Trigger.userCounters harm user classes nUser
                ++ (Trigger.targetCounters user classes =<< countering)
        if null counters then case Skill.dur skill of
            Instant -> Action.run allEfs
            _       -> do
                Action.run startEfs
                P.withContinues $ Action.run midEfs
                when (player == Player.A) Action.addChannels
        else do
            let countered = Ninja.slot <$> countering
                uncounter n
                  | slot == user     = Trigger.userUncounter  classes n
                  | slot ∈ countered = Trigger.targetUncounter classes n
                  | otherwise        = n
                  where
                    slot = Ninja.slot n
            P.modifyAll uncounter
            sequence_ counters

        P.modify user \n -> n { Ninja.acted = True }
        traverse_ (sequence_ . Traps.get user) =<< P.ninjas
        P.modifyAll \n -> n { Ninja.triggers = mempty }

act :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m) => m ()
act = Engine.processTurn $ wrap Player.A

turns :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => Turns -> m ()
turns (Duration -> i) = do
    replicateM_ (sync i) . Engine.processTurn $ return ()
    P.alter \g -> g { Game.playing = Player.A }

as :: ∀ m. (MonadPlay m, MonadHook m, MonadRandom m)
   => Target -> RunConstraint () -> m ()
as t f = do
    P.with with . Engine.processTurn $ wrap Player.B
    P.alter \g -> g { Game.playing = Player.A }
  where
    with context = Context {user, target, skill, new = True, continues = False}
      where
        skill = Skill.new { Skill.effects = [To t' f], Skill.classes }
        user = targetSlot t
        ctxTarget = Context.target context
        target
          | ctxTarget == user = Context.user context
          | otherwise         = ctxTarget
        t'
          | ctxTarget == user            = Enemy
          | Parity.allied user ctxTarget = Ally
          | otherwise                    = Enemy
        classes = All `insertSet`
                  Skill.classes (Context.skill context) `difference`
                  [Bypassing, Uncounterable, Unreflectable, Unremovable]

targetIsExposed :: ∀ m. MonadPlay m => m Bool
targetIsExposed = do
    target <- P.target
    P.with (\context -> context { Context.user = target }) $
        apply 0 [Invulnerable All]
    null . Effects.invulnerable <$> P.nTarget

allyOf :: ∀ m. MonadGame m => Slot -> m Ninja
allyOf target = P.ninja $ Slot.all !! (Slot.toInt target + 1)

withClass :: ∀ m. MonadPlay m => Class -> m () -> m ()
withClass cla = P.with ctx
  where
    ctx context = context { Context.skill = withSkill $ Context.skill context }
    withSkill sk = sk { Skill.classes = cla `insertSet` Skill.classes sk }

channel :: ∀ m. MonadPlay m => Text -> m ()
channel name = do
    user <- P.user
    target <- P.target
    let chan = Channel { target
                       , skill = Skill.new { Skill.name }
                       , dur   = Passive
                       }
    P.modify user \n -> n { Ninja.channels = chan : Ninja.channels n }
