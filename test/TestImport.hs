{-# LANGUAGE OverloadedLists #-}

module TestImport
  ( module Import
  , describeCategory
  , act
  , turns
  , enemyTurn
  , targetIsExposed
  , allyOf
  , withClass
  ) where

import ClassyPrelude as Import hiding ((\\), fromList, toList)
import Game.Action.Chakra as Import
import Game.Action.Channel as Import
import Game.Action.Combat as Import
import Game.Action.Skill as Import
import Game.Action.Status as Import
import Game.Action.Trap as Import
import Game.Characters.Base as Import (self, targetHas, userHas)
import Game.Model.Chakra as Import (Chakra(..))
import Game.Model.Character as Import (Category(..), Character)
import Game.Model.Class as Import (Class(..))
import Game.Model.Effect as Import (Amount(..), Effect(..))
import Game.Model.Ninja as Import (is, totalDefense)
import Game.Model.Skill as Import (Target(..))
import Game.Model.Trigger as Import (Trigger(..))
import GHC.Exts as Import (fromList, toList)
import Test.Hspec as Import hiding (context)

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
import           Game.Model.Skill (Skill)
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
        Just (context -> ctx) ->
            runIdentity $ evalStateT (runReaderT f ctx) $ testGame char
  where
    findSkill x   = find ((x ==) . Skill.name) . join . Character.skills
    context skill = Context { Context.skill  = skill
                            , Context.user   = unsafeHead Slot.all
                            , Context.target = targetSlot
                            , Context.new    = True
                            }
    targetSlot    = (Slot.all !!) case target of
        Self       -> 0
        Ally       -> 1
        Allies     -> 1
        RAlly      -> 1
        XAlly      -> 2
        XAllies    -> 2
        Enemy      -> 3
        Enemies    -> 3
        REnemy     -> 3
        XEnemies   -> 4
        Everyone   -> 0

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
    , Character.price    = 0
    }

wrap :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m) => Player -> m ()
wrap player = do
    user       <- P.user
    nUser      <- P.nUser
    skill      <- Skills.change nUser <$> P.skill
    let classes = Skill.classes skill
    P.with (\ctx -> ctx { Context.skill = skill }) do
        P.trigger user $ OnAction <$> toList (Skill.classes skill)
        efs        <- Action.chooseTargets
                      (Skill.start skill ++ Skill.effects skill)
        countering <- Action.filterCounters efs . toList <$> P.enemies user
        let harm     = not $ null countering
            counters =
                Trigger.userCounters harm user classes nUser
                ++ (Trigger.targetCounters user classes =<< countering)
        if null counters then do
            Action.run [] efs
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
    P.alter \game -> game { Game.playing = Player.A }

enemySkill :: Skill
enemySkill = Skill.new
    { Skill.start   = []
    , Skill.classes = [All]
    }

enemyTurn :: ∀ m. (MonadPlay m, MonadHook m, MonadRandom m) => RunConstraint () -> m ()
enemyTurn f = do
    P.with with . Engine.processTurn $ wrap Player.B
    P.alter \game -> game { Game.playing = Player.A }
  where
    with ctx = ctx
        { Context.user   = user
        , Context.target = target
        , Context.skill  = enemySkill 
            { Skill.effects = [To Enemy f]
            , Skill.classes = Skill.classes enemySkill
                              ++ Skill.classes (Context.skill ctx)
            }
        }
      where
        user = Slot.all !! 3
        ctxTarget = Context.target ctx
        target
          | Parity.allied ctxTarget user = Context.user ctx
          | otherwise                    = ctxTarget

targetIsExposed :: ∀ m. (MonadPlay m, MonadRandom m) => m Bool
targetIsExposed = do
    target <- P.target
    P.with (\ctx -> ctx { Context.user = target }) $ apply 0 [Invulnerable All]
    null . Effects.invulnerable <$> P.nTarget

allyOf :: ∀ m. MonadGame m => Slot -> m Ninja
allyOf target = P.ninja $ Slot.all !! (Slot.toInt target + 1)

withClass :: ∀ m. MonadPlay m => Class -> m () -> m ()
withClass cla = P.with withContext
  where
    withContext ctx = ctx { Context.skill = withSkill $ Context.skill ctx }
    withSkill sk = sk { Skill.classes = cla `insertSet` Skill.classes sk }
