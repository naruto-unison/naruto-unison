{-# LANGUAGE OverloadedLists #-}

module TestImport
  ( module Import
  , describeCategory
  , act
  , turns
  , enemyTurn
  , targetIsExposed, totalDefense
  , allyOf
  , withClass
  ) where

import ClassyPrelude as Import hiding ((\\), fromList, toList)
import GHC.Exts as Import (fromList, toList)
import Test.Hspec as Import hiding (context)
import Model.Chakra as Import (Chakra(..))
import Model.Character as Import (Category(..), Character)
import Model.Class as Import (Class(..))
import Model.Effect as Import (Amount(..), Effect(..))
import Model.Ninja as Import (is)
import Model.Skill as Import (Target(..))
import Model.Trap as Import (Trigger(..))
import Action.Chakra as Import
import Action.Combat as Import
import Action.Channel as Import
import Action.Skill as Import
import Action.Status as Import
import Action.Trap as Import
import Characters.Base as Import (self, targetHas, userHas)

import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import           Core.Util ((!!), (∈))
import qualified Core.Wrapper as Wrapper
import           Core.Wrapper (Wrapper(Wrapper))
import qualified Class.Parity as Parity
import qualified Class.Play as P
import           Class.Play (MonadGame, MonadPlay)
import           Class.Random (MonadRandom)
import qualified Model.Context as Context
import           Model.Context (Context(Context))
import qualified Model.Defense as Defense
import           Model.Duration (Duration(..), Turns, sync)
import qualified Model.Game as Game
import qualified Model.Character as Character
import           Model.Character (Character(Character))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Player as Player
import           Model.Player (Player)
import           Model.Runnable (Runnable(..), RunConstraint)
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import qualified Model.Slot as Slot
import           Model.Slot (Slot)
import qualified Engine.Effects as Effects
import qualified Engine.Execute as Execute
import qualified Engine.Ninjas as Ninjas
import qualified Engine.Skills as Skills
import qualified Engine.Traps as Traps
import qualified Engine.Trigger as Trigger
import qualified Engine.Turn as Turn
import qualified Characters

-- Because MonadGame and MonadRandom do not actually require IO,
-- all tests are completely pure, and can comfortably run in parallel.
type TestRun = Target
               -> Text
               -> ReaderT Context (StateT Wrapper Identity) (SpecWith ())
               -> SpecWith ()

describeCategory :: Category -> Text -> (TestRun -> SpecWith ()) -> SpecWith ()
describeCategory category name specs =
    describe (unpack name) case find matchChar Characters.list of
        Nothing   -> it "Exists in the database" False
        Just char -> parallel . specs $ useSkill char
  where
    matchChar x = Character.name x == name && Character.category x == category

useSkill :: Character -> TestRun
useSkill char target skillName f =
    describe (unpack skillName) case context <$> findSkill skillName char of
        Nothing  -> it "Exists in the database" False
        Just ctx -> runIdentity $ evalStateT (runReaderT f ctx) $ testGame char
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
    { Wrapper.game   = Game.new
    , Wrapper.ninjas = fromList $ testNinja <$> unsafeTail Slot.all
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
    }

wrap :: ∀ m. (MonadPlay m, MonadRandom m) => Player -> m ()
wrap player = do
    user       <- P.user
    nUser      <- P.nUser
    skill      <- Skills.change nUser <$> P.skill
    let classes = Skill.classes skill
    P.with (\ctx -> ctx { Context.skill = skill }) do
        P.trigger user $ OnAction <$> toList (Skill.classes skill)
        efs        <- Execute.chooseTargets
                      (Skill.start skill ++ Skill.effects skill)
        countering <- Execute.filterCounters efs . toList <$> P.enemies user
        let harm     = not $ null countering
            counters =
                Trigger.userCounters harm user classes nUser
                ++ (Trigger.targetCounters user classes =<< countering)
        if null counters then do
            Execute.effects [] efs
            when (player == Player.A) Execute.addChannels
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
        traverse_ (traverse_ P.launch . Traps.get user) =<< P.ninjas
        P.modifyAll $ Ninjas.processEffects . \n ->
            n { Ninja.triggers = mempty }

act :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
act = Turn.process $ wrap Player.A

turns :: ∀ m. (MonadGame m, MonadRandom m) => Turns -> m ()
turns (Duration -> i) = do
    replicateM_ (sync i) . Turn.process $ return ()
    P.alter \game -> game { Game.playing = Player.A }

enemySkill :: Skill
enemySkill = Skill.new
    { Skill.start   = []
    , Skill.classes = [Summon, Melee, Ranged, Chakra, Physical, Mental, All, NonMental, Bloodline, Genjutsu, Ninjutsu, Taijutsu, Random]
    }

enemyTurn :: ∀ m. (MonadPlay m, MonadRandom m) => RunConstraint () -> m ()
enemyTurn f = do
    P.with with . Turn.process $ wrap Player.B
    P.alter \game -> game { Game.playing = Player.A }
  where
    with ctx = ctx
        { Context.user   = user
        , Context.target = target
        , Context.skill  = enemySkill { Skill.effects = [To Enemy f] }
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
    P.with (\ctx -> ctx { Context.user = target }) $
        apply 0 [Invulnerable All]
    null . Effects.invulnerable . Ninjas.processEffects <$> P.nTarget

totalDefense :: Ninja -> Int
totalDefense n = sum $ Defense.amount <$> Ninja.defense n

allyOf :: ∀ m. MonadGame m => Slot -> m Ninja
allyOf target = P.ninja $ Slot.all !! (Slot.toInt target + 1)

withClass :: ∀ m. MonadPlay m => Class -> m () -> m ()
withClass cla = P.with withContext
  where
    withContext ctx = ctx { Context.skill = withSkill $ Context.skill ctx }
    withSkill sk = sk { Skill.classes = insertSet cla $ Skill.classes sk }
