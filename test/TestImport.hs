{-# LANGUAGE OverloadedLists #-}

module TestImport
  ( module Import
  , describeCategory
  , act
  , turns
  , targetTurn
  ) where

import ClassyPrelude as Import
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
import Characters.Base as Import (self)

import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Test.Hspec (SpecWith, describe, it)

import           Core.Util ((!!))
import qualified Core.Wrapper as Wrapper
import           Core.Wrapper (Wrapper(Wrapper))
import qualified Class.Play as P
import           Class.Play (MonadGame, MonadPlay)
import           Class.Random (MonadRandom)
import qualified Model.Context as Context
import           Model.Context (Context(Context))
import           Model.Duration (Duration(..), Turns, sync)
import qualified Model.Game as Game
import qualified Model.Character as Character
import           Model.Character (Character(Character))
import qualified Model.Player as Player
import           Model.Player (Player)
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Skill as Skill
import qualified Model.Slot as Slot
import           Model.Slot (Slot)
import qualified Engine.Adjust as Adjust
import qualified Engine.Execute as Execute
import qualified Engine.Traps as Traps
import qualified Engine.Turn as Turn
import qualified Characters


type TestRun = Target
               -> Text
               -> ReaderT Context (StateT Wrapper Identity) (SpecWith ())
               -> SpecWith ()

describeCategory :: Category -> Text -> (TestRun -> SpecWith ()) -> SpecWith ()
describeCategory category name specs =
    describe (unpack name) case find matchChar Characters.list of
        Nothing   -> it "Exists in the database" False
        Just char -> specs $ useSkill char
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
        Ally       -> 2
        Allies     -> 2
        RAlly      -> 2
        XAlly      -> 4
        XAllies    -> 4
        Enemy      -> 1
        Enemies    -> 1
        REnemy     -> 1
        XEnemies   -> 3
        Everyone   -> 0
        Specific x -> Slot.toInt x

testBase :: Wrapper
testBase = Wrapper
    { Wrapper.game = Game.new
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
    , Character.hooks    = mempty
    , Character.category = Original
    }

wrap :: ∀ m. (MonadPlay m, MonadRandom m) => Player -> m () -> m ()
wrap player f = do
    whenM ((player /=) . Game.playing <$> P.game) $ Turn.run []
    user  <- P.user
    skill <- P.skill
    P.trigger user $ OnAction <$> toList (Skill.classes skill)
    f
    when (player == Player.A) Execute.addChannels
    traverse_ (traverse_ P.launch . Traps.get user) =<< P.ninjas
    P.modifyAll Adjust.effects

act :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
act = Turn.process . wrap Player.A $ Execute.effects []



turns :: ∀ m. (MonadGame m, MonadRandom m) => Turns -> m ()
turns (Duration -> i) = replicateM_ (sync i) $ Turn.run []

targetTurn :: ∀ m. (MonadPlay m, MonadRandom m) => m () -> m ()
targetTurn = Turn.process . wrap Player.B . P.with \ctx ->
    ctx { Context.user   = Context.target ctx
        , Context.target = Context.user ctx
        }
