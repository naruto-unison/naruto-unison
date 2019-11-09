module SkillExample (it, useOn) where

import ClassyPrelude

import           Control.Monad.Trans.State.Strict (StateT, evalStateT)
import qualified Test.Hspec as Hspec
import           Test.Hspec hiding (context, it)
import           Test.Hspec.Core.Spec hiding (context, it)

import           Class.Hook (MonadHook)
import           Class.Play (MonadGame, MonadPlay)
import           Class.Random (MonadRandom)
import           Game.Model.Character (Character)
import qualified Game.Model.Character as Character
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Skill (Target(..))
import qualified Game.Model.Skill as Skill
import qualified Game.Model.Slot as Slot
import           Handler.Play.Wrapper (Wrapper)

import qualified Blank
import           Sim (targetSlot)

type SkillArg = (Character, Context)

newtype SkillExample a =
    SkillExample { runGame :: ReaderT Context (StateT Wrapper Identity) a }
    deriving (Monad, Functor, Applicative, MonadGame, MonadHook, MonadPlay, MonadRandom)

instance (Example a, () ~ Arg a) => Example (SkillExample a) where
    type Arg (SkillExample a) = SkillArg

    evaluateExample e params action callback = do
        ref <- newIORef (Result "" Success)
        action (action' >=> writeIORef ref)
        readIORef ref
        where
          action' (char, ctx) = evaluateExample inner params ($ ()) callback
            where
              inner = runIdentity . evalStateT (runReaderT (runGame e) ctx) $
                      testGame char

useOn :: HasCallStack
      => Target -> Text -> SpecWith SkillArg -> SpecWith Character
useOn target skillName f =
    describe (unpack skillName) $ beforeWith withChar $ parallel f
  where
    withChar char = case findSkill skillName char of
        Nothing -> error "useOn" <$ expectationFailure "invalid skill"
        Just (ctx -> context) -> return (char, context)
    findSkill x = find ((== x) . Skill.name) . join . Character.skills
    ctx skill   = Context { skill     = skill { Skill.cost = 0 }
                          , user      = targetSlot Self
                          , target    = targetSlot target
                          , new       = True
                          , continues = False
                          }

it :: ∀ a. (HasCallStack, Example a, () ~ Arg a)
   => String -> SkillExample a -> SpecWith (Arg (SkillExample a))
it = Hspec.it

testGame :: Character -> Wrapper
testGame char = Blank.gameOf
                    $ Ninja.new (unsafeHead Slot.all) char
                    : (Blank.ninjaWithSlot <$> unsafeTail Slot.all)
