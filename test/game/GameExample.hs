module GameExample (sim) where

import ClassyPrelude

import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Test.Hspec hiding (context, it)
import Test.Hspec.Core.Spec hiding (context, it)

import Class.Hook (MonadHook)
import Class.Play (MonadGame, MonadPlay)
import Class.Random (MonadRandom)
import Game.Model.Context (Context)
import Handler.Play.Wrapper (Wrapper)

import qualified Blank

newtype GameExample a =
    GameExample { runGame :: ReaderT Context (StateT Wrapper Identity) a }
    deriving (Monad, Functor, Applicative, MonadGame, MonadHook, MonadPlay, MonadRandom)

instance (Example a, () ~ Arg a) => Example (GameExample a) where
    type Arg (GameExample a) = ()

    evaluateExample e params action callback = do
        ref <- newIORef (Result "" Success)
        action (action' >=> writeIORef ref)
        readIORef ref
        where
          action' () = evaluateExample (sim e) params ($ ()) callback

sim :: GameExample a -> a
sim e = runIdentity $ evalStateT (runReaderT (runGame e) Blank.context) Blank.game
