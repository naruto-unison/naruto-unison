module Wrapper
    ( Wrapper
    , WrapperM
    , new
    , run
    ) where

import ClassyPrelude

import           Control.Monad.Trans.State.Strict (StateT(..), gets, modify', evalStateT)
import qualified Data.ByteString.Short as SBS
import           System.Random.Stateful (StatefulGen(..), Uniform(..))

import           Class.Hook (MonadHook)
import qualified Class.Hook
import           Class.Play (MonadGame)
import qualified Class.Play
import           Class.Random (MonadRandom)
import qualified Class.Random
import           Game.Model.Game (Game)
import qualified Game.Model.Game as Game
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Slot as Slot

data Ninjas = Ninjas { n0 :: Ninja
                     , n1 :: Ninja
                     , n2 :: Ninja
                     , n3 :: Ninja
                     , n4 :: Ninja
                     , n5 :: Ninja
                     }

getNinja :: Int -> Ninjas -> Ninja
getNinja 0 = n0
getNinja 1 = n1
getNinja 2 = n2
getNinja 3 = n3
getNinja 4 = n4
getNinja 5 = n5
getNinja _ = error "out of range"

modifyNinja :: Int -> (Ninja -> Ninja) -> Ninjas -> Ninjas
modifyNinja 0 f ns = ns { n0 = f ns.n0 }
modifyNinja 1 f ns = ns { n1 = f ns.n1 }
modifyNinja 2 f ns = ns { n2 = f ns.n2 }
modifyNinja 3 f ns = ns { n3 = f ns.n3 }
modifyNinja 4 f ns = ns { n4 = f ns.n4 }
modifyNinja 5 f ns = ns { n5 = f ns.n5 }
modifyNinja _ _ _  = error "out of range"

mapNinjas :: (Ninja -> Ninja) -> Ninjas -> Ninjas
mapNinjas m (Ninjas a b c d e f) = Ninjas (m a) (m b) (m c) (m d) (m e) (m f)

ninjasToVector :: Ninjas -> Vector Ninja
ninjasToVector (Ninjas a b c d e f) = fromList [a, b, c, d, e, f]

data Wrapper = Wrapper
    { game   :: Game
    , ninjas :: Ninjas
    }

new :: HasCallStack => [Ninja] -> Wrapper
new [a, b, c, d, e, f] = Wrapper Game.new $ Ninjas a b c d e f
new _                  = error "wrong number of ninjas"

type WrapperM = StateT Wrapper Identity

run :: ∀ a. Wrapper -> WrapperM a -> a
run game f = runIdentity $ evalStateT f game

modifyNinjas' :: (Ninjas -> Ninjas) -> WrapperM ()
modifyNinjas' f = modify' \(Wrapper g ns) -> Wrapper g $ f ns

instance MonadGame WrapperM where
    game        = gets game
    alterGame f = modify' \(Wrapper g ns) -> Wrapper (f g) ns
    ninjas      = gets $ ninjasToVector . ninjas
    ninja i     = gets $ getNinja (Slot.toInt i) . ninjas
    write i x   = modifyNinjas' $ modifyNinja (Slot.toInt i) (const x)
    modify i f  = modifyNinjas' $ modifyNinja (Slot.toInt i) f
    modifyAll f = modifyNinjas' $ mapNinjas f

instance MonadHook WrapperM where
    action _ _ _  = return ()
    chakra _ _ _  = return ()
    trap _ _      = return ()
    trigger _ _   = return ()
    turnEnd _ _ _ = return ()

instance StatefulGen () WrapperM where
    uniformWord32 _ = return 0
    uniformWord64 _ = return 0
    uniformShortByteString n _ = return $ SBS.replicate n 0

instance MonadRandom WrapperM where
    random       = uniformM ()
    shuffle      = return
    range (_, x) = return x
