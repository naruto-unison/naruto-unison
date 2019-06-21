module Model.Wrapper
  ( Wrapper(..)
  , new, new'
  , game
  ) where

import ClassyPrelude

import qualified System.Random.MWC as Random

import           Model.Internal (Wrapper(..))
import           Model.Game (Game)

game :: ∀ m. MonadIO m => Wrapper -> m Game
game = readIORef . gameRef

new :: ∀ m. MonadIO m => Game -> m Wrapper
new g = liftIO Random.createSystemRandom >>= flip new' g

new' :: ∀ m. MonadIO m => Random.GenIO -> Game -> m Wrapper
new' rand g = do
    gameRef <- newIORef g
    return Wrapper{..}
