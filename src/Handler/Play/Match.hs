-- | Functions related to end-of-game processing.
module Handler.Play.Match
  ( Match
  , Outcome(..)
  , outcome
  , map
  , traverse_
  , fromGame
  , load
  ) where

import ClassyPrelude hiding (map, traverse_)

import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Database.Persist.Sql (Entity(..), SqlPersistT)
import qualified Database.Persist.Sql as Sql

import Application.Model (Key, User)
import Game.Model.Game (Game(..))
import Game.Model.Player (Player)

-- | Result of a game from the perspective of one of its players.
data Outcome
    = Victory
    | Defeat
    | Tie
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

-- | Result of the game from the opposite player.
inverse :: Outcome -> Outcome
inverse Victory = Defeat
inverse Defeat  = Victory
inverse Tie     = Tie

-- | Some piece of information compared for both players, along with the
-- outcome of the game from the perspective of 'playerA'.
data Match a = Match { outcomeA :: Outcome
                     , playerA  :: a
                     , playerB  :: a
                     } deriving (Eq, Show, Read)

-- | Outcome of a game from a player's perspective, based on 'victor'.
outcome :: Game -> Player -> Outcome
outcome Game{victor = [victor]} player
  | victor == player = Victory
  | otherwise        = Defeat
outcome _ _          = Tie

map :: ∀ a b. (Outcome -> a -> a -> b) -> Match a -> Match b
map f Match{outcomeA, playerA, playerB} =
    Match outcomeA
    (f outcomeA playerA playerB) $
    f (inverse outcomeA) playerA playerB

traverse_ :: ∀ f a b. Applicative f
          => (Outcome -> a -> a -> f b) -> Match a -> f ()
traverse_ f Match{outcomeA, playerA, playerB} = do
    f outcomeA playerA playerB
    f (inverse outcomeA) playerB playerA
    pure ()

fromGame :: ∀ a. Game -> Player -> a -> a -> Match a
fromGame g playerX = Match $ outcome g playerX

load :: ∀ m. MonadIO m
     => Match (Key User) -> SqlPersistT m (Maybe (Match (Entity User)))
load Match{outcomeA, playerA, playerB} = runMaybeT do
    userA <- MaybeT $ Sql.get playerA
    userB <- MaybeT $ Sql.get playerB
    return $ Match outcomeA (Entity playerA userA) (Entity playerB userB)
