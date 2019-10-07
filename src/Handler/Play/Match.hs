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

import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Database.Persist.Sql (Entity(..), SqlPersistT)
import qualified Database.Persist.Sql as Sql

import Application.Model (Key, User)
import Game.Model.Game (Game(..))
import Game.Model.Player (Player)

data Outcome
    = Victory
    | Defeat
    | Tie
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

inverse :: Outcome -> Outcome
inverse Victory = Defeat
inverse Defeat  = Victory
inverse Tie     = Tie

data Match a = Match { outcomeA :: Outcome
                     , playerA  :: a
                     , playerB  :: a
                     } deriving (Eq, Show, Read)

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
