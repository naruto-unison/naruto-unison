{-# LANGUAGE Strict #-}

-- | Uses win/loss records to estimate skill ratings for players.
-- These ratings are internal and should not be exposed in any way to players.
-- They are useful for matchmaking, but should not otherwise affect ranking.
module Core.Rating (update) where

import ClassyPrelude hiding (Handler)

import qualified Database.Persist as DB
import Database.Persist ((=.), (+=.))
import Database.Persist.Sql (SqlPersistT)
import Database.Persist.Types (Update)

import Core.App (Handler)
import Core.Model (EntityField(..), Key, User(..))
import Model.Game (Game(Game, victor))
import Model.Player (Player)

square :: Double -> Double
square x = x * x

-- | Updates fields in the user table based on the end of a game.
-- Win record fields: 'userWins', 'userLosses', 'userStreak'.
-- Skill rating fields: 'userRating', 'userDeviation', 'userVolatility'.
update :: Game -- ^ Completed game.
       -> Player -- ^ Whether the viewed user was 'Player.A' or 'Player.B'.
       -> Key User -- ^ Viewed user.
       -> Key User -- ^ Opponent user.
       -> SqlPersistT Handler ()
update game player who1 who2 = do
    mUser1 <- DB.get who1
    mUser2 <- DB.get who2
    case (mUser1, mUser2) of
        (Just user1, Just user2) -> do
            let (updates1, updates2) = compute (user1, user2) victors
            DB.update who1 updates1
            DB.update who2 updates2
        _ -> return ()
  where
    victors = case game of
        Game{victor = [victor]}
          | player == victor -> (1,   0)
          | otherwise        -> (0,   1)
        _                    -> (0.5, 0.5)

compute :: (User, User) -> (Double, Double) -> ([Update User], [Update User])
compute (playerA, playerB) (scoreA, scoreB) =
    ( updatePlayer playerA playerB scoreA
    , updatePlayer playerB playerA scoreB
    )

-- | Updates the win/loss record.
updateRecord :: Double -> [Update User]
updateRecord 1 = [UserWins +=. 1,   UserStreak +=. 1] -- User won.
updateRecord 0 = [UserLosses +=. 1, UserStreak =. 0] -- User lost.
updateRecord _ = [UserStreak =. 0] -- Tie.

-- | Updates skill ratings.
-- Uses the [Glicko-2 algorithm](http://glicko.net/glicko/glicko2.pdf)
-- by Dr. Mark E. Glickman.
updatePlayer :: User -> User -> Double -> [Update User]
updatePlayer player opponent s =
    updateRecord s ++ [ UserDeviation  =. φ'
                      , UserRating     =. µ'
                      , UserVolatility =. σ'
                      ]
  where
    µ  = userRating     player
    φ  = userDeviation  player
    σ  = userVolatility player
    µ₁ = userRating     opponent
    φ₁ = userDeviation  opponent

    g = 1 / sqrt (1 + 3 * square φ₁ / square pi)
    e = 1 / (1 + exp (- g * (µ - µ₁)))
    v = 1 / (square g * e * (1 - e))

    perf = g * (s - e)
    𝛿 = v * perf

    σ' = sigma 𝛿 φ σ v

    φstar = sqrt $ square φ + square σ'

    φ' = 1 / sqrt (1 / square φstar + 1 / v)
    µ' = µ + square φ' * perf

-- | Constrains the change in volatility over time.
τ :: Double
τ = 0.5

-- | Convergence tolerance for 'sigma'.
ε :: Double
ε = 0.000001

-- | Iteratively calculates the value of σ.
-- Based on the "Illinois algorithm," a variant of the regula falsi procedure.
sigma :: Double -> Double -> Double -> Double -> Double
sigma 𝛿 φ σ v = go a b (f a) (f b)
  where
    a = log $ square σ
    f x = (exp x * (square 𝛿 - square φ - v - exp x))
         / (2 * square (square φ + v + exp x))
         - (x - a) / square τ
    b
      | square 𝛿 > square φ + v = log $ square 𝛿 - square φ - v
      | otherwise               = bracketB 1
    bracketB k
      | f (a - k * τ) < 0 = bracketB $ k + 1
      | otherwise         = a - k * τ
    go a' b' fA fB
      | abs (b' - a') <= ε = exp $ a' / 2
      | fC * fB        < 0 = go b' c fB       fC
      | otherwise          = go a' c (fA / 2) fC
      where
        c = a' + (a' - b') * fA / (fB - fA)
        fC = f c
