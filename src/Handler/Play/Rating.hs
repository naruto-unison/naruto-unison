{-# LANGUAGE Strict #-}

-- | Uses win/loss records to estimate skill ratings for players.
-- These ratings are internal and should not be exposed in any way to players.
-- They are useful for matchmaking, but should not otherwise affect ranking.
module Handler.Play.Rating (update) where

import ClassyPrelude

import qualified Database.Esqueleto as ESQL
import           Database.Esqueleto ((>.), (^.), (==.))
import qualified Database.Persist.Sql as Sql
import           Database.Persist.Sql ((=.), (+=.), Entity(..), SqlPersistT)
import           Database.Persist.Types (Update)

import           Application.Model (EntityField(..), Key, User(..))
import qualified Handler.Play.Match as Match
import           Handler.Play.Match (Match, Outcome(..))

square :: Double -> Double
square x = x * x

-- | Updates fields in the user table based on the end of a game.
-- Win record fields: 'userWins', 'userLosses', 'userStreak'.
-- Skill rating fields: 'userRating', 'userDeviation', 'userVolatility'.
update :: ∀ m. MonadIO m => Match (Entity User) -> SqlPersistT m ()
update match = Match.traverse_ go match
  where
    go outcome (Entity who player) (Entity _ opponent) = do
        Sql.update who $ compute outcome player opponent
        updateStreak who

compute :: Outcome -> User -> User -> [Update User]
compute outcome player opponent = updateUser player opponent outcome

-- | Updates the win/loss record.
updateRecord :: Outcome -> [Update User]
updateRecord Victory = [UserWins +=. 1, UserStreak +=. 1]
updateRecord Defeat  = [UserLosses +=. 1, UserStreak =. 0]
updateRecord Tie     = [UserStreak =. 0]

updateStreak :: ∀ m. MonadIO m => Key User -> SqlPersistT m ()
updateStreak who =
    ESQL.update \p -> do
        ESQL.set p [ UserRecord ESQL.=. p ^. UserStreak ]
        ESQL.where_ $ p ^. UserId ==. ESQL.val who
        ESQL.where_ $ p ^. UserStreak >. p ^. UserRecord

-- | Updates skill ratings.
-- Uses the [Glicko-2 algorithm](http://glicko.net/glicko/glicko2.pdf)
-- by Dr. Mark E. Glickman.
updateUser :: User -> User -> Outcome -> [Update User]
updateUser player opponent outcome =
    updateRecord outcome ++ [ UserDeviation  =. φ'
                            , UserRating     =. µ'
                            , UserVolatility =. σ'
                            ]
  where
    s  = case outcome of
        Victory -> 1
        Defeat  -> 0
        Tie     -> 0.5
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
