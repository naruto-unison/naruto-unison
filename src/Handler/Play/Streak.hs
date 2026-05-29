-- | Uses win/loss records to estimate skill ratings for players.
-- These ratings are internal and should not be exposed in any way to players.
-- They are useful for matchmaking, but should not otherwise affect ranking.
module Handler.Play.Streak (updatePostMatch) where

import ClassyPrelude
import Database.Esqueleto.Experimental

import Application.Model (EntityField(..), User(..))

updatePostMatch :: ∀ m. MonadIO m => Key User -> SqlPersistT m ()
updatePostMatch who = update \row -> do
    set row [ UserRecord =. row ^. UserStreak ]
    where_ $ row ^. UserId    ==. val who
    where_ $ row ^. UserStreak >. row ^. UserRecord
