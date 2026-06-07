{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | User profile handler.
module Handler.Profile (getProfileR) where

import ClassyPrelude hiding (delete)
import Yesod

import qualified Application.App as App
import           Application.Model (EntityField(..),  Privilege(..), User(..))
import           Application.Settings (widgetFile)
import qualified Game.Characters as Characters
import qualified Handler.Link as Link
import           Util ((!?), fromMaybeM)

-- | Renders a 'User' profile.
getProfileR :: Text -> App.Handler Html
getProfileR name = do
    Entity _ user  <- fromMaybeM notFound $ runDB
                    $ selectFirst [ UserName ==. name ] []
    let User { userAvatar
             , userClan
             , userJoined
             , userLosses
             , userName
             , userRecord
             , userStreak
             , userTeam
             , userXp
             , userWins
             }      = user
        team        = getTeam userTeam
        (level, xp) = quotRem userXp 5000
    defaultLayout $(widgetFile "profile/profile")
  where
    getTeam (Just names) = Characters.lookupAll names
    getTeam Nothing      = []

-- | Displays a user's rank, or their 'Privilege' level if higher than 'Normal'.
userRank :: User -> Text
userRank User{userXp, userPrivilege = Normal} = fromMaybe "Hokage"
    $ userRanks !? (userXp `quot` 5000)
  where
    userRanks :: Vector Text
    userRanks = fromList [ "Academy Student"
                         , "Genin"
                         , "Chūnin"
                         , "Missing-Nin"
                         , "Anbu"
                         , "Jōnin"
                         , "Sannin"
                         , "Jinchūriki"
                         , "Akatsuki"
                         , "Kage"
                         , "Hokage"
                         ]
userRank User{userPrivilege} = tshow userPrivilege
