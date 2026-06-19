{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | User profile handler.
module Handler.Profile (getProfileR) where

import ClassyPrelude hiding (delete)
import Yesod

import qualified Application.App as App
import           Application.Model (EntityField(..))
import           Application.Model.User (Privilege(..), User(User))
import qualified Application.Model.User
import           Application.Settings (widgetFile)
import qualified Game.Characters as Characters
import qualified Handler.Link as Link
import           Util ((!?), fromMaybeM)

-- | Renders a 'User' profile.
getProfileR :: Text -> App.Handler Html
getProfileR name = do
    Entity _ user  <- fromMaybeM notFound $ runDB
                    $ selectFirst [ UserName ==. name ] []
    let User { avatar
             , clan
             , joined
             , losses
             , record
             , streak
             , team = teamNames
             , xp = totalXp
             , wins
             }      = user
        team        = getTeam teamNames
        (level, xp) = quotRem totalXp 5000
        rank        = userRank user
    defaultLayout $(widgetFile "profile/profile")
  where
    getTeam (Just names) = mapMaybe Characters.siteLookup names
    getTeam Nothing      = mempty

-- | Displays a user's rank, or their 'Privilege' level if higher than 'Normal'.
userRank :: User -> Text
userRank User{xp, privilege = Normal} = fromMaybe "Hokage"
    $ userRanks !? (xp `quot` 5000)
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
userRank User{privilege} = tshow privilege
