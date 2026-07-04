{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | User profile handler.
module Handler.Profile (getProfileR) where

import ClassyPrelude hiding (delete)
import Yesod

import qualified Application.App as App
import           Application.Model (EntityField(..))
import           Application.Model.User (User(User))
import qualified Application.Model.User as User
import           Application.Settings (widgetFile)
import qualified Game.Characters as Characters
import qualified Handler.Link as Link
import           Util (fromMaybeM)

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
             , wins
             }      = user
        team        = getTeam teamNames
        level       = User.level user
        levelXp     = User.levelXp user
        rank        = User.rank user
    defaultLayout $(widgetFile "profile/profile")
  where
    getTeam (Just names) = mapMaybe Characters.siteLookup names
    getTeam Nothing      = mempty
