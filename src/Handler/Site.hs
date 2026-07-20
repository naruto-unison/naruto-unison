{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Miscellaneous website handlers.
module Handler.Site
  ( getHomeR
  , getChangelogR
  , getGuideR
  , getCharactersR, getCharacterR
  , getGroupsR
  , getMechanicsR
  , getTeamBuildingR
  ) where

import ClassyPrelude
import Yesod

import           Text.Blaze.Html (preEscapedToHtml)
import qualified Yesod.Auth as Auth

import           Application.App (Route(..))
import qualified Application.App as App
import           Application.Model.News (News(News))
import qualified Application.Model.News as News
import qualified Application.Model.NewsPost as NewsPost
import           Application.Model.NewsPost (NewsPost(NewsPost))
import           Application.Settings (widgetFile)
import           Class.Display (Display(..), shorten)
import qualified Game.Characters as Characters
import           Game.Model.Character (Category(..), Character(Character))
import qualified Game.Model.Character as Character
import qualified Game.Model.Class as Class
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Handler.Client.Data (addDataJS)
import qualified Handler.Site.Link as Link
import           Handler.Site.Parse (richText)
import qualified Mission
import           Mission.Goal (Goal(Reach))
import qualified Mission.Goal as Goal
import           Util ((∈), epoch)

-- | Renders the changelog.
getChangelogR :: App.Handler Html
getChangelogR = do
    App.unchanged304
    (title, _) <- breadcrumbs
    defaultLayout do
        addDataJS
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "home/changelog")
  where
    change = getChangelog True

-- | Renders the homepage of the website.
getHomeR :: App.Handler Html
getHomeR = do
    newsList <- runDB $ NewsPost.selectAll 5 1 Nothing
    App.lastModified . maybe epoch (News.time . NewsPost.news) $ headMay newsList
    defaultLayout do
        addDataJS
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "home/home")
  where
    change = getChangelog False

data LogType
    = Balance
    | New
    | Rework
    deriving (Bounded, Enum, Eq, Ord, Show)

logLabel :: Bool -> LogType -> Text
logLabel True  Balance = "Balance update:"
logLabel False Balance = "Balance:"
logLabel True  New     = "New character:"
logLabel False New     = "New:"
logLabel True  Rework  = "Character rework:"
logLabel False Rework  = "Rework:"

getCharacterEx :: Text -> Character
getCharacterEx ident = fromMaybe (error err) $ Characters.siteLookup ident
  where
    err = "Site.getChangelog: character " ++ unpack ident ++ " not found"


getChangelog :: Bool -> LogType -> Text -> Character.Category -> App.Widget
getChangelog long logType name category = $(widgetFile "widgets/change")
  where
    change  = logLabel long
    ident = Character.identFrom category name
    char = getCharacterEx ident

getCharacter :: Text -> Character.Category -> App.Widget
getCharacter name category = $(widgetFile "widgets/character")
  where
    ident = Character.identFrom category name
    char = getCharacterEx ident

news :: NewsPost -> App.Widget
news NewsPost{author, tags, newsID, news = News{content, time, title}} =
    $(widgetFile "home/news")

-- Renders the game guide, which includes the list of characters as well as
-- introductions to game mechanics.
getGuideR :: App.Handler Html
getGuideR = do
    App.unchanged304
    loggedin   <- isJust <$> Auth.maybeAuthId
    (title, _) <- breadcrumbs
    defaultLayout $(widgetFile "guide/guide")

-- Renders the list of all characters.
getCharactersR :: App.Handler Html
getCharactersR = do
    App.unchanged304
    (title, _) <- breadcrumbs
    defaultLayout $(widgetFile "guide/characters")
  where
    categoryChars category = filter ((== category) . Character.category)
                             Characters.siteList
    categories             = [minBound..maxBound]
    heading :: Category -> Html
    heading Original   = "Original"
    heading Shippuden  = "Shippūden"
    heading Reanimated = "Reanimated"

-- | Renders a character's details and the user's progress on their mission.
getCharacterR :: Character -> App.Handler Html
getCharacterR Character{bio, category, ident, name, skills} = do
    -- due to mission objectives, content does change if logged in
    whenM (isNothing <$> Auth.maybeAuthId) App.unchanged304
    mmission <- Mission.userMission ident
    defaultLayout $(widgetFile "guide/character")
  where
    skillClasses classes = intercalate ", "
        $ display <$> filter Class.visible (toList classes)

-- | Renders character groups.
getGroupsR :: App.Handler Html
getGroupsR = do
    App.unchanged304
    (title, _) <- breadcrumbs
    defaultLayout $(widgetFile "guide/groups")
  where
    groups    = [minBound..maxBound]
    inGroup x = (x ∈) . Character.groups

-- | Renders the game mechanics guide.
getMechanicsR :: App.Handler Html
getMechanicsR = do
    App.unchanged304
    (title, _) <- breadcrumbs
    defaultLayout do
        addDataJS
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "guide/mechanics")

-- | Renders the team building guide.
getTeamBuildingR :: App.Handler Html
getTeamBuildingR = do
    App.unchanged304
    (title, _) <- breadcrumbs
    defaultLayout do
        addDataJS
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "guide/teambuilding")
  where
    character = getCharacter
