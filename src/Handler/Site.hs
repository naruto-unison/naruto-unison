{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoBangPatterns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Miscellaneous website handlers.
module Handler.Site
  ( getHomeR
  , getChangelogR
  , getGuideR
  , getCharactersR, getCharacterR
  , getGroupsR
  , getMechanicsR
  ) where

import ClassyPrelude
import Yesod

import           Data.List (nubBy)
import qualified Data.List.NonEmpty as NonEmpty
import           Text.Blaze.Html (preEscapedToHtml)
import qualified Yesod.Auth as Auth

import           Application.App (Handler, Route(..), Widget)
import qualified Application.App as App
import           Application.Model (EntityField(..), News(..), User(..))
import           Application.Settings (widgetFile)
import           Class.Display (Display(..))
import qualified Game.Characters as Characters
import           Game.Model.Character (Category(..), Character)
import qualified Game.Model.Character as Character
import qualified Game.Model.Class as Class
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import qualified Handler.Forum as Forum
import qualified Handler.Link as Link
import qualified Handler.Parse as Parse
import qualified Mission
import qualified Mission.Goal as Goal
import           Util ((∈), shorten)

-- | Renders the changelog.
getChangelogR :: Handler Html
getChangelogR = do
    App.unchanged304
    (title, _) <- breadcrumbs
    defaultLayout do
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "home/changelog")
  where
    change = getChangelog True

-- | Renders the homepage of the website.
getHomeR :: Handler Html
getHomeR = do
    newsList <- runDB $ traverse withAuthor
                        =<< selectList [] [Desc NewsDate, LimitTo 5]
    topics   <- Forum.selectWithAuthors [] [Desc ForumTopicTime, LimitTo 10]
    citelink <- liftIO Link.cite
    defaultLayout do
        setTitle "Naruto Unison"
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "home/home")
  where
    change = getChangelog False
    withAuthor (Entity _ new) = ((new, ) <$>) <$> get $ newsAuthor new

data LogType
    = Balance
    | New
    | Rework
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

logLabel :: Bool -> LogType -> Text
logLabel True  Balance = "Balance update:"
logLabel False Balance = "Balance:"
logLabel True  New     = "New character:"
logLabel False New     = "New:"
logLabel True  Rework  = "Character rework:"
logLabel False Rework  = "Rework:"

separate :: NonEmpty Skill -> [Skill]
separate = nubBy ((==) `on` Skill.name) . toList

getChangelog :: Bool -> LogType -> Text -> Character.Category -> Widget
getChangelog long logType name category = case Characters.lookup tagName of
    Just char ->
        $(widgetFile "home/change")
    Nothing ->
        error $ "Site.getChangelog: character " ++ unpack tagName
                ++ " not found"
  where
    change  = logLabel long
    tagName = Character.identFrom category name

news :: (News, Maybe User) -> Widget
news (News{..}, author) = $(widgetFile "home/news")

-- Renders the game guide, which includes the list of characters as well as
-- introductions to game mechanics.
getGuideR :: Handler Html
getGuideR = do
    App.unchanged304
    loggedin   <- isJust <$> Auth.maybeAuthId
    (title, _) <- breadcrumbs
    defaultLayout $(widgetFile "guide/guide")

-- Renders the list of all characters.
getCharactersR :: Handler Html
getCharactersR = do
    App.unchanged304
    (title, _) <- breadcrumbs
    defaultLayout $(widgetFile "guide/characters")
  where
    categories = [minBound..maxBound]
    heading :: Category -> Html
    heading Original   = "Original"
    heading Shippuden  = "Shippūden"
    heading Reanimated = "Reanimated"
    categoryChars category =
        filter ((== category) . Character.category) Characters.list

-- | Renders a character's details and the user's progress on their mission.
getCharacterR :: Character -> Handler Html
getCharacterR char = do
    -- content does change if logged in, due to mission objectives
    whenM (isNothing <$> Auth.maybeAuthId) App.unchanged304
    mmission <- Mission.userMission name
    defaultLayout $(widgetFile "guide/character")
  where
    name = Character.ident char
    skillClasses sk =
        intercalate ", " $
        display <$> filter Class.visible (toList $ Skill.classes sk)

-- | Renders character groups.
getGroupsR :: Handler Html
getGroupsR = do
    App.unchanged304
    (title, _) <- breadcrumbs
    defaultLayout $(widgetFile "guide/groups")
  where
    groups    = [minBound..maxBound]
    inGroup x = (x ∈) . Character.groups

-- | Renders the game mechanics guide.
getMechanicsR :: Handler Html
getMechanicsR = do
    App.unchanged304
    (title, _) <- breadcrumbs
    defaultLayout $(widgetFile "guide/mechanics")
