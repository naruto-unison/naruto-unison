{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE NoBangPatterns              #-}
{-# LANGUAGE QuasiQuotes                 #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- | Miscellaneous website handlers.
module Handler.Site
  ( getHomeR
  , getChangelogR
  , getGuideR
  , getCharactersR, getCharacterR
  , getMechanicsR
  ) where

import ClassyPrelude
import Yesod

import           Data.List (nubBy)
import qualified Data.List.NonEmpty as NonEmpty
import           Text.Blaze.Html (preEscapedToHtml)

import           Util (shorten)
import           Application.App (Handler, Route(..), Widget)
import           Application.Model (Cite(..), EntityField(..), News(..), User(..))
import           Application.Settings (widgetFile)
import           Class.Display (Display(..))
import qualified Game.Model.Character as Character
import           Game.Model.Character (Category(..))
import qualified Game.Model.Class as Class
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Skill as Skill
import           Game.Model.Skill (Skill)
import qualified Game.Characters as Characters
import qualified Handler.Forum as Forum
import qualified Handler.Parse as Parse
import qualified Mission
import qualified Mission.Goal as Goal

userlink :: User -> Widget
userlink User{..} = $(widgetFile "widgets/userlink")

-- | Renders the changelog.
getChangelogR :: Handler Html
getChangelogR = do
    (title, _) <- breadcrumbs
    defaultLayout do
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "changelog/changelog")
  where
    change = getChangelog True

-- | Renders the homepage of the website.
getHomeR :: Handler Html
getHomeR = do
    mmsg     <- getMessage
    newsList <- runDB $ traverse withAuthor
                        =<< selectList [] [Desc NewsDate, LimitTo 5]
    topics   <- Forum.selectWithAuthors [] [Desc TopicTime, LimitTo 10]
    citelink <- liftIO Forum.makeCitelink
    defaultLayout do
        setTitle "Naruto Unison"
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "home/home")
  where
    change = getChangelog False
    withAuthor (Entity _ new) = ((new, ) <$>) <$> get $ newsAuthor new

(!) :: Text -> Text -> Html
name ! l = [shamlet| $newline never
<a .skill data-name=#{name}>#{l}|]

data LogType = Added | New | Rework | Change

logLabel :: Bool -> LogType -> Text
logLabel True  Added  = "Character added:"
logLabel False Added  = "Added:"
logLabel True  New    = "New character:"
logLabel False New    = "New:"
logLabel True  Rework = "Character rework:"
logLabel False Rework = "Rework:"
logLabel True  Change = "Character update:"
logLabel False Change = "Update:"

separate :: NonEmpty Skill -> [Skill]
separate = nubBy ((==) `on` Skill.name) . toList

getChangelog :: Bool -> LogType -> Text -> Character.Category -> Widget
getChangelog long logType name characterType =
    case Characters.lookupName tagName of
        Nothing -> [whamlet|Error: character #{tagName} not found!|]
        Just char -> $(widgetFile "changelog/change")
  where
    change   = logLabel long
    tagName  = case characterType of
        Original   -> name
        Reanimated -> name ++ " (R)"
        Shippuden  -> name ++ " (S)"

news :: (News, Maybe User) -> Widget
news (News{..}, author) = $(widgetFile "home/news")

getGuideR :: Handler Html
getGuideR = do
    (title, _) <- breadcrumbs
    defaultLayout $(widgetFile "guide/guide")

getCharactersR :: Handler Html
getCharactersR = do
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

getCharacterR :: Category -> Text -> Handler Html
getCharacterR category charLink = case Characters.lookupSite category charLink of
    Nothing   -> notFound
    Just char -> do
        let name = Character.format char
            path = shorten name
        mmission <- Mission.userMission name
        defaultLayout $(widgetFile "guide/character")
  where
    skillClasses skill =
        intercalate ", " $
        display <$> filter Class.visible (toList $ Skill.classes skill)

getMechanicsR :: Handler Html
getMechanicsR = do
    (title, _) <- breadcrumbs
    defaultLayout $(widgetFile "guide/mechanics")
