{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE NoBangPatterns              #-}
{-# LANGUAGE QuasiQuotes                 #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- | Miscellaneous website handlers.
module Handler.Site
  ( getChangelogR
  , getHomeR
  , getCharactersR, getCharacterR
  ) where

import ClassyPrelude hiding (Handler)
import Yesod

import Data.List (nubBy)
import Text.Blaze.Html (preEscapedToHtml)

import           Core.App (Handler, Route(..), Widget)
import           Core.Model (Cite(..), EntityField(..), News(..), User(..))
import           Core.Settings (widgetFile)
import           Core.Util (shorten)
import           Class.Display (Display(..))
import qualified Model.Character as Character
import           Model.Character (Category(..))
import qualified Model.Class as Class
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import qualified Characters
import qualified Handler.Forum as Forum
import qualified Handler.Parse as Parse

-- | Renders the changelog.
getChangelogR :: Handler Html
getChangelogR = defaultLayout do
    setTitle "Naruto Unison: Changelog"
    $(widgetFile "tooltip/tooltip")
    $(widgetFile "changelog/changelog")
  where
    change = getChangelog True

-- | Renders the homepage of the website.
getHomeR :: Handler Html
getHomeR = do
    newsList <- runDB $ traverse withAuthor =<<
                selectList [] [Desc NewsDate, LimitTo 5]
    topics   <- Forum.selectWithAuthors [] [Desc TopicTime, LimitTo 10]
    citelink <- liftIO Forum.makeCitelink
    defaultLayout do
        setTitle "Naruto Unison"
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "home/home")
  where
    change = getChangelog False
    withAuthor (Entity _ newsVal) = do
        mAuthor <- get $ newsAuthor newsVal
        return case mAuthor of
            Nothing     -> (newsVal, "Unknown")
            Just author -> (newsVal, userName author)

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

news :: (News, Text) -> Widget
news (News{..}, authorName) = $(widgetFile "home/news")

getCharactersR :: Handler Html
getCharactersR = defaultLayout do
    setTitle "Characters"
    $(widgetFile "character/characters")
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
    Just char -> defaultLayout do
        let fmt  = Character.format char
        let path = shorten fmt
        setTitle . toHtml $ Character.format char
        $(widgetFile "character/character")
  where
    skillClasses skill =
        intercalate ", " $ display <$>
        filter Class.visible (toList $ Skill.classes skill)
