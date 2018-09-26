{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- | Interface for the PureScript game client.
module Handler.Site
  ( getChangelogR
  , getHomeR
  ) where

import StandardLibrary

import qualified Data.List.NonEmpty  as NonEmpty

import qualified Handler.Forum as Forum

import Calculus
import Core.Import
import Game.Structure
import Game.Characters

(!) :: Text -> Text -> Html
usr ! l = [shamlet| $newline never
<a .skill data-usr=#{usr}>#{l}|]

data LogType = Added | New | Rework | Change
data CharacterType = O | R | S

logLabel :: Bool -> LogType -> Text
logLabel True  Added  = "Character added:"
logLabel False Added  = "Added:"
logLabel True  New    = "New character:"
logLabel False New    = "New:"
logLabel True  Rework = "Character rework:"
logLabel False Rework = "Rework:"
logLabel True  Change = "Character update:"
logLabel False Change = "Update:"

getChangelog :: Bool -> LogType -> Text -> CharacterType -> Html
getChangelog long logType name characterType = case lookup tagName cs of
    Nothing -> [shamlet|Error: character #{tagName} not found!|]
    Just Character{..} -> [shamlet|
$if not long
  <img data-name=#{tagName} .char.head src="/img/ninja/#{shorten(tagName)}/icon.jpg">
<li>
  #{change logType} 
  <a .name data-name=#{tagName}>#{display characterType}
  <ul>
    $if long
      <img data-name=#{tagName} .char.head src="/img/ninja/#{shorten(tagName)}/icon.jpg">
    $forall skills <- NonEmpty.take 4 characterSkills
      <li>
        $forall skill <- separate skills
          <a .skill data-name=#{tagName}>#{label skill}
|]
  where 
    separate      = nubBy (eqs label) . toList
    tag O         = name
    tag R         = name ++ " (R)"
    tag S         = name ++ " (S)"
    tagName       = tag characterType
    change        = logLabel long
    display O     = [shamlet|#{name}|]
    display R     = [shamlet|#{name}
<a .minor data-name=#{tagName}>ℝ|]
    display S     = [shamlet|#{name}
<a .minor data-name=#{tagName}>𝕊|]

s :: Html
s = [shamlet|<a .minor>𝕊|]

-- * HANDLERS

-- | Renders the changelog.
getChangelogR :: Handler Html
getChangelogR = defaultLayout $ do
    setTitle "Naruto Unison: Changelog"
    $(widgetFile "tooltip/tooltip")
    $(widgetFile "changelog/changelog")
  where 
    changelog = getChangelog True 

-- | Renders the main site.
getHomeR :: Handler Html
getHomeR = do
    topics   <- Forum.selectWithAuthors [] [Desc TopicTime, LimitTo 10]
    citelink <- liftIO Forum.makeCitelink
    defaultLayout $ do
        setTitle "Naruto Unison"
        $(widgetFile "tooltip/tooltip")
        $(widgetFile "home/home")
  where 
    changelog = getChangelog False
