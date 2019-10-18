{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoBangPatterns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Miscellaneous website handlers.
module Handler.Link
  ( character
  , cite
  , topic
  , user
  , makeTimestamp
  , skill
  , staffTag
  ) where

import ClassyPrelude
import Yesod

import qualified Data.Time.Format as Format
import qualified Data.Time.LocalTime as LocalTime

import           Application.App (Route(..), Widget)
import           Application.Model (Cite(..), Topic(..), User(..))
import           Application.Settings (widgetFile)
import qualified Game.Characters as Characters
import           Game.Model.Character (Category, Character)
import qualified Game.Model.Character as Character
import qualified Game.Model.Skill as Skill

-- | Link to a character's detail page.
character :: Character -> Widget
character char = $(widgetFile "widgets/link/character")

-- | Link to a forum post or thread.
cite :: IO (Cite Topic -> Widget)
cite = do
    timestamp <- makeTimestamp
    return \citation@Cite{..} -> $(widgetFile "widgets/link/cite")

-- | Link to a character's skill. The character's name links to their detail
-- page, and the skill name shows skill details when hovered over.
skill :: Text -> Category -> Text -> Widget
skill charName category name = case Characters.lookup tagName of
      Nothing -> [whamlet|Error: character #{tagName} not found!|]
      Just char
        | any (any $ (== name) . Skill.name) $ Character.skills char ->
            $(widgetFile "widgets/link/skill")
        | otherwise ->
            [whamlet|Error: skill #{name} not found for character #{tagName}!|]
  where
    tagName = Character.identFrom category charName

-- | Link to a forum topic.
topic :: Cite Topic -> Widget
topic Cite{..} = $(widgetFile "widgets/link/topic")

-- | Link to a user's profile.
user :: User -> Widget
user User{..} = $(widgetFile "widgets/link/user")

-- | Current time widget.
makeTimestamp :: IO (UTCTime -> Widget)
makeTimestamp = do
    time   <- getCurrentTime
    zone   <- LocalTime.getCurrentTimeZone
    return . pureTimestamp zone $ utctDay time

-- | Parses the current time into a widget.
pureTimestamp :: LocalTime.TimeZone -> Day -> UTCTime -> Widget
pureTimestamp zone today unzoned = $(widgetFile "widgets/timestamp")
  where
    zoned = LocalTime.utcToLocalTime zone unzoned
    time  = Format.formatTime Format.defaultTimeLocale format zoned
    format
      | utctDay unzoned == today = "%l:%M %p"
      | otherwise                = "%m/%d/%y"

-- | Appended to titles of posts and threads by staff.
staffTag :: Char
staffTag = '*'
