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

character :: Character -> Widget
character char = $(widgetFile "widgets/link/character")

cite :: IO (Cite Topic -> Widget)
cite = do
    timestamp <- makeTimestamp
    return \citation@Cite{..} -> $(widgetFile "widgets/link/cite")

skill :: Text -> Category -> Text -> Widget
skill charName category name = case Characters.lookup tagName of
      Nothing -> [whamlet|Error: character #{tagName} not found!|]
      Just char -> $(widgetFile "widgets/link/skill")
  where
    tagName = Character.identFrom category charName

topic :: Cite Topic -> Widget
topic Cite{..} = $(widgetFile "widgets/link/topic")

user :: User -> Widget
user User{..} = $(widgetFile "widgets/link/user")

makeTimestamp :: IO (UTCTime -> Widget)
makeTimestamp = do
    time   <- getCurrentTime
    zone   <- LocalTime.getCurrentTimeZone
    return . pureTimestamp zone $ utctDay time

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
