{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Miscellaneous website handlers.
module Handler.Link
  ( character
  , cite
  , head
  , topic
  , user
  , makeTimestamp
  , skill
  , staffTag
  ) where

import ClassyPrelude hiding (head)

import qualified Data.Time.Format as Format
import qualified Data.Time.LocalTime as LocalTime

import           Application.App (Route(..))
import qualified Application.App as App
import           Application.Model (Cite(..), ForumTopic(..), User(..))
import           Application.Settings (widgetFile)
import qualified Class.Labeled as Labeled
import qualified Game.Characters as Characters
import           Game.Model.Character (Category, Character)
import qualified Game.Model.Character as Character

-- | Link to a character's detail page.
character :: Character -> App.Widget
character char = $(widgetFile "widgets/link/character")

-- | Link to a forum post or thread.
cite :: IO (Cite ForumTopic -> App.Widget)
cite = do
    timestamp <- makeTimestamp
    return \citation@Cite{citeLatest, citeVal} ->
        $(widgetFile "widgets/link/cite")

-- | Link to a character's detail page using their icon.
head :: Character -> App.Widget
head char = $(widgetFile "widgets/link/head")

-- | Link to a character's skill. The character's name links to their detail
-- page, and the skill name shows skill details when hovered over.
skill :: Text -> Category -> Text -> App.Widget
skill charName category name = case Characters.lookup tagName of
      Nothing -> error
        $ "Link.skill: character " ++ unpack tagName ++ " not found"
      Just char | any (any $ Labeled.named name) $ Character.skills char ->
        $(widgetFile "widgets/link/skill")
      Just _ -> error
        $ "Link.skill: skill " ++ unpack name ++ " not found for "
          ++ unpack tagName
  where
    tagName = Character.identFrom category charName
    suffix :: Text
    suffix  = case charName of
        "Demon Brothers" -> "" -- to avoid "Demon Brothers's"
        _                -> "s"

-- | Link to a forum topic.
topic :: Cite ForumTopic -> App.Widget
topic Cite{citeAuthor, citeKey, citeVal} = $(widgetFile "widgets/link/topic")

-- | Link to a user's profile.
user :: User -> App.Widget
user User{userName, userPrivilege} = $(widgetFile "widgets/link/user")

-- | Current time widget.
makeTimestamp :: IO (UTCTime -> App.Widget)
makeTimestamp = pureTimestamp <$> LocalTime.getCurrentTimeZone

-- | Parses the current time into a widget.
pureTimestamp :: LocalTime.TimeZone -> UTCTime -> App.Widget
pureTimestamp zone unzoned = $(widgetFile "widgets/timestamp")
  where
    zoned = LocalTime.utcToLocalTime zone unzoned
    time  = Format.formatTime Format.defaultTimeLocale format zoned
    format = "%l:%M %p %m/%d/%y"

-- | Appended to titles of posts and threads by staff.
staffTag :: Char
staffTag = '*'
