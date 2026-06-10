{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Miscellaneous website handlers.
module Handler.Link
  ( character
  , head
  , user
  , skill
  , staffTag
  ) where

import ClassyPrelude hiding (head)

import           Application.App (Route(..))
import qualified Application.App as App
import           Application.Model.User (User(User))
import qualified Application.Model.User
import           Application.Settings (widgetFile)
import qualified Game.Characters as Characters
import           Game.Model.Character (Category, Character(Character))
import qualified Game.Model.Character as Character
import qualified Game.Model.Skill as Skill

-- | Link to a character's detail page.
character :: Character -> App.Widget
character char@Character{category, name, ident} =
    $(widgetFile "widgets/link/character")

-- | Link to a character's detail page using their icon.
head :: Character -> App.Widget
head char@Character{ident} = $(widgetFile "widgets/link/head")
  where
    title = Character.format char

-- | Link to a character's skill. The character's name links to their detail
-- page, and the skill name shows skill details when hovered over.
skill :: Text -> Category -> Text -> App.Widget
skill charName category skillName = case Characters.lookup ident of
      Nothing -> error
        $ "Link.skill: character " ++ unpack ident ++ " not found"
      Just char@Character{skills}
        | any (any $ (==) skillName . Skill.name) skills ->
          $(widgetFile "widgets/link/skill")
      Just _ -> error
        $ "Link.skill: skill " ++ unpack skillName ++ " not found for "
          ++ unpack ident
  where
    ident = Character.identFrom category charName
    suffix :: Text
    suffix  = case charName of
        "Demon Brothers" -> "" -- to avoid "Demon Brothers's"
        _                -> "s"

-- | Link to a user's profile.
user :: User -> App.Widget
user User{name, privilege} = $(widgetFile "widgets/link/user")

-- | Appended to titles of posts and threads by staff.
staffTag :: Char
staffTag = '*'
