{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Fields for persistent data structures in 'Core.Model'.
module Core.Fields 
    ( Privilege(..)
    -- * Forums
    , ForumCategory(..)
    , ForumBoard(..)
    , getBoards
    , boardName, boardDesc
    ) where

import Preludesque
import GHC.Generics

import ClassyPrelude.Yesod (PathPiece(..), derivePersistField)
import Data.Aeson
import Data.Text  (Text, pack, unpack)
import Text.Read

import Calculus

data Privilege = Normal 
               | Moderator 
               | Admin 
  deriving (Enum, Ord, Bounded, Eq, Show, Read, Generic, FromJSON, ToJSON)
derivePersistField "Privilege"

data ForumCategory = Official
                   | Community
                   | Feedback
                   | General
                   deriving (Enum, Bounded, Eq, Show, Read)
                   
                -- Official
data ForumBoard = NewsAndAnnouncements
                | ForumInfo
                -- Community
                | IntroduceYourself
                -- Feedback
                | BugReports       
                | TechnicalSupport 
                | Suggestions
                -- General
                | OffTopic
                deriving (Enum, Ord, Bounded, Eq, Show, Read)
derivePersistField "ForumBoard"
instance PathPiece ForumBoard where
  toPathPiece = pack ∘ show
  fromPathPiece = readMaybe ∘ unpack

category ∷ ForumBoard → ForumCategory
category NewsAndAnnouncements = Official
category ForumInfo            = Official
category IntroduceYourself    = Community
category BugReports           = Feedback
category TechnicalSupport     = Feedback
category Suggestions          = Feedback
category OffTopic             = General

boardName ∷ ForumBoard → Text
boardName NewsAndAnnouncements = "News and Announcements"
boardName ForumInfo            = "Forum Info"
boardName IntroduceYourself    = "Introduce Yourself"
boardName BugReports           = "Bug Reports"
boardName TechnicalSupport     = "Technical Support"
boardName OffTopic             = "Off Topic"
boardName a = pack $ show a

boardDesc ∷ ForumBoard → Text
boardDesc = ("Sample description for " ⧺) ∘ (⧺ ".") ∘ boardName

getBoards ∷ ForumCategory → [ForumBoard]
getBoards cat = filter ((cat ≡) ∘ category) enums
