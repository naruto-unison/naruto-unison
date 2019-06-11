{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Fields for persistent data structures in 'Core.Model'.
module Core.Fields
    ( Privilege(..)
    -- * Forums
    , ForumCategory(..)
    , ForumBoard(..)
    , boardName, boardDesc, boardCategory
    ) where

import ClassyPrelude.Yesod
import Text.Read


data Privilege
    = Normal
    | Moderator
    | Admin
  deriving (Enum, Ord, Bounded, Eq, Show, Read, Generic, FromJSON, ToJSON)
derivePersistField "Privilege"

data ForumCategory
    = Official
    | Community
    | Feedback
    | General
    deriving (Enum, Bounded, Eq, Show, Read)


data ForumBoard
    -- Official
    = NewsAndAnnouncements
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
  toPathPiece = tshow
  fromPathPiece = readMaybe . unpack

boardCategory :: ForumBoard -> ForumCategory
boardCategory NewsAndAnnouncements = Official
boardCategory ForumInfo            = Official
boardCategory IntroduceYourself    = Community
boardCategory BugReports           = Feedback
boardCategory TechnicalSupport     = Feedback
boardCategory Suggestions          = Feedback
boardCategory OffTopic             = General

boardName :: ForumBoard -> Text
boardName NewsAndAnnouncements = "News and Announcements"
boardName ForumInfo            = "Forum Info"
boardName IntroduceYourself    = "Introduce Yourself"
boardName BugReports           = "Bug Reports"
boardName TechnicalSupport     = "Technical Support"
boardName OffTopic             = "Off Topic"
boardName x = tshow x

boardDesc :: ForumBoard -> Text
boardDesc = ("Sample description for " ++) . (++ ".") . boardName
