{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Fields for persistent types in 'Application.Model'.
module Application.Fields
    ( Privilege(..)
    -- * Forums
    , ForumCategory(..)
    , ForumBoard(..)
    , boardName, boardDesc, boardCategory
    ) where

import ClassyPrelude
import Yesod

import Text.Blaze (ToMarkup(..))
import Text.Read

data Privilege
    = Normal
    | Moderator
    | Admin
  deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)
derivePersistField "Privilege"

instance ToMarkup Privilege where
    toMarkup = toMarkup . show

data ForumCategory
    = Official
    | Community
    | Feedback
    | General
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

instance ToMarkup ForumCategory where
    toMarkup = toMarkup . show

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
    deriving (Bounded, Enum, Eq, Ord, Show, Read)
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
boardDesc board = "Sample description for " ++ boardName board ++ "."
