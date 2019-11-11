{-# LANGUAGE TemplateHaskell #-}

-- | Fields for persistent types in 'Application.Model'.
module Handler.Forum.Markdown
    ( Markdown(..)
    , quote
    ) where

import ClassyPrelude

import qualified CMark
import           Data.Aeson (FromJSON, ToJSON)
import           Database.Persist.Sql (PersistField(..), PersistFieldSql(..))
import           Text.Blaze (ToMarkup(..))

newtype Markdown = Markdown Text deriving ( Eq, Ord, Show, Read, IsString
                                          , FromJSON, ToJSON
                                          , Semigroup, Monoid
                                          , PersistField, PersistFieldSql
                                          )
instance ToMarkup Markdown where
    toMarkup (Markdown x) =
        preEscapedToMarkup $ CMark.commonmarkToHtml
        [CMark.optNormalize, CMark.optHardBreaks, CMark.optSmart, CMark.optSafe]
        x
    {-# INLINE toMarkup #-}

quote :: Markdown -> Markdown
quote (Markdown x) = Markdown $ "> " ++ replaceSeq "\n" "\n> " x
