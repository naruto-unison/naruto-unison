{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | These handlers embed files in the executable at compile time to avoid runtime dependencies and improve efficiency.
module Handler.Embed
  ( getFaviconR
  , getRobotsR
  ) where

import ClassyPrelude hiding (Handler)
import Yesod

import qualified Data.FileEmbed as FileEmbed

import Core.App (Handler)

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon" $
                    toContent $(FileEmbed.embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain $
                toContent $(FileEmbed.embedFile "config/robots.txt")
