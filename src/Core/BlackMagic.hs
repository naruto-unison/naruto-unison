{-# OPTIONS_HADDOCK hide #-}
-- | Abandon hope, all ye who enter here.
module Core.BlackMagic () where

import Prelude
import Yesod (ToJSON, toJSON)

-- * Function ignoring for derives
instance Eq (a -> b) where 
    (==) = const $ const True
instance ToJSON (a -> b) where 
    toJSON = const $ toJSON (Nothing :: Maybe ())
