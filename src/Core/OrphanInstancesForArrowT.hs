{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

module Core.OrphanInstancesForArrowT () where

import Prelude (($), Bool(..), Eq(..), Maybe(..), const)
import Data.Aeson (ToJSON(..))

instance Eq (a -> b) where
    (==) = const $ const True

instance ToJSON (a -> b) where
    toJSON = const $ toJSON (Nothing :: Maybe ())
