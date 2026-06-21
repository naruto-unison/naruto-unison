module Application.Model.Unlocked
    ( Unlocked(..), UnlockedId
    , reanimated
    ) where

import ClassyPrelude

import Application.Model.Internal (Unlocked(..), UnlockedId)

reanimated :: Text -> Bool
reanimated = ("-(r)" `isSuffixOf`)
