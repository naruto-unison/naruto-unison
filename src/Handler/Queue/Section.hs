module Handler.Queue.Section (Section(..)) where

import ClassyPrelude

import           Class.Parse (Parse)
import qualified Class.Parse as Parse

-- | Queue section.
data Section
    = Quick
    | Private
    deriving (Bounded, Enum, Eq, Ord, Show)

instance Parse Section where
    parser = Parse.choice
        [ Parse.string "private" $> Private
        , Parse.string "quick"   $> Quick
        ]
