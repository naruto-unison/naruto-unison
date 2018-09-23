-- | This package contains all typeclasses and functions necessary for deriving
-- | `Decode` generically for any data structure.
-- | See `Foreign.Class`.

module Generic (module Import) where

import Data.Generic.Rep (class Generic) as Import
import Data.Generic.Rep.Show (genericShow) as Import
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson) as Import
import Data.Newtype (class Newtype) as Import
import Foreign.Class (class Decode, decode) as Import
import Foreign.Generic (genericDecode, defaultOptions) as Import
import Foreign.Generic.Types (Options) as Import
