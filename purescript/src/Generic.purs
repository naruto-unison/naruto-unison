-- | This package contains all typeclasses and functions necessary for deriving
-- | `BoundedEnum` generically for any data structure.
-- | See `Data.Generic.Rep.Enum`.

module Generic (module Import) where

import Data.Bounded (class Bounded) as Import
import Data.Enum (class BoundedEnum, class Enum) as Import
import Data.Generic.Rep (class Generic) as Import
import Data.Generic.Rep.Bounded (genericBottom, genericTop) as Import
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum) as Import
import Data.Generic.Rep.Show (genericShow) as Import
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson) as Import
import Data.Newtype (class Newtype) as Import
import Foreign.Class (class Decode, decode) as Import
import Foreign.Generic (genericDecode, defaultOptions) as Import
import Foreign.Generic.Types (Options) as Import
