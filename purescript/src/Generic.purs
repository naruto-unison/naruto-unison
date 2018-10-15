-- | This package contains all typeclasses and functions necessary for deriving
-- | `Decode` generically for any data structure.
-- | See `Foreign.Class`.

module Generic (module Import, decodeObj, decodeEnum) where

import Data.Generic.Rep (class Generic) as Import
import Data.Generic.Rep.Show (genericShow) as Import
import Data.Newtype (class Newtype) as Import
import Foreign.Class (class Decode, decode) as Import

import Data.Generic.Rep (class Generic)
import Foreign.Generic.Class (class GenericDecode)
import Foreign.Generic (genericDecode, defaultOptions)
import Foreign (Foreign, F)
import Foreign.Generic.EnumEncoding (class GenericDecodeEnum, genericDecodeEnum, defaultGenericEnumOptions)

decodeObj :: ∀ a rep. Generic a rep => GenericDecode rep => Foreign -> F a
decodeObj = genericDecode
            (defaultOptions { unwrapSingleConstructors = true })

decodeEnum :: ∀ a rep. Generic a rep => GenericDecodeEnum rep => Foreign -> F a
decodeEnum = genericDecodeEnum defaultGenericEnumOptions
