{-# LANGUAGE TemplateHaskell #-}
import Elm.Derive
import Elm.Module
import Elm.TyRep

import Prelude
import Data.Proxy

import ElmDefs
import qualified Class.Classed as Classed
import           Class.Classed (Classed)
import qualified Class.Parity as Parity
import           Class.Parity (Parity)
import qualified Class.Labeled as Labeled
import           Class.Labeled (Labeled)
import           Class.TurnBased (TurnBased(..))
import qualified Data.Char as Char
import           Data.Text as Text
import           Model.Class (Class(..))
import           Model.Chakra (Chakras(..))
import           Model.Face (Face(..))
import           Model.Player (Player(..))
import           Model.Slot (Slot)
import           Core.Fields (Privilege(..))

data User = User { name       :: Text
                 , avatar     :: Text
                 , clan       :: Maybe Text
                 , xp         :: Int
                 , wins       :: Int
                 , losses     :: Int
                 , streak     :: Int
                 , background :: Maybe Text
                 , privilege  :: Privilege
                 , condense   :: Bool
                 }

data GameInfo = GameInfo { opponent   :: User
                         , left       :: Int
                         , game       :: Game
                         , characters :: [Character]
                         , player     :: Player
                         }

typeAlterations :: EType -> EType
typeAlterations t = case t of
    ETyCon (ETCon "Seq")      -> ETyCon (ETCon "List")
    ETyCon (ETCon "NonEmpty") -> ETyCon (ETCon "List")
    ETyCon (ETCon "Slot")     -> ETyCon (ETCon "Int")
    ETyCon (ETCon "Class")    -> ETyCon (ETCon "String")
    ETyCon (ETCon "Duration") -> ETyCon (ETCon "Int")
    ETyCon (ETCon "Trigger")  -> ETyCon (ETCon "String")
    _                         -> defaultTypeAlterations t

alterations :: ETypeDef -> ETypeDef
alterations = recAlterType typeAlterations

deriveElmDef opts ''User
deriveElmDef opts ''Privilege
deriveElmDef opts ''Barrier
deriveElmDef opts ''Bomb
deriveElmDef opts ''Category
deriveElmDef opts ''Channel
deriveElmDef opts ''Channeling
deriveElmDef opts ''Chakras
deriveElmDef opts ''ChannelTag
deriveElmDef opts ''Character
deriveElmDef opts ''Context
deriveElmDef opts ''Copy
deriveElmDef opts ''Copying
deriveElmDef opts ''Defense
deriveElmDef opts ''Direction
deriveElmDef opts ''Effect
deriveElmDef opts ''Face
deriveElmDef opts ''Game
deriveElmDef opts ''GameInfo
deriveElmDef opts ''Ninja
deriveElmDef opts ''Player
deriveElmDef opts ''Requirement
deriveElmDef opts ''Skill
deriveElmDef opts ''Status
deriveElmDef opts ''Target
deriveElmDef opts ''Trap
deriveElmDef opts ''Variant

main :: IO ()
main =
    writeFile "elm/src/Model.elm" $
    "module Model exposing (..)\n\
\\n\
\import Json.Decode\n\
\import Json.Encode exposing (Value)\n\
\import Json.Helpers exposing (ObjectEncoding, encodeObject, encodeValue, decodeSumObjectWithSingleField, encodeSumObjectWithSingleField, decodeSumTwoElemArray, encodeSumTwoElementArray, encodeSumTaggedObject, decodeSumUnaries, decodeSumNullaries, decodeSumNullaryOrSingleField, decodeMap, encodeMap, jsonEncDict, jsonDecDict, encodeSet, decodeSet, maybeEncode, encodeSumUntagged, required, custom, fnullable, tuple2, tuple3)\n\
\import Dict exposing (Dict)\n\
\import Set exposing (Set)\n\
\\n\
\import Decode exposing (decodeSumTaggedObject)\n\
\\n\
\" ++
    makeModuleContentWithAlterations alterations
    [ DefineElm (Proxy :: Proxy User)
    , DefineElm (Proxy :: Proxy Privilege)
    , DefineElm (Proxy :: Proxy Character)
    , DefineElm (Proxy :: Proxy Category)
    , DefineElm (Proxy :: Proxy Context)
    , DefineElm (Proxy :: Proxy Game)
    , DefineElm (Proxy :: Proxy GameInfo)
    , DefineElm (Proxy :: Proxy Chakras)
    , DefineElm (Proxy :: Proxy Player)
    , DefineElm (Proxy :: Proxy Ninja)
    , DefineElm (Proxy :: Proxy Skill)
    , DefineElm (Proxy :: Proxy Requirement)
    , DefineElm (Proxy :: Proxy Target)
    , DefineElm (Proxy :: Proxy Variant)
    , DefineElm (Proxy :: Proxy Channel)
    , DefineElm (Proxy :: Proxy Channeling)
    , DefineElm (Proxy :: Proxy ChannelTag)
    , DefineElm (Proxy :: Proxy Copy)
    , DefineElm (Proxy :: Proxy Copying)
    , DefineElm (Proxy :: Proxy Effect)
    , DefineElm (Proxy :: Proxy Status)
    , DefineElm (Proxy :: Proxy Bomb)
    , DefineElm (Proxy :: Proxy Face)
    , DefineElm (Proxy :: Proxy Barrier)
    , DefineElm (Proxy :: Proxy Defense)
    , DefineElm (Proxy :: Proxy Trap)
    , DefineElm (Proxy :: Proxy Direction)
    ]
