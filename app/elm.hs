{-# LANGUAGE TemplateHaskell #-}

import Prelude
import Data.Proxy
import Elm.Module
import Elm.TyRep

import ElmDerive

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

import Model.Internal hiding (Effect(..))

data Effect = Effect 
    { desc    :: Text 
    , helpful :: Bool 
    , sticky  :: Bool 
    , trap    :: Bool 
    } 
 
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

deriveElmDef defaultOptions ''User
deriveElmDef defaultOptions ''Privilege
deriveElmDef defaultOptions ''Barrier
deriveElmDef defaultOptions ''Bomb
deriveElmDef defaultOptions ''Category
deriveElmDef defaultOptions ''Channel
deriveElmDef defaultOptions ''Channeling
deriveElmDef defaultOptions ''Chakras
deriveElmDef defaultOptions ''ChannelTag
deriveElmDef defaultOptions ''Character
deriveElmDef defaultOptions ''Copy
deriveElmDef defaultOptions ''Copying
deriveElmDef defaultOptions ''Defense
deriveElmDef defaultOptions ''Delay
deriveElmDef defaultOptions ''Direction
deriveElmDef defaultOptions ''Effect
deriveElmDef defaultOptions ''Face
deriveElmDef defaultOptions ''Flag
deriveElmDef defaultOptions ''Game
deriveElmDef defaultOptions ''GameInfo
deriveElmDef defaultOptions ''Ninja
deriveElmDef defaultOptions ''Player
deriveElmDef defaultOptions ''Requirement
deriveElmDef defaultOptions ''Skill
deriveElmDef defaultOptions ''Status
deriveElmDef defaultOptions ''Target
deriveElmDef defaultOptions ''Trap
deriveElmDef defaultOptions ''Variant

main :: IO ()
main =
    writeFile "elm/src/Import/Model.elm" $
    "module Import.Model exposing (..)\n\
\\n\
\import Json.Decode\n\
\import Json.Encode exposing (Value)\n\
\import Json.Helpers exposing (ObjectEncoding, encodeObject, encodeValue, decodeSumObjectWithSingleField, encodeSumObjectWithSingleField, decodeSumTwoElemArray, encodeSumTwoElementArray, encodeSumTaggedObject, decodeSumUnaries, decodeSumNullaries, decodeSumNullaryOrSingleField, decodeMap, encodeMap, jsonEncDict, jsonDecDict, encodeSet, decodeSet, maybeEncode, encodeSumUntagged, required, custom, fnullable, tuple2, tuple3)\n\
\import Dict exposing (Dict)\n\
\import Set exposing (Set)\n\
\\n\
\import Import.Decode exposing (decodeSumTaggedObject)\n\
\\n\
\" ++
    makeModuleContentWithAlterations alterations
    [ DefineElm (Proxy :: Proxy User)
    , DefineElm (Proxy :: Proxy Privilege)
    , DefineElm (Proxy :: Proxy Character)
    , DefineElm (Proxy :: Proxy Category)
    , DefineElm (Proxy :: Proxy Game)
    , DefineElm (Proxy :: Proxy Delay)
    , DefineElm (Proxy :: Proxy GameInfo)
    , DefineElm (Proxy :: Proxy Chakras)
    , DefineElm (Proxy :: Proxy Player)
    , DefineElm (Proxy :: Proxy Ninja)
    , DefineElm (Proxy :: Proxy Flag)
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
