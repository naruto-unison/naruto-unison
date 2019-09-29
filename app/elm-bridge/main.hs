{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

import Prelude
import Data.Sequence (Seq)
import Data.List (dropWhileEnd)
import Data.List.NonEmpty (NonEmpty)

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
import qualified Data.Text as Text
import           Data.Text (Text)
import           Game.Model.Class (Class(..))
import           Game.Model.Chakra (Chakras(..))
import           Game.Model.Defense (Defense)
import           Game.Model.Face (Face(..))
import           Game.Model.Player (Player(..))
import           Game.Model.Slot (Slot)
import           Game.Model.Variant (Variant)
import           Application.Fields (Privilege(..))
import           Handler.Play.Queue (Failure(..))

import Game.Model.Internal hiding (Barrier(..), Effect(..), Ninja(..), Game(..))

-- From Model.GameInfo.ninjaToJSON
data Ninja = Ninja
    { slot      :: Slot
    , character :: Text
    , health    :: Int
    , defense   :: [Defense]
    , barrier   :: [Barrier]
    , statuses  :: [Status]
    , charges   :: Seq Int
    , cooldowns :: Seq Int
    , variants  :: Seq (NonEmpty Variant)
    , copies    :: Seq (Maybe Copy)
    , channels  :: [Channel]
    , traps     :: Seq Trap
    , face      :: [Face]
    , lastSkill :: Maybe Skill
    , skills    :: [Skill]
    }

-- From Model.GameInfo.gameToJSON
data Game = Game { chakra  :: (Chakras, Chakras)
                 , playing :: Player
                 , victor  :: [Player]
                 , ninjas  :: Seq Ninja
                 , targets :: [[[Slot]]]
                 }

-- From the ToJSON instance of GameInfo in Model.GameInfo
data GameInfo = GameInfo { opponent   :: User
                         , game       :: Game
                         , player     :: Player
                         }

-- From the ToJSON instance of Barrier in Model.Internal
data Barrier = Barrier { amount :: Int
                       , user   :: Slot
                       , name   :: Text
                       , dur    :: Int
                       }

-- From the ToJSON instance of Effect in Model.Effect
data Effect = Effect
    { desc    :: Text
    , helpful :: Bool
    , sticky  :: Bool
    , trap    :: Bool
    }

-- From the ToJSON instance of User in Application.Model
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

alterations :: ETypeDef -> ETypeDef
alterations = recAlterType typeAlterations

typeAlterations :: EType -> EType
typeAlterations t = case t of
    ETyApp (ETyCon (ETCon "Runnable")) x -> typeAlterations x
    ETyApp (ETyCon (ETCon "EnumSet")) x  -> ETyApp (ETyCon (ETCon "Set")) $
                                            typeAlterations x
    ETyCon (ETCon "Seq")       -> ETyCon (ETCon "List")
    ETyCon (ETCon "NonEmpty")  -> ETyCon (ETCon "List")
    ETyCon (ETCon "Slot")      -> ETyCon (ETCon "Int")
    ETyCon (ETCon "Class")     -> ETyCon (ETCon "String")
    ETyCon (ETCon "Duration")  -> ETyCon (ETCon "Int")
    ETyCon (ETCon "Trigger")   -> ETyCon (ETCon "String")
    ETyCon (ETCon "Varying")   -> ETyCon (ETCon "Int")
    ETyCon (ETCon "()")        -> ETyCon (ETCon "Unit") -- See elmUnitHandlers
    _                          -> defaultTypeAlterations t


-- | Aeson encodes () as a zero-length array.
elmUnitHandlers :: String
elmUnitHandlers = "type alias Unit = ()\n\
\jsonDecUnit : Json.Decode.Decoder ( () )\n\
\jsonDecUnit = Json.Decode.succeed ()\n\
\jsonEncUnit : () -> Value\n\
\jsonEncUnit = always <| Json.Encode.list (always Json.Encode.null) []"

deriveElmDef defaultOptions ''Failure

deriveElmDef defaultOptions ''User
deriveElmDef defaultOptions ''Privilege
deriveElmDef defaultOptions ''Barrier
deriveElmDef defaultOptions ''Bomb
deriveElmDef defaultOptions ''Category
deriveElmDef defaultOptions ''Channel
deriveElmDef defaultOptions ''Channeling
deriveElmDef defaultOptions ''Chakras
deriveElmDef defaultOptions ''Character
deriveElmDef defaultOptions ''Copy
deriveElmDef defaultOptions ''Copying
deriveElmDef defaultOptions ''Defense
deriveElmDef defaultOptions ''Direction
deriveElmDef defaultOptions ''Effect
deriveElmDef defaultOptions ''Face
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

trimAll :: String -> String
trimAll s = unlines $ dropWhileEnd Char.isSpace <$> lines s

main :: IO ()
main =
    writeFile "elm/src/Import/Model.elm" . trimAll $
    "module Import.Model exposing (..)\n\
\\n\
\import Json.Decode\n\
\import Json.Encode exposing (Value)\n\
\import Json.Helpers exposing (ObjectEncoding, encodeObject, encodeValue, decodeSumObjectWithSingleField, encodeSumObjectWithSingleField, decodeSumTwoElemArray, encodeSumTwoElementArray, encodeSumTaggedObject, decodeSumUnaries, decodeSumNullaries, decodeSumNullaryOrSingleField, decodeMap, encodeMap, jsonEncDict, jsonDecDict, encodeSet, decodeSet, maybeEncode, encodeSumUntagged, required, custom, fnullable, tuple2, tuple3)\n\
\import Dict exposing (Dict)\n\
\import Set exposing (Set)\n\
\\n\
\import Import.Decode exposing (decodeSumTaggedObject)\n\n"
    ++ elmUnitHandlers ++ "\n\n" ++
    makeModuleContentWithAlterations alterations
    [ DefineElm (Proxy :: Proxy Failure)
    , DefineElm (Proxy :: Proxy User)
    , DefineElm (Proxy :: Proxy Privilege)
    , DefineElm (Proxy :: Proxy Character)
    , DefineElm (Proxy :: Proxy Category)
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
