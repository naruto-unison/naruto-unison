{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-top-binds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

import Prelude
import Data.Sequence (Seq)
import Data.List (dropWhileEnd)

import Data.Proxy
import Elm.Module
import Elm.TyRep

import ElmDerive

import Data.Char (isSpace)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Game.Model.Chakra (Chakras(..))
import Game.Model.Defense (Defense)
import Game.Model.Player (Player(..))
import Game.Model.Slot (Slot)
import Application.Fields (Privilege(..))
import Handler.Client (ObjectiveProgress(..))
import Handler.Client.Reward (Reward(..))
import Handler.Play.Queue (Failure(..))
import Handler.Play.Turn (Turn(..))
import Handler.Play.War (War(..))
import Handler.Play (Message(..))
import OrphanInstances.Ninja (Face(..))

import Game.Model.Internal hiding (Barrier(..), Ninja(..))

-- From Model.GameInfo.ninjaToJSON
data Ninja = Ninja
    { slot      :: Slot
    , character :: Text
    , health    :: Int
    , cooldowns :: Map Text Int
    , charges   :: Map Text Int
    , defense   :: [Defense]
    , barrier   :: [Barrier]
    , statuses  :: [Status]
    , copies    :: Seq (Maybe Copy)
    , channels  :: [Channel]
    , traps     :: Seq Trap
    , face      :: Maybe Face
    , lastSkill :: Maybe Skill
    , skills    :: [Skill]
    }

-- From the ToJSON instance of GameInfo in Model.GameInfo
data GameInfo = GameInfo { opponent   :: User
                         , turn       :: Turn
                         , player     :: Player
                         , war        :: Maybe War
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
    , visible :: Bool
    , trap    :: Bool
    , slot    :: Maybe Slot
    }

-- From the ToJSON instance of User in Application.Model
data User = User { privilege  :: Privilege
                 , name       :: Text
                 , avatar     :: Text
                 , background :: Maybe Text
                 , xp         :: Int
                 , wins       :: Int
                 , losses     :: Int
                 , streak     :: Int
                 , record     :: Int
                 , clan       :: Maybe Text
                 , muted      :: Bool
                 , condense   :: Bool
                 , dna        :: Int
                 }

alterations :: ETypeDef -> ETypeDef
alterations = recAlterType typeAlterations

typeAlterations :: EType -> EType
typeAlterations t = case t of
    ETyApp (ETyCon (ETCon "Runnable")) x -> typeAlterations x
    ETyApp (ETyCon (ETCon "EnumSet")) x  -> ETyApp (ETyCon (ETCon "Set")) $
                                            typeAlterations x
    ETyCon (ETCon "Class")     -> ETyCon (ETCon "String")
    ETyCon (ETCon "Duration")  -> ETyCon (ETCon "Int")
    ETyCon (ETCon "Group")     -> ETyCon (ETCon "String")
    ETyCon (ETCon "NonEmpty")  -> ETyCon (ETCon "List")
    ETyCon (ETCon "Seq")       -> ETyCon (ETCon "List")
    ETyCon (ETCon "Slot")      -> ETyCon (ETCon "Int")
    ETyCon (ETCon "Trigger")   -> ETyCon (ETCon "String")
    ETyCon (ETCon "Varying")   -> ETyCon (ETCon "Int")
    _                          -> defaultTypeAlterations t

deriveElmDef defaultOptions ''Message
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
deriveElmDef defaultOptions ''Defense
deriveElmDef defaultOptions ''Direction
deriveElmDef defaultOptions ''Effect
deriveElmDef defaultOptions ''Face
deriveElmDef defaultOptions ''GameInfo
deriveElmDef defaultOptions ''Ninja
deriveElmDef defaultOptions ''ObjectiveProgress
deriveElmDef defaultOptions ''Player
deriveElmDef defaultOptions ''Requirement
deriveElmDef defaultOptions ''Reward
deriveElmDef defaultOptions ''Skill
deriveElmDef defaultOptions ''Status
deriveElmDef defaultOptions ''Target
deriveElmDef defaultOptions ''Trap
deriveElmDef defaultOptions ''Turn
deriveElmDef defaultOptions ''War

trimAll :: String -> String
trimAll s = unlines $ dropWhileEnd isSpace <$> lines s

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
\import Import.Decode exposing (decodeSumTaggedObject)\n\n" ++
    makeModuleContentWithAlterations alterations
    [ DefineElm (Proxy :: Proxy Barrier)
    , DefineElm (Proxy :: Proxy Bomb)
    , DefineElm (Proxy :: Proxy Category)
    , DefineElm (Proxy :: Proxy Chakras)
    , DefineElm (Proxy :: Proxy Channel)
    , DefineElm (Proxy :: Proxy Channeling)
    , DefineElm (Proxy :: Proxy Character)
    , DefineElm (Proxy :: Proxy Copy)
    , DefineElm (Proxy :: Proxy Defense)
    , DefineElm (Proxy :: Proxy Direction)
    , DefineElm (Proxy :: Proxy Effect)
    , DefineElm (Proxy :: Proxy Face)
    , DefineElm (Proxy :: Proxy Failure)
    , DefineElm (Proxy :: Proxy GameInfo)
    , DefineElm (Proxy :: Proxy Message)
    , DefineElm (Proxy :: Proxy Ninja)
    , DefineElm (Proxy :: Proxy ObjectiveProgress)
    , DefineElm (Proxy :: Proxy Player)
    , DefineElm (Proxy :: Proxy Privilege)
    , DefineElm (Proxy :: Proxy Requirement)
    , DefineElm (Proxy :: Proxy Reward)
    , DefineElm (Proxy :: Proxy Skill)
    , DefineElm (Proxy :: Proxy Status)
    , DefineElm (Proxy :: Proxy Target)
    , DefineElm (Proxy :: Proxy Trap)
    , DefineElm (Proxy :: Proxy Turn)
    , DefineElm (Proxy :: Proxy User)
    , DefineElm (Proxy :: Proxy War) -- hehehe
    ]
