{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-top-binds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

import ClassyPrelude

import Data.Aeson (defaultOptions)
import Data.Char (isSpace)
import Data.Enum.Set (EnumSet)
import Data.List (dropWhileEnd)
import Data.Proxy
import Elm.Derive hiding (defaultOptions)
import Elm.Module
import Elm.TyRep

import Application.Model.User (Privilege(..))
import Game.Model.Chakras (Chakras)
import Game.Model.Channel (Channel, Channeling)
import Game.Model.Character (Category, Character)
import Game.Model.Class (Class)
import Game.Model.Copy (Copy)
import Game.Model.Destructible (Destructible)
import Game.Model.Duration (Duration)
import Game.Model.Face (Face)
import Game.Model.Player (Player(..))
import Game.Model.Runnable (Runnable)
import Game.Model.Skill (Target)
import Game.Model.Slot (Slot)
import Game.Model.Status (Status, Bomb(..))
import Game.Model.Trap (Direction)
import Game.Model.Trigger (Trigger)
import Handler.Client (ObjectiveProgress)
import Handler.Client.Message (Failure(..), Message(..))
import Handler.Client.Reward (Reward)
import Handler.Play.Turn (Turn)
import Handler.Play.War (War(..))

-- From Game.Model.Internal
data Ninja = Ninja
    { slot      :: Slot
    , character :: Text
    , health    :: Int
    , cooldowns :: HashMap Text Int
    , charges   :: HashMap Text Int
    , defense   :: [Destructible]
    , barrier   :: [Destructible]
    , statuses  :: [Status]
    , copies    :: Vector (Maybe Copy)
    , channels  :: [Channel]
    , traps     :: [Trap]
    , face      :: Maybe Face
    , skills    :: [Skill]
    }

-- From Game.Model.Internal
data Skill = Skill
    { name      :: Text
    , desc      :: Text
    , classes   :: EnumSet Class
    , cost      :: Chakras
    , cooldown  :: Int
    , charges   :: Int
    , dur       :: Channeling
    , start     :: [Runnable Target]
    , always    :: [Runnable Target]
    , effects   :: [Runnable Target]
    , end       :: [Runnable Target]
    , owner     :: Slot
    }

-- From Game.Model.Internal
data Trap = Trap
    { direction :: Direction
    , trigger   :: Trigger
    , name      :: Text
    , skill     :: Skill
    , user      :: Slot
    , classes   :: EnumSet Class
    , dur       :: Duration
    }

-- From the ToJSON instance of GameInfo in Model.GameInfo
data GameInfo = GameInfo
    { opponent   :: User
    , turn       :: Turn
    , player     :: Player
    , war        :: Maybe War
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
data User = User
    { privilege  :: Privilege
    , name       :: Text
    , avatar     :: Text
    , background :: Maybe Text
    , xp         :: Int
    , wins       :: Int
    , losses     :: Int
    , streak     :: Int
    , record     :: Int
    , clan       :: Maybe Text
    , condense   :: Bool
    , dna        :: Int
    }

alterations :: ETypeDef -> ETypeDef
alterations = recAlterType alter
  where
    alter (ETyApp (ETyCon (ETCon "NonNull"))  ty) = alter ty
    alter (ETyApp (ETyCon (ETCon "Runnable")) ty) = alter ty
    alter (ETyCon (ETCon "Class"))    = ETyCon (ETCon "String")
    alter (ETyCon (ETCon "Duration")) = ETyApp (ETyCon (ETCon "Maybe"))
                                      $ ETyCon (ETCon "Int")
    alter (ETyCon (ETCon "EnumSet"))  = ETyCon (ETCon "Set")
    alter (ETyCon (ETCon "Group"))    = ETyCon (ETCon "String")
    alter (ETyCon (ETCon "Slot"))     = ETyCon (ETCon "Int")
    alter (ETyCon (ETCon "Trigger"))  = ETyCon (ETCon "String")
    alter ty                          = defaultTypeAlterations ty

deriveElmDef defaultOptions ''Message
deriveElmDef defaultOptions ''Failure

deriveElmDef defaultOptions ''User
deriveElmDef defaultOptions ''Privilege
deriveElmDef defaultOptions ''Bomb
deriveElmDef defaultOptions ''Category
deriveElmDef defaultOptions ''Channel
deriveElmDef defaultOptions ''Channeling
deriveElmDef defaultOptions ''Chakras
deriveElmDef defaultOptions ''Character
deriveElmDef defaultOptions ''Copy
deriveElmDef defaultOptions ''Destructible
deriveElmDef defaultOptions ''Direction
deriveElmDef defaultOptions ''Effect
deriveElmDef defaultOptions ''Face
deriveElmDef defaultOptions ''GameInfo
deriveElmDef defaultOptions ''Ninja
deriveElmDef defaultOptions ''ObjectiveProgress
deriveElmDef defaultOptions ''Player
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
main = writeFile "elm/src/Import/Model.elm" . fromString . trimAll $ "module Import.Model exposing (..)\n\
\\n\
\import Dict exposing (Dict)\n\
\import Json.Decode\n\
\import Json.Encode exposing (Value)\n\
\import Json.Helpers exposing (..)\n\
\import Set exposing (Set)\n\n" ++
    makeModuleContentWithAlterations alterations
    [ DefineElm (Proxy :: Proxy Bomb)
    , DefineElm (Proxy :: Proxy Category)
    , DefineElm (Proxy :: Proxy Chakras)
    , DefineElm (Proxy :: Proxy Channel)
    , DefineElm (Proxy :: Proxy Channeling)
    , DefineElm (Proxy :: Proxy Character)
    , DefineElm (Proxy :: Proxy Copy)
    , DefineElm (Proxy :: Proxy Destructible)
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
    , DefineElm (Proxy :: Proxy Reward)
    , DefineElm (Proxy :: Proxy Skill)
    , DefineElm (Proxy :: Proxy Status)
    , DefineElm (Proxy :: Proxy Target)
    , DefineElm (Proxy :: Proxy Trap)
    , DefineElm (Proxy :: Proxy Turn)
    , DefineElm (Proxy :: Proxy User)
    , DefineElm (Proxy :: Proxy War) -- hehehe
    ]
