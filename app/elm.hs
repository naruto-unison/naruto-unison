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
import           Model.Class (Class(..))
import           Model.Chakra (Chakras(..))
import           Model.Face (Face(..))
import           Model.Player (Player)
import           Model.Slot (Slot)

typeAlterations :: EType -> EType
typeAlterations t = case t of
    ETyCon (ETCon "Seq")      -> ETyCon (ETCon "List")
    ETyCon (ETCon "NonEmpty") -> ETyCon (ETCon "List")
    ETyCon (ETCon "Slot")     -> ETyCon (ETCon "Int")
    ETyCon (ETCon "Class")    -> ETyCon (ETCon "String")
    ETyCon (ETCon "Effect")   -> ETyCon (ETCon "String")
    _                         -> defaultTypeAlterations t

alterations :: ETypeDef -> ETypeDef
alterations = recAlterType typeAlterations

deriveElmDef defaultOptions ''Barrier
deriveElmDef defaultOptions ''Bomb
deriveElmDef defaultOptions ''Category
deriveElmDef defaultOptions ''Channel
deriveElmDef defaultOptions ''Channeling
deriveElmDef defaultOptions ''Chakras
deriveElmDef defaultOptions ''ChannelTag
deriveElmDef defaultOptions ''Character
deriveElmDef defaultOptions ''Context
deriveElmDef defaultOptions ''Copy
deriveElmDef defaultOptions ''Copying
deriveElmDef defaultOptions ''Defense
deriveElmDef defaultOptions ''Direction
deriveElmDef defaultOptions ''Requirement
deriveElmDef defaultOptions ''Face
deriveElmDef defaultOptions ''Skill
deriveElmDef defaultOptions ''Status
deriveElmDef defaultOptions ''Target
deriveElmDef defaultOptions ''Trap
deriveElmDef defaultOptions ''Trigger
deriveElmDef defaultOptions ''Variant

main :: IO ()
main =
    putStrLn $ makeModuleContentWithAlterations alterations
    [ DefineElm (Proxy :: Proxy Barrier)
    , DefineElm (Proxy :: Proxy Bomb)
    --, DefineElm (Proxy :: Proxy Effect)
    , DefineElm (Proxy :: Proxy Category)
    , DefineElm (Proxy :: Proxy Chakras)
    , DefineElm (Proxy :: Proxy Channel)
    , DefineElm (Proxy :: Proxy Channeling)
    , DefineElm (Proxy :: Proxy ChannelTag)
    , DefineElm (Proxy :: Proxy Character)
    , DefineElm (Proxy :: Proxy Context)
    , DefineElm (Proxy :: Proxy Copy)
    , DefineElm (Proxy :: Proxy Copying)
    , DefineElm (Proxy :: Proxy Defense)
    , DefineElm (Proxy :: Proxy Direction)
    , DefineElm (Proxy :: Proxy Face)
    , DefineElm (Proxy :: Proxy Requirement)
    , DefineElm (Proxy :: Proxy Skill)
    , DefineElm (Proxy :: Proxy Status)
    , DefineElm (Proxy :: Proxy Target)
    , DefineElm (Proxy :: Proxy Trap)
    , DefineElm (Proxy :: Proxy Trigger)
    , DefineElm (Proxy :: Proxy Variant)
    --, DefineElm (Proxy :: Proxy Ninja)
    --, DefineElm (Proxy :: Proxy Game)
    ]
