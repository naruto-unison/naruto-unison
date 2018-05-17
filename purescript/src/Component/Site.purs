module Component.Site 
    ( Effects
    , Query(..)
    , Stage(..)
    , component
    , module Component.Common
    ) where

import Prelude

import Component.CharacterSelect as Select
import Component.Play            as Play
import Halogen                   as Halogen
import Halogen.HTML              as H
import Halogen.HTML.Events       as E
import Network.HTTP.Affjax       as AX

import Control.Monad.Aff           (Aff)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Argonaut.Parser        (jsonParser)
import Data.Array                  ((:), intercalate, reverse)
import Data.Either         
import Data.Maybe          
import Data.Time.Duration          (Milliseconds(..))
import Data.UUID
import DOM                         (DOM)
import Halogen                     ( Component, ParentDSL, ParentHTML
                                   , get, liftAff, liftEff, modify, query, raise
                                   )
import Halogen.HTML                (HTML)
import Network.HTTP.Affjax         (AJAX)

import FFI.Import                  (bg)
import FFI.Progress                (progress)
import FFI.Sound                   (AUDIO, Sound(..), sound)

import Operators
import Structure           
import Component.Common    

type Effects e = (ajax ∷ AJAX, audio ∷ AUDIO, dom ∷ DOM | e)

data Query a = HandleQueue Select.Output      a 
             | HandleGame  Play.Output        a
             | ReceiveMsg  SocketMsg     UUID a
             | EndTurn     UUID               a

data ChildSlot = SelectSlot | PlaySlot
derive instance eqChildSlot  ∷ Eq ChildSlot
derive instance ordChildSlot ∷ Ord ChildSlot

data Stage = Waiting | Queueing | Playing | Practicing
derive instance eqStage ∷ Eq Stage

type State = { stage    ∷ Stage
             , gameInfo ∷ Either String GameInfo
             , turn     ∷ Maybe UUID
             }

component ∷ ∀ m. Component HTML Query Unit SocketMsg (Aff (Effects m))
component = Halogen.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState ∷ State
  initialState = { stage:    Waiting
                 , gameInfo: Left ""
                 , turn:     Nothing
                 }

  render ∷ State → ParentHTML Query ChildQuery ChildSlot (Aff (Effects m))
  render {gameInfo, stage} = contents $ case gameInfo of
    Right gameInfo' → 
      [ H.img [_i "bg", _src bg ]
      , H.slot PlaySlot (Play.component (stage ≡ Practicing) gameInfo') 
                        unit (E.input HandleGame)
      ]
    Left error → 
      [ H.span [_c "error"] [H.text error]
      , H.slot SelectSlot Select.component unit (E.input HandleQueue)
      ]
    where contents | stage ≡ Queueing = H.div [_i "contents", _c "queueing"] 
                                      ∘ (H.aside [_i "searching"] 
                                        [H.img [_src "/img/spin.gif"]]:_)
                   | otherwise        = H.div [_i "contents"]
  eval ∷ Query ~> ParentDSL State Query ChildQuery ChildSlot SocketMsg (Aff (Effects m))
  eval = case _ of
      HandleQueue (Select.Queued Practice team) next → do
        let teamList = intercalate "/" ∘ reverse $ _characterName ↤ team
        {response} ← liftAff $ AX.get ("/api/practicequeue/" ⧺ teamList)
        modify _{ gameInfo = decodeJson response, stage = Practicing }
        liftEff $ progress (Milliseconds 0.0) 1 1
        sound SFXStartFirst
        pure next
      HandleQueue (Select.Queued Quick team) next → do
        let teamList = intercalate "/" ∘ reverse $ _characterName ↤ team
        modify _{ stage = Queueing }
        sound SFXApplySkill
        raise $ SocketMsg teamList
        pure next
      HandleQueue (Select.UpdateMsg msg) next → do
        raise msg
        pure next
      HandleQueue _ next →
        pure next
      HandleGame (Play.Finish _) next → do
        modify _{ gameInfo = Left "", turn = Nothing, stage = Waiting }
        pure next
      HandleGame (Play.ActMsg msg) next → do
        modify _{ turn = Nothing }
        raise msg
        pure next
      ReceiveMsg (SocketMsg msg) uuid next → do
        {stage} ← get
        case stage of
          Queueing → do
            let result = jsonParser msg ≫= decodeJson
            modify _{ gameInfo = result
                    , stage    = Playing 
                    }
            case result of
              Left _ → pure next
              Right (GameInfo {gamePar}) → do
                liftEff $ progress (Milliseconds 60000.0) (1 - gamePar) gamePar
                sound SFXStartFirst
                pure next
          Playing → do
            modify _{ turn = Just uuid }
            case jsonParser msg ≫= decodeJson of
              Left _ → pure next
              Right (game ∷ Game) → do
                _ ← query PlaySlot $ QueryPlay (ReceiveGame game) next
                pure next
          _ → pure next  
      EndTurn uuid next → do
        {turn} ← get
        when (turn ≡ Just uuid) ∘ raise $ SocketMsg "0,0,0,0/0,0,0,0"
        pure next
