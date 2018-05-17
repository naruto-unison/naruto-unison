module Main where

import Prelude

import Control.Coroutine                as CR
import Control.Coroutine.Aff            as CRA
import DOM.Event.EventTarget            as EET
import DOM.Websocket.Event.EventTypes   as WSET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket          as WS
import Halogen                          as H
import Halogen.Aff                      as HA

import Control.Monad.Aff           (Aff, delay, launchAff_)
import Control.Monad.Aff.AVar      (AVAR)
import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except        (runExcept)

import Data.Either                 (Either(..), either)
import Data.Foldable               (for_)
import Data.Foreign                (F, Foreign, toForeign, readString)
import Data.Maybe                  (Maybe(..))
import Data.Time.Duration          (Milliseconds(..))
import Data.UUID 
import DOM                         (DOM)
import Halogen.VDom.Driver         (runUI)

import Component.Site as Site

import Operators
import FFI.Import (hostname)
import FFI.Sound  (register)

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer ∷ ∀ e. WS.WebSocket → CR.Producer String 
             (Aff (avar ∷ AVAR, exception ∷ EXCEPTION, dom ∷ DOM | e)) Unit
wsProducer socket = CRA.produce \emit →
  EET.addEventListener
    WSET.onMessage
    (listener emit)
    false
    (WS.socketToEventTarget socket)
  where
  listener emit = EET.eventListener \ev → do
    for_ (readHelper WS.readMessageEvent ev) \msgEvent →
      for_ (readHelper readString (ME.data_ msgEvent)) \msg →
        emit (Left msg)
  readHelper :: forall a b. (Foreign → F a) → b → Maybe a
  readHelper read = either (const Nothing) Just ∘ runExcept ∘ read ∘ toForeign

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `AddMessage` queries in when it receives inputs from the
-- producer.
wsConsumer ∷ ∀ e. (Site.Query ~> Aff (HA.HalogenEffects (uuid ∷ GENUUID | e)))
           → CR.Consumer String (Aff (HA.HalogenEffects (uuid ∷ GENUUID | e))) 
             Unit
wsConsumer query = CR.consumer \msg -> do
  uuid ← liftEff genUUID
  query ∘ H.action $ Site.ReceiveMsg (Site.SocketMsg msg) uuid
  liftEff ∘ launchAff_ $ do
    delay $ Milliseconds 60000.0
    query ∘ H.action $ Site.EndTurn uuid
  pure Nothing

-- A consumer coroutine that takes output messages from our component IO
-- and sends them using the websocket
wsSender ∷ ∀ e. WS.WebSocket → CR.Consumer Site.SocketMsg 
           (Aff (HA.HalogenEffects (dom ∷ DOM | e))) Unit
wsSender socket = CR.consumer \(Site.SocketMsg msg) → do
  liftEff $ WS.sendString socket msg
  pure Nothing

main ∷ Eff (HA.HalogenEffects (Site.Effects (dom ∷ DOM, uuid ∷ GENUUID))) Unit
main = do
  register
  socket ← WS.create (WS.URL hostname) []
  HA.runHalogenAff do
    body ← HA.awaitBody
    io ← runUI Site.component unit body
    io.subscribe $ wsSender socket
    CR.runProcess (wsProducer socket CR.$$ wsConsumer io.query)
