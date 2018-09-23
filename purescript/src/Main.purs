module Main (main) where

import StandardLibrary

import Effect.Aff                    as Aff
import Control.Coroutine             as CR
import Control.Coroutine.Aff         as CRA
import Web.Event.EventTarget         as EET
import Web.Socket.Event.EventTypes   as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket          as WS
import Halogen                       as H
import Halogen.Aff                   as HA

import Foreign                     (F, Foreign, unsafeToForeign, readString)
import Data.Time.Duration          (Milliseconds(..))
import Data.UUID 
import Halogen.VDom.Driver         (runUI)

import Site as Site

import FFI.Import (hostname)
import FFI.Sound  (register)

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer :: WS.WebSocket -> CR.Producer String Aff Unit
wsProducer socket = CRA.produce \emitter -> do
    listen <- listener emitter
    EET.addEventListener
        WSET.onMessage
        listen
        false
        (WS.toEventTarget socket)
  where
  listener emitter = EET.eventListener \ev -> 
      for_ (ME.fromEvent ev) \msgEvent ->
      for_ (readHelper readString (ME.data_ msgEvent)) $ 
          CRA.emit emitter
  readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
  readHelper read = either (const Nothing) 
                    Just <<< runExcept <<< read <<< unsafeToForeign

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `AddMessage` queries in when it receives inputs from the
-- producer.
wsConsumer :: (Site.Query ~> Aff) -> CR.Consumer String Aff Unit
wsConsumer query = CR.consumer \msg -> do
    uuid <- liftEffect genUUID
    query <<< H.action $ Site.ReceiveMsg (Site.SocketMsg msg) uuid
    liftEffect <<< Aff.launchAff_ $ do
        Aff.delay $ Milliseconds 60000.0
        query <<< H.action $ Site.EndTurn uuid
    pure Nothing

-- A consumer coroutine that takes Message messages from our component IO
-- and sends them using the websocket
wsSender :: WS.WebSocket -> CR.Consumer Site.SocketMsg Aff Unit
wsSender socket = CR.consumer \(Site.SocketMsg msg) -> do
    liftEffect $ WS.sendString socket msg
    pure Nothing

main :: Effect Unit
main = do
    register
    socket <- WS.create hostname []
    HA.runHalogenAff do
        body <- HA.awaitBody
        io <- runUI Site.component unit body
        io.subscribe $ wsSender socket
        CR.runProcess (wsProducer socket CR.$$ wsConsumer io.query)
