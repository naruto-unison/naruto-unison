{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Behind-the-scenes utility pages. Requires sufficient 'Privilege'.
module Handler.Admin (getTestR) where

import ClassyPrelude.Yesod
import qualified Yesod.Auth as Auth
import           Yesod.WebSockets (webSockets)

import Core.App (Handler)
import Core.Fields (Privilege(..))
import Core.Model (User(..))
import Handler.Play (gameSocket)

-- | Fails if not logged in or 'userPrivilege' is lower than the argument.
authorize :: Privilege -> Handler ()
authorize privilege = do
  (_, user) <- Auth.requireAuthPair
  when (userPrivilege user < privilege) notAuthenticated

-- | Provides a simple JavaScript interface for 'gameSocket'.
getTestR :: Handler Html
getTestR = do
  authorize Moderator
  webSockets gameSocket
  defaultLayout do
    setTitle "Socket Test"
    [whamlet|
        <div #output>
        <form #form>
            <input #input autofocus>
    |]
    toWidget [lucius|
        \#output {
            width: 600px;
            height: 400px;
            border: 1px solid black;
            margin-bottom: 1em;
            p {
                margin: 0 0 0.5em 0;
                padding: 0 0 0.5em 0;
                border-bottom: 1px dashed #99aa99;
            }
        }
        \#input {
            width: 600px;
            display: block;
        }
    |]
    toWidget [julius|
        const output = document.getElementById("output")
        const form   = document.getElementById("form")
        const input  = document.getElementById("input")
        const conn   = new WebSocket(document.URL.replace(/^http/g, "ws"))

        conn.onmessage = e => {
            const p = document.createElement("p")
            p.appendChild(document.createTextNode(e.data.substr(0, 100) + "\n\n"))
            output.appendChild(p)
        }

        form.addEventListener("submit", e => {
            conn.send(input.value)
            input.value = ""
            e.preventDefault()
        })
|]
