port module Main exposing (main)

import Browser
import Json.Encode exposing (Value)

import Ports exposing (Ports)
import Site.Application exposing (Model, Msg, app)
import Sound


port progress : ( Int, Int, Int ) -> Cmd msg


port sounds : List String -> Cmd msg


port sound : String -> Cmd msg


port websocketSend : String -> Cmd msg


port websocketReceive : (String -> msg) -> Sub msg


ports : Ports msg
ports =
    { progress  = \dur from to -> progress ( dur, from, to )
    , sounds    = sounds << List.map Sound.show
    , sound     = sound << Sound.show
    , websocket = websocketSend
    }


{-| Runs the website interface.
-}
main : Program Value Model Msg
main =
    app websocketReceive ports
    |> Browser.document
