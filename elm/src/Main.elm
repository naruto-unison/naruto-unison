port module Main exposing (main)

import Browser
import Ports exposing (Ports)

import Sound exposing (Sound)
import Site.Application exposing (app)

port progress : (Int, Int, Int) -> Cmd msg
port sounds   : List String -> Cmd msg
port sound    : String -> Cmd msg
port websocketSend    : String -> Cmd msg
port websocketReceive : (String -> msg) -> Sub msg

sounds_ : List Sound -> Cmd msg
sounds_ = sounds << List.map Sound.show

sound_ : Sound -> Cmd msg
sound_ = sound << Sound.show

ports : Ports msg
ports = { progress  = \dur from to -> progress (dur, from, to)
        , sounds    = sounds << List.map Sound.show
        , sound     = sound << Sound.show
        , websocket = websocketSend
        }

{-| Runs the website interface. -}
main = Browser.document <| app websocketReceive ports
