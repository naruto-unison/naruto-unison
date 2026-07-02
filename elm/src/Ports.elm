module Ports exposing (Ports, map)

import Sound exposing (Sound)


type alias Ports msg =
    { progress : Int -> Int -> Int -> Cmd msg
    , sounds : List Sound -> Cmd msg
    , sound : Sound -> Cmd msg
    , websocket : String -> Cmd msg
    }


map : Ports a -> (a -> b) -> Ports b
map {progress, sounds, sound, websocket} f =
    { progress = \dur from to -> Cmd.map f <| progress dur from to
    , sounds = Cmd.map f << sounds
    , sound = Cmd.map f << sound
    , websocket = Cmd.map f << websocket
    }
