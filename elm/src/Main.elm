port module Main exposing (main)

import Browser

import Sound exposing (Sound, showSound)
import Site.Application exposing (app)

{-| Updates the progress bar. -}
port progress  : Int -> Cmd msg
{-| Plays a sound. -}
port sound     : String -> Cmd msg

sound_ : Sound -> Cmd msg
sound_ = sound << showSound

{-| Runs the website interface. -}
main = Browser.application <| app sound_ progress
