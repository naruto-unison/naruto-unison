module Site.Component exposing (Component)

import Html exposing (Html)

import Flags exposing (Flags)

type alias Component model msg =
    { init          : Flags -> model
    , update        : msg -> model -> (model, Cmd msg)
    , view          : model -> Html msg
    }
