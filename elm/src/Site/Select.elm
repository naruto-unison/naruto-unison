module Site.Select exposing (Model, Msg(..), component)

import Browser.Navigation as Navigation
import Html as H exposing (Html)

import Flags exposing (Flags)
import Site.Component exposing (Component)
import Sound exposing (Sound(..))
{-
import Html.Keyed         as Keyed
import Browser.Navigation as Navigation
import Html.Lazy exposing (lazy3)

import Html.Events     as E
import Html.Attributes as P

import StandardLibrary       exposing (..)
import Database.CraftEssence exposing (..)
import Database.Skill        exposing (..)
import Persist.Flags         exposing (..)
import Persist.Preferences   exposing (..)
import Printing              exposing (..)
import Site.Algebra          exposing (..)
import Site.Common           exposing (..)
import Site.Filtering        exposing (..)
import Site.Rendering        exposing (..)
import Site.Update           exposing (..)
import Sorting               exposing (..)

import Class.ToImage as ToImage

import Site.CraftEssence.Filters exposing (..)
import Site.CraftEssence.Sorting exposing (..)
-}
type alias Model =
    { index : Int
    }

type Msg
    = Scroll Int
    | Quick
    | Practice
    | Private
    | DoNothing

component : (Sound -> Cmd Msg) -> (Int -> Cmd Msg) -> Component Model Msg
component sound = always <|
  let
    init : Flags -> Model
    init flags =
        { index = 0 }

    view : Model -> Html Msg
    view st = H.span [] [H.text "Select"]

    update : Msg -> Model -> (Model, Cmd Msg)
    update msg st = case msg of
        DoNothing -> (st, Cmd.none)
        _         -> (st, Cmd.none)
  in
    { init = init, view = view, update = update }
