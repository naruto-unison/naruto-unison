module Site.Application exposing (app)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Browser.Dom        as Dom
import Html               as H exposing (Html)
import Html.Attributes    as P
import Json.Decode        as Json
import Json.Encode exposing (Value)
import Url exposing (Url)

import Flags
import Sound exposing (Sound(..))
import Site.Component exposing (Component)
import Site.Play as Play
import Site.Select as Select
{-
import List.Extra         as List
import Browser.Navigation as Navigation

import Date
import Dict    exposing (Dict)
import Task
import Time

import StandardLibrary     exposing (..)
import Persist.Flags       exposing (..)
import Persist.Preferences exposing (..)
import Printing            exposing (..)
import Site.Algebra        exposing (..)

import Site.CraftEssence.Component as CraftEssences
import Site.Servant.Component      as Servants
import Site.Team.Component         as Teams
-}

{-| The page currently being shown. -}
type Viewing = Select | Play

type alias Model =
    { error       : Maybe String
    , navKey      : Navigation.Key
    , viewing     : Viewing
    , selectModel : Select.Model
    , playModel   : Play.Model
    }

type Msg
    = RequestUrl UrlRequest
    | ChangeUrl  Url
    | SelectMsg  Select.Msg
    | PlayMsg    Play.Msg
    | OnError    (Result Dom.Error ())

printError : Dom.Error -> String
printError a = case a of
  Dom.NotFound id -> "Element #" ++ id ++ " not found!"

app sound progress =
  let
    child constr un = constr (Cmd.map un << sound) (Cmd.map un << progress)

    selectChild : Component Select.Model Select.Msg
    selectChild = child Select.component <| \a -> case a of
      SelectMsg x -> x
      _           -> Select.DoNothing

    playChild : Component Play.Model Play.Msg
    playChild = child Play.component <| \a -> case a of
      PlayMsg x -> x
      _         -> Play.DoNothing

    init : Value -> Url -> Navigation.Key -> (Model, Cmd Msg)
    init val url key =
      let
        (error, flags) =
          case Json.decodeValue Flags.decode val of
            Ok ok   -> (Nothing, ok)
            Err err ->
              (Just <| Json.errorToString err
              , { characters = []
                , user       = Nothing
                }
              )
        st = { error       = error
             , navKey      = key
             , viewing     = Select
             , selectModel = selectChild.init flags
             , playModel   = playChild.init flags
             }
      in
        (st, Cmd.none)

    view : Model -> Document Msg
    view st =
      let
        showError = case st.error of
          Nothing -> identity
          Just err -> (::) <| H.div [P.id "error"] [H.text err]
      in
        Document "Naruto Unison" << showError <| case st.viewing of
          Select ->
            [ H.map SelectMsg <| selectChild.view st.selectModel ]
          Play ->
            [ H.map PlayMsg <| playChild.view st.playModel ]

    update : Msg -> Model -> (Model, Cmd Msg)
    update parentMsg st = case parentMsg of
      OnError (Ok _)    -> (st, Cmd.none)
      OnError (Err err) -> ({ st | error = Just <| printError err }, Cmd.none)
      RequestUrl urlRequest -> case urlRequest of
        Browser.Internal url  ->
            (st, Navigation.pushUrl st.navKey <| Url.toString url)
        Browser.External href -> 
            (st, Navigation.load href)
      ChangeUrl {path} -> (st, Cmd.none)
      SelectMsg msg ->
          let
            (model, cmd) = selectChild.update msg st.selectModel
          in
            ({ st | selectModel = model }, Cmd.map SelectMsg cmd)
      PlayMsg msg ->
          let
            (model, cmd) = playChild.update msg st.playModel
          in
            ({ st | playModel = model }, Cmd.map PlayMsg cmd)
  in
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = always Sub.none
    , onUrlRequest  = RequestUrl
    , onUrlChange   = ChangeUrl
    }
