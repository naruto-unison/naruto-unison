module Site.Application exposing (app)

import Browser exposing (Document, UrlRequest)
import Browser.Dom        as Dom
import Html               as H exposing (Html)
import Html.Attributes    as A
import Json.Decode        as D exposing (Value)
import Process
import Task
import Url exposing (Url)

import Import.Flags as Flags exposing (Flags)
import Import.Model as Model exposing (GameInfo)
import Ports exposing (Ports)
import Site.Play as Play
import Site.Select as Select
import Sound exposing (Sound(..))
import Util exposing (pure, showErr)

type alias Model =
    { error       : Maybe String
    , flags       : Flags
    , selectModel : Select.Model
    , playModel   : Maybe Play.Model
    }

type Msg
    = OnError   (Result Dom.Error ())
    | PlayMsg   Play.Msg
    | Receive   String
    | SelectMsg Select.Msg

printError : Dom.Error -> String
printError a = case a of
  Dom.NotFound id -> "Element #" ++ id ++ " not found!"

app websocket ports =
  let
    select = Select.component << Ports.map ports <| \a -> case a of
      SelectMsg x -> x
      _           -> Select.DoNothing

    play = Play.component << Ports.map ports <| \a -> case a of
      PlayMsg x -> x
      _         -> Play.DoNothing

    init : Value -> (Model, Cmd Msg)
    init val =
      let
        (flags, error) = case D.decodeValue Flags.decode val of
            Ok ok   -> (ok, Nothing)
            Err err -> (Flags.failure, Just <| D.errorToString err)
        st = { flags       = flags
             , error       = error
             , selectModel = select.init flags
             , playModel   = Nothing
             }
      in
        (st, ports.sounds Sound.enum)

    view : Model -> Document Msg
    view st =
      let
        showError = case st.error of
          Nothing  -> identity
          Just err -> (::) <| H.div [A.id "error"] [H.text err]
        contents =
            if st.selectModel.stage == Select.Queued then
                H.main_ [A.class "queueing"] <<
                (::)
                (H.div [A.id "searching"] [H.img [A.src "/img/spin.gif"] []])
            else
                H.main_ []
      in
        Document "Naruto Unison" << List.singleton << contents << showError <|
        case st.playModel of
            Just model ->
                [ H.img [A.id "bg", A.src st.flags.bg] []
                , H.map PlayMsg <| play.view model
                ]
            Nothing   ->
                [ H.map SelectMsg <| select.view st.selectModel ]

    update : Msg -> Model -> (Model, Cmd Msg)
    update parentMsg st = case parentMsg of
      OnError (Ok _)    -> pure st
      OnError (Err err) -> pure { st | error = Just <| printError err }
      SelectMsg (Select.ReceiveGame (Ok x)) ->
        let
          selectModel = st.selectModel
        in
          ( { st
            | playModel = Just <| play.init st.flags True x
            , selectModel = { selectModel | stage = Select.Browsing }
            }
          , Cmd.batch [ports.sound Sound.StartFirst, ports.progress 0 1 1]
          )
      SelectMsg (Select.ReceiveGame (Err err)) ->
          pure { st | error = Just <| showErr err }
      SelectMsg msg ->
          let
            (model, cmd) = select.update msg st.selectModel
          in
            ({ st | selectModel = model }, Cmd.map SelectMsg cmd)
      PlayMsg msg -> case Maybe.map (play.update msg) <| st.playModel of
          Nothing           -> pure st
          Just (model, cmd) ->
              ({ st | playModel = Just model }, Cmd.map PlayMsg cmd)
      Receive json -> case D.decodeString Model.jsonDecGameInfo json of
                Err err -> pure { st | error = Just <| D.errorToString err }
                Ok info ->
                  let
                    selectModel = st.selectModel
                    firstPlayer = info.player == info.game.playing
                    progress    = if firstPlayer then 0 else 1
                  in
                    ( { st
                      | playModel   = Just <| play.init st.flags False info
                      , selectModel = { selectModel | stage = Select.Browsing }
                      }
                    , Cmd.batch
                      [ ports.progress 60000 (1 - progress) progress
                      , ports.sound <|
                            if firstPlayer then
                                Sound.StartFirst
                            else
                                Sound.StartSecond
                      ]
                    )

    subscriptions : Model -> Sub Msg
    subscriptions st = case st.playModel of
        Nothing -> websocket Receive
        Just _  -> websocket <| (PlayMsg << Play.Receive)
  in
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
