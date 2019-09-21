module Site.Application exposing (app)

import Browser exposing (Document, UrlRequest)
import Browser.Dom        as Dom
import Html               as H exposing (Html)
import Html.Attributes    as A
import Html.Events        as E
import Json.Decode        as D exposing (Value)
import Process
import Task
import Url exposing (Url)

import Import.Flags as Flags exposing (Flags, printFailure)
import Import.Model as Model exposing (Failure(..), GameInfo)
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
        contents els =
            if st.selectModel.stage == Select.Queued then
                H.main_ [A.class "queueing"] <|
                [ H.div [A.id "searching"] [H.img [A.src "/img/spin.gif"] []]
                , H.button
                  [ A.id "cancel"
                  , A.class "parchment playButton click"
                  , E.onClick <| SelectMsg Select.Dequeue
                  ]
                  [H.text "Cancel"]
                ] ++ els
            else
                H.main_ [] els
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
      Receive "ping" ->
          if st.selectModel.stage == Select.Queued then
              (st, ports.websocket "pong")
          else
              pure st
      Receive msg -> case D.decodeString Model.jsonDecGameInfo msg of
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
          Err err -> case D.decodeString Model.jsonDecFailure msg of
              Ok AlreadyQueued    -> failAt Select.Browsing  AlreadyQueued st
              Ok OpponentNotFound -> failAt Select.Searching OpponentNotFound st
              Ok _                -> pure st
              Err _ -> pure { st | error = Just <| D.errorToString err }

    failAt : Select.Stage -> Failure -> Model -> (Model, Cmd Msg)
    failAt stage failure st =
        let
          selectModel = st.selectModel
        in
          if selectModel.stage == Select.Queued then
              ( { st
                | selectModel =
                  { selectModel
                  | stage = stage
                  , error = Just <| printFailure failure
                  }
                }
              , ports.sound Sound.Death
              )
          else
              pure st

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
