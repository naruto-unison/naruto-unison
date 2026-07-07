module Site.Application exposing (Model, Msg, app)

import Browser exposing (Document)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Keyed as Keyed
import Import.Flags as Flags exposing (Flags, printFailure)
import Import.Model as Model exposing (QueueFailure(..), QueueMessage(..))
import Json.Decode as D exposing (Value)
import Ports exposing (Ports)
import Site.Play as Play
import Site.Select as Select
import Sound exposing (Sound)
import Util exposing (pure, showErr)


type ComponentModel
    = SelectModel Select.Model
    | PlayModel Play.Model


type alias Model =
    { error : Maybe String
    , flags : Flags
    , component : ComponentModel
    , bg : String
    }


isQueued : Model -> Bool
isQueued { component } =
    case component of
        SelectModel { stage } ->
            stage == Select.Queued

        PlayModel _ ->
            False


type Msg
    = PlayMsg Play.Msg
    | Receive String
    | SelectMsg Select.Msg


getPlayMsg : Msg -> Play.Msg
getPlayMsg msg =
    case msg of
        PlayMsg x ->
            x

        _ ->
            Play.DoNothing


getSelectMsg : Msg -> Select.Msg
getSelectMsg msg =
    case msg of
        SelectMsg x ->
            x

        _ ->
            Select.DoNothing


app :
    ((String -> Msg) -> Sub Msg)
    -> Ports Msg
    ->
        { init : Value -> ( Model, Cmd Msg )
        , subscriptions : Model -> Sub Msg
        , update : Msg -> Model -> ( Model, Cmd Msg )
        , view : Model -> Document Msg
        }
app websocket ports =
    let
        withSound : Sound -> Model -> ( Model, Cmd Msg )
        withSound sound st =
            ( st, ports.sound sound )

        select =
            Select.component <| Ports.map ports getSelectMsg

        play =
            Play.component <| Ports.map ports getPlayMsg

        init : Value -> ( Model, Cmd Msg )
        init val =
            let
                ( flags, error ) =
                    case D.decodeValue Flags.decode val of
                        Ok ok ->
                            ( ok, Nothing )

                        Err err ->
                            ( Flags.failure, Just <| D.errorToString err )

                st : Model
                st =
                    { flags = flags
                    , error = error
                    , component = SelectModel <| select.init flags
                    , bg = "url(" ++ flags.bg ++ ")"
                    }
            in
            ( st, ports.sounds Sound.enum )

        view : Model -> Document Msg
        view st =
            let
                showError xs =
                    case st.error of
                        Nothing ->
                            xs

                        Just err ->
                            renderError err
                                :: xs

                contents els =
                    if isQueued st then
                        Keyed.node "div" [ A.id "main", A.class "queueing" ] <|
                            renderSearching
                                ++ els

                    else
                        Keyed.node "div" [ A.id "main" ] els
            in
            Document "Naruto Unison"
                << List.singleton
                << contents
                << showError
            <|
                case st.component of
                    SelectModel model ->
                        [ ( "select", H.map SelectMsg <| select.view model ) ]

                    PlayModel model ->
                        [ renderBg st.bg
                        , ( "play", H.map PlayMsg <| play.view model )
                        ]

        update : Msg -> Model -> ( Model, Cmd Msg )
        update parentMsg st =
            case parentMsg of
                SelectMsg (Select.ReceiveGame (Ok x)) ->
                    ( { st | component = PlayModel <| play.init st.flags True x }
                    , Cmd.batch
                        [ ports.sound Sound.StartFirst, ports.progress 0 1 1 ]
                    )

                SelectMsg (Select.ReceiveGame (Err err)) ->
                    pure { st | error = Just <| showErr err }

                SelectMsg msg ->
                    case st.component of
                        SelectModel model ->
                            let
                                ( newmodel, cmd ) =
                                    select.update msg model
                            in
                            ( { st | component = SelectModel newmodel }
                            , Cmd.map SelectMsg cmd
                            )

                        PlayModel _ ->
                            pure st

                PlayMsg msg ->
                    case st.component of
                        PlayModel model ->
                            let
                                ( newmodel, cmd ) =
                                    play.update msg model
                            in
                            ( { st | component = PlayModel newmodel }
                            , Cmd.map PlayMsg cmd
                            )

                        SelectModel _ ->
                            pure st

                Receive msg ->
                    case D.decodeString Model.jsonDecQueueMessage msg of
                        Ok Ping ->
                            if isQueued st then
                                ( st, ports.websocket "pong" )

                            else
                                pure st

                        Ok (Fail failure) ->
                            fail failure st

                        Ok (Info info) ->
                            let
                                firstPlayer =
                                    info.player == info.turn.playing

                                progress =
                                    if firstPlayer then
                                        0

                                    else
                                        1
                            in
                            ( { st | component = PlayModel <| play.init st.flags False info }
                            , Cmd.batch
                                [ ports.progress 60000 (1 - progress) progress
                                , ports.sound <|
                                    if firstPlayer then
                                        Sound.StartFirst

                                    else
                                        Sound.StartSecond
                                ]
                            )

                        Err err ->
                            pure { st | error = Just <| D.errorToString err }

        fail : QueueFailure -> Model -> ( Model, Cmd Msg )
        fail failure st =
            case failure of
                AlreadyQueued ->
                    failTo Select.Browsing AlreadyQueued st

                NotFound ->
                    failTo Select.Searching NotFound st

                Locked a ->
                    failTo Select.Browsing (Locked a) st

                _ ->
                    pure st

        failTo : Select.Stage -> QueueFailure -> Model -> ( Model, Cmd Msg )
        failTo stage failure st =
            case st.component of
                SelectModel model ->
                    if model.stage == Select.Queued then
                        let
                            newmodel =
                                { model
                                    | stage = stage
                                    , error = Just <| printFailure failure
                                }
                        in
                        withSound Sound.Death
                            { st | component = SelectModel newmodel }

                    else
                        pure st

                PlayModel _ ->
                    pure st

        subscriptions : Model -> Sub Msg
        subscriptions { component } =
            case component of
                SelectModel _ ->
                    websocket Receive

                PlayModel _ ->
                    websocket (Play.Receive >> PlayMsg)
    in
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


renderError : String -> ( String, Html msg )
renderError err =
    ( "error"
    , H.div [ A.id "error" ]
        [ H.text err ]
    )


renderBg : String -> ( String, Html msg )
renderBg url =
    ( "bg"
    , H.div
        [ A.id "bg"
        , A.style "background-image" url
        ]
        []
    )


renderSearching : List ( String, Html Msg )
renderSearching =
    [ ( "searching"
      , H.div [ A.id "searching" ]
            [ H.img
                [ A.src "/img/spin.gif"
                , A.alt "Spinning loading indicator"
                ]
                []
            ]
      )
    , ( "cancel"
      , H.button
            [ A.id "cancel"
            , A.class "parchment playButton"
            , E.onClick <| SelectMsg Select.Dequeue
            ]
            [ H.text "Cancel" ]
      )
    ]
