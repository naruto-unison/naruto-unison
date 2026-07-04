module Site.Play exposing (Model, Msg(..), component)

import Array exposing (Array)
import Dict
import Game.Act as Act exposing (Act)
import Game.Chakras as Chakras exposing (none)
import Game.Characters as Characters exposing (Characters)
import Game.Detail as Detail exposing (Detail)
import Game.Effect as Effect
import Game.Game as Game
import Game.Skill as Skill
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Lazy exposing (lazy2, lazy4)
import Http
import Import.Flags exposing (Flags)
import Import.Model as Model exposing (Chakras, Character, Destructible, Effect, GameInfo, GameMessage(..), Ninja, Player(..), Reward, Skill, Turn, User, War(..))
import Json.Decode as D
import List.Extra as List
import Maybe.Extra as Maybe
import Ports exposing (Ports)
import Process
import Set exposing (Set)
import Site.Render as Render
import Sound exposing (Sound)
import Task
import User
import Util exposing (ListChange(..), pure, showErr)


type Viewable
    = ViewCharacter Character
    | ViewDestructible Destructible
    | ViewDetail (Effect -> Bool) Detail
    | ViewSkill (List Int) Int Skill
    | ViewUser User


type alias ChakraPair =
    { chakra : String
    , spend : Chakras
    , amount : Int
    , random : Int
    }


getChakraPairs : Chakras -> Chakras -> List ChakraPair
getChakraPairs net randoms =
    let
        pair : String -> (Chakras -> Int) -> Chakras -> ChakraPair
        pair chakra get spend =
            { chakra = chakra
            , spend = spend
            , amount = get net
            , random = get randoms
            }
    in
    [ pair "blood" .blood { none | blood = 1 }
    , pair "gen" .gen { none | gen = 1 }
    , pair "nin" .nin { none | nin = 1 }
    , pair "tai" .tai { none | tai = 1 }
    ]


type alias NinjaBundle =
    { character : Character
    , ninja : Ninja
    , targets : List (List Int)
    }


type alias ChakraSums =
    { free : Chakras
    , net : Chakras
    , rand : Int
    }


type alias Model =
    { url : String
    , practice : Bool
    , player : Player
    , user : User
    , vs : User
    , characters : Characters
    , game : Turn
    , ninjas : Array Character
    , ownTurn : Bool
    , chakras : Chakras
    , randoms : Chakras
    , exchanged : Chakras
    , chakraSums : ChakraSums
    , exchange : Bool
    , viewing : Viewable
    , highlight : List Int
    , toggled : Maybe Act
    , acts : List Act
    , dna : List Reward
    , war : Maybe War
    , error : String
    }


recalculateChakra : Model -> Model
recalculateChakra st =
    { st | chakraSums = sumChakras st }


sumChakras : Model -> ChakraSums
sumChakras { acts, chakras, exchanged, randoms } =
    let
        costs =
            acts
                |> List.map (.skill >> .cost)
                |> Chakras.sum

        net =
            Chakras.sub (Chakras.add exchanged chakras) costs

        netUnrand =
            { net | rand = 0 }

        rand =
            Chakras.total randoms
                + net.rand
                - (Chakras.rate * Chakras.total exchanged)

        free =
            { net | rand = Chakras.total netUnrand + rand }
    in
    { free = free, net = net, rand = rand }


setGame : Turn -> Model -> Model
setGame game st =
    recalculateChakra
        { st
            | game = game
            , chakras = game.chakra
            , ninjas =
                Array.fromList <|
                    List.map (Characters.merge st.characters) game.ninjas
            , randoms = Chakras.none
            , exchanged = Chakras.none
            , acts = []
            , ownTurn = st.player == game.playing
        }


type ExchangeMsg
    = Begin
    | Conclude Chakras
    | Reset


type Msg
    = DoNothing
    | Enact ListChange Act
    | Exchange ExchangeMsg
    | Forfeit
    | Ready
    | Receive String
    | ReceivePractice (Result Http.Error (List Turn))
    | Spend Chakras
    | Toggle Act
    | Unhighlight
    | View Viewable


encodePathPieces : (a -> List Int) -> List a -> List String
encodePathPieces toPathPieces =
    List.map (toPathPieces >> List.map String.fromInt >> String.join ",")


encodeEnact : Model -> String
encodeEnact { acts, exchanged, randoms } =
    let
        chakraPieces =
            encodePathPieces Chakras.toPathPieces [ randoms, exchanged ]

        actPieces =
            encodePathPieces Act.toPathPieces acts
    in
    String.join "/" <| chakraPieces ++ actPieces


enactUrl : Model -> String
enactUrl st =
    st.url ++ "api/practiceact/" ++ encodeEnact st


component :
    Ports Msg
    ->
        { init : Flags -> Bool -> GameInfo -> Model
        , update : Msg -> Model -> ( Model, Cmd Msg )
        , view : Model -> Html Msg
        }
component ports =
    let
        withSound : Sound -> Model -> ( Model, Cmd Msg )
        withSound sfx st =
            ( st, ports.sound sfx )

        init : Flags -> Bool -> GameInfo -> Model
        init flags practice info =
            recalculateChakra <|
                setGame info.turn
                    { url = flags.url
                    , practice = practice
                    , player = info.player
                    , user = Maybe.withDefault info.opponent flags.user
                    , vs = info.opponent
                    , characters = flags.characters
                    , game = info.turn
                    , ownTurn = info.player == info.turn.playing
                    , ninjas = Array.empty
                    , chakras = info.turn.chakra
                    , randoms = Chakras.none
                    , exchanged = Chakras.none
                    , chakraSums =
                        { free = Chakras.none
                        , net = Chakras.none
                        , rand = 0
                        }
                    , exchange = False
                    , viewing = ViewUser info.opponent
                    , highlight = []
                    , toggled = Nothing
                    , acts = []
                    , dna = []
                    , war = info.war
                    , error = ""
                    }

        view : Model -> Html Msg
        view st =
            let
                { allies, enemies } =
                    List.map3 NinjaBundle
                        (Array.toList st.ninjas)
                        st.game.ninjas
                        st.game.targets
                        |> Game.split st.player

                ninjaData =
                    createNinjaData st
            in
            H.div
                [ A.id "game"
                , A.classList [ ( "over", not <| List.isEmpty st.game.victor ) ]
                ]
            <|
                [ H.div [ A.id "error" ]
                    [ H.text st.error ]
                , renderTop st st.ninjas
                , H.section [ A.id "player0", A.class "player" ] <|
                    List.map (renderNinja ninjaData True) allies
                , H.section [ A.id "player1", A.class "player" ] <|
                    List.map (renderNinja ninjaData False) enemies
                ]
                    ++ renderCenter st

        setGameAnd : Turn -> Model -> List (Cmd Msg) -> ( Model, Cmd Msg )
        setGameAnd game st cmds =
            ( setGame game st
            , Cmd.batch <|
                if Game.died st.player st.game game then
                    ports.sound Sound.Death :: cmds

                else
                    cmds
            )

        update : Msg -> Model -> ( Model, Cmd Msg )
        update msg st =
            case msg of
                View ((ViewSkill targets _ _) as viewing) ->
                    pure { st | viewing = viewing, highlight = targets }

                View viewing ->
                    pure { st | viewing = viewing }

                DoNothing ->
                    pure st

                Unhighlight ->
                    pure { st | highlight = [] }

                Toggle skill ->
                    withSound Sound.Target <|
                        if st.toggled == Just skill then
                            { st | toggled = Nothing }

                        else
                            { st | toggled = Just skill }

                Enact Add act ->
                    withSound Sound.ApplySkill <|
                        recalculateChakra
                            { st
                                | acts = st.acts ++ [ act ]
                                , toggled = Nothing
                            }

                Enact Delete act ->
                    withSound Sound.Cancel <|
                        recalculateChakra
                            { st
                                | acts = List.remove act st.acts
                                , toggled = Nothing
                            }

                Spend chakras ->
                    withSound Sound.Click <|
                        recalculateChakra
                            { st
                                | randoms = Chakras.add st.randoms chakras
                                , chakras = Chakras.sub st.chakras chakras
                            }

                Exchange Begin ->
                    withSound Sound.Target
                        { st | exchange = not st.exchange }

                Exchange Reset ->
                    withSound Sound.Cancel <|
                        recalculateChakra
                            { st
                                | chakras = st.game.chakra
                                , randoms = Chakras.none
                                , exchanged = Chakras.none
                                , exchange = False
                            }

                Exchange (Conclude chakras) ->
                    withSound Sound.Click <|
                        recalculateChakra
                            { st
                                | exchanged = Chakras.add st.exchanged chakras
                                , exchange = False
                            }

                Forfeit ->
                    if st.practice then
                        setGameAnd
                            (Game.forfeit st.player st.game)
                            st
                            [ ports.sound Sound.Lose ]

                    else
                        ( st, ports.websocket "forfeit" )

                Receive json ->
                    case D.decodeString Model.jsonDecGameMessage json of
                        Err err ->
                            pure { st | error = D.errorToString err }

                        Ok (Play game) ->
                            setGameAnd game st <|
                                case game.victor of
                                    [ victor ] ->
                                        if victor == st.player then
                                            [ ports.sound Sound.Win ]

                                        else
                                            [ ports.sound Sound.Lose ]

                                    [] ->
                                        [ ports.sound Sound.StartTurn
                                        , if game.playing == st.player then
                                            ports.progress 60000 1 0

                                          else
                                            ports.progress 60000 0 1
                                        ]

                                    _ ->
                                        [ ports.sound Sound.Death ]

                        Ok (Rewards dna) ->
                            pure { st | dna = dna }

                Ready ->
                    if st.practice then
                        ( recalculateChakra
                            { st
                                | exchange = False
                                , exchanged = Chakras.none
                                , toggled = Nothing
                            }
                        , Http.get
                            { url = enactUrl st
                            , expect =
                                Http.expectJson ReceivePractice <|
                                    D.list Model.jsonDecTurn
                            }
                        )

                    else
                        ( st
                        , Cmd.batch
                            [ ports.sound Sound.StartTurn
                            , ports.websocket <| encodeEnact st
                            ]
                        )

                ReceivePractice (Ok [ x, y ]) ->
                    setGameAnd x
                        st
                        [ ports.progress 1500 0 1
                        , Process.sleep 1500
                            |> Task.perform
                                (always <| ReceivePractice <| Ok [ y ])
                        ]

                ReceivePractice (Ok [ y ]) ->
                    setGameAnd y
                        st
                        [ ports.sound Sound.StartTurn
                        , ports.progress 0 1 1
                        ]

                ReceivePractice (Ok _) ->
                    pure { st | error = "Invalid response from server" }

                ReceivePractice (Err err) ->
                    pure { st | error = showErr err }
    in
    { init = init, view = view, update = update }



-- TOP


warInverse : War -> War
warInverse war =
    case war of
        Red ->
            Blue

        Blue ->
            Red


renderTop : Model -> Array Character -> Html Msg
renderTop { game, user, viewing, vs, war } characters =
    let
        vsWar =
            Maybe.map warInverse war

        ( playerInactive, vsInactive ) =
            game.inactive
    in
    H.section [ A.id "top" ]
        [ lazy4 renderUserBox "account0" user war playerInactive
        , lazy2 renderView characters viewing
        , lazy4 renderUserBox "account1" vs vsWar vsInactive
        ]


renderUserBox : String -> User -> Maybe War -> Int -> Html Msg
renderUserBox id user war inactive =
    H.section
        [ A.id id
        , E.onMouseOver <| View <| ViewUser user
        ]
        [ H.section []
            [ H.h3 []
                [ H.text user.name ]
            , H.p []
                [ H.text <| User.rank user ]
            , H.p [ A.class "inactive" ] <| List.repeat inactive <| H.text "X"
            ]
        , H.div [ A.class "charWrapper" ]
            [ H.img
                [ A.class "charicon"
                , A.src user.avatar
                ]
                []
            , case war of
                Just Red ->
                    H.div [ A.class "red" ] []

                Just Blue ->
                    H.div [ A.class "blue" ] []

                Nothing ->
                    H.div [] []
            ]
        ]



-- CENTER


renderCenter : Model -> List (Html Msg)
renderCenter st =
    let
        { victor } =
            st.game
    in
    if List.isEmpty victor then
        [ renderChakraModule st
        , renderActs st
        ]

    else
        renderGameOver st.player st.dna victor


renderChakraButton : String -> msg -> Bool -> Html msg
renderChakraButton text msg condition =
    H.button
        [ A.id text
        , A.class "chakraButton"
        , E.onClick msg
        , A.disabled <| not condition
        ]
        [ H.text text ]


renderChakraModule : Model -> Html Msg
renderChakraModule { chakraSums, exchange, exchanged, ownTurn, randoms } =
    let
        { free, net, rand } =
            chakraSums

        chakraPairs =
            getChakraPairs net randoms
    in
    H.section [ A.id "playchakra" ] <|
        List.map (renderChakraPair ownTurn exchange net) chakraPairs
            ++ [ Render.rands (Chakras.total { net | rand = 0 }) rand
               , renderChakraButton "exchange" (Exchange Begin) <|
                    (free.rand >= Chakras.rate)
                        && Chakras.canExchange net
                        && ownTurn
               , renderChakraButton "reset" (Exchange Reset) <|
                    (exchanged /= Chakras.none)
                        || (randoms /= Chakras.none)
               , renderChakraButton "forfeit" Forfeit ownTurn
               ]


renderChakraPair : Bool -> Bool -> Chakras -> ChakraPair -> Html Msg
renderChakraPair turn exchange chakras { chakra, spend, amount, random } =
    H.div []
        [ H.button
            [ A.class <| "chakra " ++ chakra
            , E.onClick <| Exchange <| Conclude spend
            , A.disabled <| not <| exchange && Chakras.affordable chakras spend
            ]
            []
        , H.span []
            [ H.text <| String.fromInt amount ]
        , H.button
            [ A.class "more"
            , E.onClick <| Spend <| Chakras.negate spend
            , A.disabled <| not turn || random <= 0
            ]
            [ H.text "+" ]
        , H.button
            [ A.class "less"
            , E.onClick <| Spend spend
            , A.disabled <| not turn || amount <= 0
            ]
            [ H.text "—" ]
        , H.div [ A.class "chakra rand" ] []
        , H.span []
            [ H.text <| String.fromInt random ]
        ]


renderActs : Model -> Html Msg
renderActs { ownTurn, chakraSums, ninjas, acts } =
    let
        noChakra =
            chakraSums.rand /= 0
    in
    H.section [ A.id "playqueuecont" ]
        [ H.div [ A.id "playqueue" ] <|
            List.map (renderAct ninjas) acts
        , H.button
            [ A.id "ready"
            , A.classList [ ( "noChakra", noChakra ) ]
            , E.onClick Ready
            , A.disabled <| not ownTurn || noChakra
            ]
            [ H.text <|
                if not ownTurn then
                    "Waiting"

                else if noChakra then
                    "Choose Chakra"

                else
                    "Ready"
            ]
        ]


renderAct : Array Character -> Act -> Html Msg
renderAct characters ({ skill } as act) =
    H.button
        [ A.class "act"
        , E.onClick <| Enact Delete act
        ]
        [ Render.skillIcon (Characters.root characters skill) skill []
        , H.div [ A.class "actcost" ] <|
            Render.chakras skill.cost
        ]


renderDna : Reward -> List (Html Msg)
renderDna { amount, reason } =
    [ H.dt [] [ H.text reason ]
    , H.dd [ A.class "dna" ] [ H.text <| String.fromInt amount ]
    ]


renderGameOver : Player -> List Reward -> List Player -> List (Html Msg)
renderGameOver player dna victors =
    let
        message =
            case victors of
                [ victor ] ->
                    if victor == player then
                        "Victory"

                    else
                        "Defeat"

                _ ->
                    "Tie"
    in
    [ H.div [ A.id "endgame" ]
        [ H.p []
            [ H.text message ]
        , H.a
            [ A.id "return"
            , A.class "playButton parchment"
            , A.href "/"
            ]
            [ H.text "Return" ]
        , H.dl [] <| List.concatMap renderDna dna
        ]
    ]



-- NINJA


renderHealth : String -> Int -> List (Html msg)
renderHealth anchor health =
    [ H.div [ A.style "width" <| String.fromInt health ++ "%" ]
        []
    , H.span
        [ A.class "charhealthtext"
        , A.style anchor (String.fromInt (health * 93 // 100) ++ "%")
        ]
      <|
        if health /= 0 then
            [ H.text <| String.fromInt health ]

        else
            []
    ]


renderDestructible : String -> String -> Int -> Destructible -> Html Msg
renderDestructible anchor class track x =
    H.div
        [ A.classList
            [ ( class, True )
            , ( "ghost", x.dur == Just 0 )
            ]
        , A.style anchor <| String.fromInt track ++ "%"
        , A.style "width" <| String.fromInt x.amount ++ "%"
        , E.onMouseOver <| View <| ViewDestructible x
        ]
        []


renderHpBar : String -> Ninja -> List (Html Msg)
renderHpBar anchor { barrier, defense, health } =
    let
        fold class x ( xs, amount ) =
            ( renderDestructible anchor class amount x :: xs, amount + x.amount )

        renderAll class destructibles init =
            List.foldr (fold class) init destructibles
    in
    ( renderHealth anchor health, health )
        |> renderAll "chardefense" defense
        |> renderAll "charbarrier" barrier
        |> Tuple.first


type alias SkillData =
    { user : Ninja
    , freeChakras : Chakras
    , active : Bool
    , characters : Array Character
    }


skillKey : Skill -> String
skillKey { name, owner } =
    String.cons (Char.fromCode <| owner + 48) name


renderSkill :
    SkillData
    -> Int
    -> List Int
    -> Skill
    -> Html Msg
renderSkill { user, freeChakras, active, characters } button targets skill =
    let
        { slot } =
            user

        key =
            skillKey skill

        icon =
            Render.skillIcon (Characters.root characters skill) skill []

        charge =
            Dict.get key user.charges
                |> Maybe.withDefault 0

        disabled =
            not active
                || List.isEmpty targets
                || Chakras.lacks freeChakras skill.cost

        cooldown =
            if disabled && user.health > 0 && skill.cooldown > 0 then
                Dict.get key user.cooldowns
                    |> Maybe.withDefault 0

            else
                0

        act : Act
        act =
            { user = slot
            , skill = skill
            , target = slot
            , button = button
            , targets = targets
            }

        onClick =
            if disabled then
                DoNothing

            else if Skill.targets slot skill == [ slot ] then
                Enact Add act

            else
                Toggle act
    in
    H.button
        [ A.class "charmove"
        , E.onMouseOver <| View <| ViewSkill [] charge skill
        , E.onMouseLeave Unhighlight
        , E.onClick onClick
        , A.disabled disabled
        ]
    <|
        if cooldown <= 0 then
            [ icon ]

        else
            [ icon
            , H.span [] [ H.text <| String.fromInt cooldown ]
            ]


renderDetail : Bool -> Int -> Array Character -> Detail -> Html Msg
renderDetail onTeam slot characters ({ classes } as detail) =
    let
        removable =
            if Detail.allied slot detail then
                always False

            else
                Effect.removable onTeam

        icon =
            Render.detailIcon (Characters.get characters detail.source) detail []
    in
    H.div
        [ E.onMouseOver <| View <| ViewDetail removable detail
        , A.classList
            [ ( "detail"
              , True
              )
            , ( "trap"
              , detail.trap
              )
            , ( "ghost"
              , detail.dur == Just 0
              )
            , ( "remove"
              , List.any removable detail.effects
                    && not (Set.member "Unremovable" classes)
              )
            , ( "invis"
              , Set.member "Invisible" classes
              )
            ]
        ]
        [ H.div [] <|
            if detail.amount > 1 then
                [ H.span [] [ H.text <| String.fromInt detail.amount ]
                , icon
                ]

            else
                [ icon ]
        , H.p [] <|
            if Set.member "Continues" classes then
                [ H.text "•" ]

            else
                [ H.text <| Render.duration "\u{00A0}" detail.dur ]
        ]


type alias NinjaData =
    { characters : Array Character
    , acted : List Int
    , toggle : Maybe Act
    , highlight : List Int
    , freeChakras : Chakras
    , ownTurn : Bool
    }


createNinjaData : Model -> NinjaData
createNinjaData { acts, chakraSums, highlight, ownTurn, ninjas, toggled } =
    { characters = ninjas
    , acted = List.map .user acts
    , toggle = toggled
    , highlight = highlight
    , freeChakras = chakraSums.free
    , ownTurn = ownTurn
    }


renderNinja :
    NinjaData
    -> Bool
    -> NinjaBundle
    -> Html Msg
renderNinja { characters, acted, toggle, highlight, freeChakras, ownTurn } onTeam { character, ninja, targets } =
    let
        anchor =
            if onTeam then
                "left"

            else
                "right"

        { slot } =
            ninja

        toggled =
            List.member slot (Act.toggles toggle)

        faceIcon =
            case ninja.face of
                Nothing ->
                    Render.charIcon character

                Just face ->
                    Render.icon (Characters.get characters face.user) <|
                        "icon"
                            ++ face.icon

        skillData =
            { user = ninja
            , freeChakras = freeChakras
            , characters = characters
            , active =
                onTeam
                    && ownTurn
                    && (ninja.health > 0)
                    && not (List.member slot acted)
            }

        render =
            renderDetail onTeam slot characters

        renderDetails attrs els =
            H.aside attrs <| List.map render els
    in
    H.section [ A.classList [ ( "dead", ninja.health == 0 ) ] ]
        [ renderDetails [ A.class "channels" ] <|
            List.map Detail.copy (Maybe.values ninja.copies)
                ++ List.map (Detail.channel slot) ninja.channels
        , H.button
            [ A.classList
                [ ( "face", True )
                , ( "highlighted", List.member slot highlight )
                , ( "toggled skill", toggled )
                ]
            , E.onMouseOver <| View <| ViewCharacter character
            , case Maybe.filter (always toggled) toggle of
                Just act ->
                    E.onClick <| Enact Add { act | target = slot }

                Nothing ->
                    A.disabled True
            ]
            [ faceIcon [ A.class "charicon" ] ]
        , H.div [ A.class "charmoves" ] <|
            List.map3
                (renderSkill skillData)
                (List.range 0 10 {- doesn't matter, not the limiter -})
                targets
                ninja.skills
        , H.div [ A.class "charhealth" ] <|
            renderHpBar anchor ninja
        , renderDetails [ A.class "statuses" ] <|
            Detail.get ninja
        ]



-- View


renderViewCharacter : Character -> Html msg
renderViewCharacter char =
    H.section []
        [ Render.charIcon char [ A.class "char" ]
        , H.section []
            [ H.h4 [] <|
                Render.name char
            , H.p [] <|
                Render.desc char.bio
            ]
        ]


renderViewDestructible : Array Character -> Destructible -> Html msg
renderViewDestructible characters { amount, dur, skill, user } =
    let
        source =
            Characters.get characters user

        { name } =
            skill
    in
    H.section []
        [ Render.icon source name [ A.class "char" ]
        , H.dl []
            [ H.h4 []
                [ H.text name ]
            , H.dt [] [ H.text "Amount" ]
            , H.dd [] [ H.text <| String.fromInt amount ]
            , H.dt [] [ H.text "Duration" ]
            , H.dd [] [ H.text <| Render.duration "Permanent" dur ]
            , H.dt [] [ H.text "Source" ]
            , H.dd [] <| Render.name source
            ]
        ]


viewIgnoredClasses : Set String
viewIgnoredClasses =
    Set.fromList
        [ "Bypassing"
        , "Uncounterable"
        , "Unreflectable"
        ]


renderViewDetail : Array Character -> (Effect -> Bool) -> Detail -> List (Html msg)
renderViewDetail characters removable detail =
    [ H.section []
        [ Render.detailIcon (Characters.get characters detail.source)
            detail
            [ A.class "char" ]
        , H.dl [] <|
            [ H.h4 [] [ H.span [] [ H.text detail.name ] ]
            , Render.classes <| Set.diff detail.classes viewIgnoredClasses
            , H.dt [] [ H.text "Source" ]
            , H.dd [] <| Render.name <| Characters.get characters detail.user
            , H.dt [] [ H.text "Duration" ]
            , H.dd [] [ H.text <| Render.duration "Permanent" detail.dur ]
            ]
                ++ (if detail.amount > 1 then
                        [ H.dt [] [ H.text "Amount" ]
                        , H.dd [] [ H.text <| String.fromInt detail.amount ]
                        ]

                    else
                        []
                   )
        ]
    , detail.effects
        |> List.filter .visible
        |> List.map (Render.effect characters removable)
        |> H.ul []
    ]


renderAlternateButton : String -> Skill -> Html Msg
renderAlternateButton class skill =
    H.button
        [ A.class <| class ++ " click"
        , E.onClick <| View <| ViewSkill [] 0 { skill | charges = 0 }
        ]
        []


renderViewSkill : Array Character -> Int -> Skill -> List (Html Msg)
renderViewSkill characters charges skill =
    let
        character =
            Characters.get characters skill.owner

        cooldown =
            case skill.cooldown of
                0 ->
                    "None"

                y ->
                    String.fromInt y

        name =
            skill.name

        skillSplit =
            List.findMap (List.splitWhen (\y -> y.name == name)) character.skills
    in
    [ H.section []
        [ H.div [] <|
            Maybe.values
                [ Just <| Render.skillIcon character skill [ A.class "char" ]
                , Maybe.andThen (Tuple.first >> List.last) skillSplit
                    |> Maybe.map (renderAlternateButton "prevSkill")
                , Maybe.andThen (Tuple.second >> List.getAt 1) skillSplit
                    |> Maybe.map (renderAlternateButton "nextSkill")
                ]
        , H.dl []
            [ H.h4 []
                [ H.text skill.name ]
            , Render.classes skill.classes
            , H.dt [] [ H.text "Cost" ]
            , H.dd [] <| Render.skillCost skill.cost
            , H.dt [] [ H.text "Duration" ]
            , H.dd [] [ H.text <| Render.skillDuration skill.dur ]
            , H.dt [] [ H.text "Cooldown" ]
            , H.dd [] [ H.text cooldown ]
            ]
        ]
    , H.p [] <| Render.skillDesc charges skill
    ]


renderViewUser : User -> Html msg
renderViewUser user =
    let
        rank =
            User.rank user
    in
    H.section []
        [ H.img
            [ A.class "char"
            , A.src user.avatar
            ]
            []
        , H.dl []
            [ H.h4 []
                [ H.text user.name ]
            , H.p [ A.class <| String.toLower rank ]
                [ H.text rank ]
            , H.dt [] [ H.text "Clan" ]
            , H.dd [] [ H.text <| Maybe.withDefault "Clanless" user.clan ]
            , H.dt [] [ H.text "Level" ]
            , H.dd [] [ H.text <| String.fromInt <| User.level user ]
            , H.dt [] [ H.text "Record" ]
            , H.dd [] [ Render.userStreak user ]
            ]
        ]


renderView : Array Character -> Viewable -> Html Msg
renderView characters viewing =
    H.article [ A.class "parchment" ] <|
        case viewing of
            ViewCharacter x ->
                [ renderViewCharacter x ]

            ViewDestructible x ->
                [ renderViewDestructible characters x ]

            ViewDetail removable x ->
                renderViewDetail characters removable x

            ViewSkill _ charge x ->
                renderViewSkill characters charge x

            ViewUser x ->
                [ renderViewUser x ]
