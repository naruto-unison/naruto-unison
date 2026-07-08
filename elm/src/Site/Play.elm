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
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2, lazy4)
import Http
import Import.Flags exposing (Flags)
import Import.Model as Model exposing (Chakras, Character, Destructible, Effect, GameInfo, GameMessage(..), Ninja, Player(..), Reward, Skill, Target(..), Turn, User, War(..))
import Json.Decode as D
import List.Extra as List
import Maybe.Extra as Maybe
import Ports exposing (Ports)
import Process
import Set exposing (Set)
import Site.Render as Render
import Sound exposing (Sound)
import Task
import Util exposing (ListChange(..), pure, showErr, toAsciiDigit)


type Viewable
    = ViewCharacter Character
    | ViewDestructible Destructible
    | ViewDetail (Effect -> Bool) Detail
    | ViewSkill Int (Set Int) Int Skill
    | ViewUser User


type alias ChakraSpend =
    { chakra : String
    , spend : Chakras
    , amount : Int
    , random : Int
    }


getChakraSpend : ChakraSums -> Chakras -> List ChakraSpend
getChakraSpend { net, rand } randoms =
    let
        pair : String -> (Chakras -> Int) -> Chakras -> ChakraSpend
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
    , ChakraSpend "rand" none rand 0
    ]


type alias NinjaBundle =
    { character : Character
    , ninja : Ninja
    , targets : List (Set Int)
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
    , targetable : Set Int
    , untargetable : Set Int
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
                    , targetable = Set.empty
                    , untargetable = Set.empty
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

                { victor } =
                    st.game
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
                , if List.isEmpty victor then
                    renderCenter st

                  else
                    renderGameOver st.player st.dna victor
                ]

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
                View ((ViewSkill user targets _ skill) as viewing) ->
                    pure <|
                        if
                            (user >= 0)
                                && Game.allied st.player user
                                && List.any (\t -> t /= Self) skill.targets
                        then
                            { st
                                | viewing = viewing
                                , targetable = targets
                                , untargetable = Set.diff (Skill.affectedSlots user skill) targets
                            }

                        else
                            { st | viewing = viewing }

                View viewing ->
                    pure { st | viewing = viewing }

                DoNothing ->
                    pure st

                Unhighlight ->
                    pure
                        { st
                            | targetable = Set.empty
                            , untargetable = Set.empty
                        }

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
            [ H.h3 [] [ H.text user.name ]
            , H.p [] [ H.text user.rank ]
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


renderCenter : Model -> Html Msg
renderCenter { acts, chakraSums, exchange, exchanged, ownTurn, ninjas, randoms } =
    let
        { free, net, rand } =
            chakraSums

        chakraPairs =
            getChakraSpend chakraSums randoms
    in
    H.div [ A.id "center" ]
        [ Keyed.node "div" [ A.id "playqueue" ] <|
            List.map (renderAct ninjas) acts
        , renderReadyButton ownTurn <| rand /= 0
        , H.div [ A.id "spend" ] <|
            H.div [ A.class "space" ] []
                :: List.concatMap (renderChakraSpend ownTurn exchange net) chakraPairs
                ++ [ H.div [ A.class "space" ] [] ]
        , H.button
            [ A.id "exchange"
            , E.onClick <| Exchange Begin
            , A.disabled <|
                not ownTurn
                    || (free.rand < Chakras.rate)
                    || not (Chakras.canExchange net)
            ]
            [ H.text "Exchange" ]
        , H.button
            [ A.id "reset"
            , E.onClick <| Exchange Reset
            , A.disabled <| exchanged == Chakras.none && randoms == Chakras.none
            ]
            [ H.text "Reset" ]
        , H.button
            [ A.id "forfeit"
            , E.onClick Forfeit
            , A.disabled <| not ownTurn
            ]
            [ H.text "Forfeit" ]
        ]


renderChakraSpend : Bool -> Bool -> Chakras -> ChakraSpend -> List (Html Msg)
renderChakraSpend turn exchange chakras { chakra, spend, amount, random } =
    [ H.div []
        [ H.button
            [ A.class "more"
            , E.onClick <| Spend <| Chakras.negate spend
            , A.disabled <| not turn || random <= 0
            ]
            []
        , H.button
            [ A.class <| "chakra " ++ chakra
            , E.onClick <| Exchange <| Conclude spend
            , A.disabled <| not <| exchange && Chakras.affordable chakras spend
            ]
            []
        , H.button
            [ A.class "less"
            , E.onClick <| Spend spend
            , A.disabled <| not turn || amount <= 0 || chakra == "rand"
            ]
            []
        ]
    , H.span [] [ H.text <| String.fromInt amount ]
    ]


renderReadyButton : Bool -> Bool -> Html Msg
renderReadyButton ownTurn noChakra =
    H.button
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


renderAct : Array Character -> Act -> ( String, Html Msg )
renderAct characters ({ skill, user } as act) =
    ( String.fromChar <| toAsciiDigit user
    , H.button
        [ A.class "act"
        , E.onClick <| Enact Delete act
        ]
        [ Render.skillIcon (Characters.root characters skill) skill []
        , H.div [ A.class "actcost" ] <|
            Render.chakras skill.cost
        ]
    )


renderDna : Reward -> List (Html Msg)
renderDna { amount, reason } =
    [ H.dt [] [ H.text reason ]
    , H.dd [ A.class "dna" ] [ H.text <| String.fromInt amount ]
    ]


renderGameOver : Player -> List Reward -> List Player -> Html Msg
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
    H.div [ A.id "endgame" ]
        [ H.p [] [ H.text message ]
        , H.a
            [ A.id "return"
            , A.class "playButton parchment"
            , A.href "/"
            ]
            [ H.text "Return" ]
        , H.dl [] <| List.concatMap renderDna dna
        ]



-- NINJA


renderHealth : String -> Int -> List ( String, Html msg )
renderHealth anchor health =
    [ ( "healthtext"
      , H.div
            [ A.class "charhealth"
            , A.style "width" <| String.fromInt health ++ "%"
            ]
            []
      )
    , ( "health"
      , H.span [ A.style anchor (String.fromInt (health * 93 // 100) ++ "%") ] <|
            if health /= 0 then
                [ H.text <| String.fromInt health ]

            else
                []
      )
    ]


renderDestructible : String -> String -> Int -> Destructible -> ( String, Html Msg )
renderDestructible anchor class track x =
    ( class ++ String.cons (toAsciiDigit x.user) x.skill.name
    , H.div
        [ A.classList
            [ ( class, True )
            , ( "ghost", x.dur == Just 0 )
            ]
        , A.style anchor <| String.fromInt track ++ "%"
        , A.style "width" <| String.fromInt x.amount ++ "%"
        , E.onMouseOver <| View <| ViewDestructible x
        ]
        []
    )


renderHpBar : String -> Ninja -> List ( String, Html Msg )
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
    String.cons (toAsciiDigit owner) name


renderSkill :
    SkillData
    -> Int
    -> Set Int
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
                || Set.isEmpty targets
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

            else if Skill.isTargeted skill then
                Toggle act

            else
                Enact Add act
    in
    H.button
        [ A.class "charmove"
        , E.onMouseOver <| View <| ViewSkill slot targets charge skill
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
        , H.p []
            [ H.text <|
                if Set.member "Continues" classes then
                    "•"

                else
                    Render.duration "\u{00A0}" detail.dur
            ]
        ]


type alias NinjaData =
    { characters : Array Character
    , acted : Set Int
    , toggle : Maybe Act
    , toggled : Set Int
    , targetable : Set Int
    , untargetable : Set Int
    , freeChakras : Chakras
    , ownTurn : Bool
    }


createNinjaData : Model -> NinjaData
createNinjaData { acts, chakraSums, targetable, untargetable, ownTurn, ninjas, toggled } =
    { characters = ninjas
    , acted = Set.fromList <| List.map .user acts
    , toggle = toggled
    , targetable = targetable
    , untargetable = untargetable
    , freeChakras = chakraSums.free
    , ownTurn = ownTurn
    , toggled =
        case toggled of
            Just act ->
                Act.targetSlots act

            Nothing ->
                Set.empty
    }


renderNinja :
    NinjaData
    -> Bool
    -> NinjaBundle
    -> Html Msg
renderNinja { characters, acted, toggled, toggle, targetable, untargetable, freeChakras, ownTurn } onTeam { character, ninja, targets } =
    let
        anchor =
            if onTeam then
                "left"

            else
                "right"

        { slot } =
            ninja

        faceIcon =
            case ninja.face of
                Nothing ->
                    Render.charIcon character

                Just face ->
                    Render.icon (Characters.get characters face.user) <| "icon" ++ face.icon

        skillData =
            { user = ninja
            , freeChakras = freeChakras
            , characters = characters
            , active =
                onTeam
                    && ownTurn
                    && (ninja.health > 0)
                    && not (Set.member slot acted)
            }

        render =
            renderDetail onTeam slot characters

        renderDetails attrs els =
            H.aside attrs <| List.map render els
    in
    H.section []
        [ renderDetails [ A.class "channels" ] <|
            List.map Detail.copy (Maybe.values ninja.copies)
                ++ List.map (Detail.channel slot) ninja.channels
        , H.button
            [ A.classList
                [ ( "face", True )
                , ( "dead", ninja.health == 0 )
                , ( "targetable", Set.member slot targetable )
                , ( "untargetable", Set.member slot untargetable )
                ]
            , E.onMouseOver <| View <| ViewCharacter character
            , case toggle of
                Just act ->
                    if Set.member slot toggled then
                        E.onClick <| Enact Add { act | target = slot }

                    else
                        A.disabled True

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
        , Keyed.node "div" [ A.class "charhealthbar" ] <|
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
            [ H.h4 [] <| Render.name char
            , H.p [] <| Render.desc char.bio
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
            [ H.h4 [] [ H.text name ]
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
        [ Render.detailIcon (Characters.get characters detail.source) detail [ A.class "char" ]
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


renderAlternateButton : String -> Maybe Skill -> Html Msg
renderAlternateButton class mskill =
    H.button
        [ A.class class
        , case mskill of
            Just skill ->
                E.onClick <| View <| ViewSkill -1 Set.empty 0 { skill | charges = 0 }

            Nothing ->
                A.hidden True
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
        [ H.div []
            [ Render.skillIcon character skill [ A.class "char" ]
            , renderAlternateButton "prevSkill" <|
                Maybe.andThen (Tuple.first >> List.last) skillSplit
            , renderAlternateButton "nextsSkill" <|
                Maybe.andThen (Tuple.second >> List.getAt 1) skillSplit
            ]
        , H.dl []
            [ H.h4 [] [ H.text skill.name ]
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
renderViewUser ({ avatar, clan, level, name, rank } as user) =
    H.section []
        [ H.img
            [ A.class "char"
            , A.src avatar
            ]
            []
        , H.dl []
            [ H.h4 [] [ H.text name ]
            , H.p [ A.class <| String.toLower rank ]
                [ H.text rank ]
            , H.dt [] [ H.text "Clan" ]
            , H.dd [] [ H.text <| Maybe.withDefault "Clanless" clan ]
            , H.dt [] [ H.text "Level" ]
            , H.dd [] [ H.text <| String.fromInt level ]
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

            ViewSkill _ _ charge x ->
                renderViewSkill characters charge x

            ViewUser x ->
                [ renderViewUser x ]
