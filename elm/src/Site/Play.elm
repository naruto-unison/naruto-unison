module Site.Play exposing (Model, Msg(..), component)

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
import Util exposing (ListChange(..), clickIf, pure, showErr)


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


nullChakraSums : ChakraSums
nullChakraSums =
    { free = Chakras.none, net = Chakras.none, rand = 0 }


sumChakras : Model -> ChakraSums
sumChakras st =
    let
        costs =
            st.acts
                |> List.map (.skill >> .cost)
                >> Chakras.sum

        net =
            Chakras.sum [ st.exchanged, st.chakras, Chakras.negate costs ]

        netUnrand =
            { net | rand = 0 }

        rand =
            Chakras.total st.randoms
                + net.rand
                - Chakras.rate
                * Chakras.total st.exchanged

        free =
            { net | rand = Chakras.total netUnrand + rand }
    in
    { free = free, net = net, rand = rand }


type alias Model =
    { url : String
    , practice : Bool
    , player : Player
    , user : User
    , vs : User
    , characters : Characters
    , game : Turn
    , ninjas : List Character
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


setGame : Turn -> Model -> Model
setGame game st =
    recalculateChakra
        { st
            | game = game
            , chakras = game.chakra
            , ninjas = List.map (Characters.merge st.characters) game.ninjas
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


createUrl : (a -> List Int) -> List a -> List String
createUrl toPathPieces =
    List.map (toPathPieces >> List.map String.fromInt >> String.join ",")


enactUrl : Model -> String
enactUrl st =
    let
        chakras =
            createUrl Chakras.toPathPieces [ st.randoms, st.exchanged ]

        acts =
            createUrl Act.toPathPieces st.acts
    in
    String.join "/" <| chakras ++ acts


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
                    , ninjas = []
                    , chakras = info.turn.chakra
                    , randoms = Chakras.none
                    , exchanged = Chakras.none
                    , chakraSums = nullChakraSums
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
                ( left, right ) =
                    List.map3 NinjaBundle
                        st.ninjas
                        st.game.ninjas
                        st.game.targets
                        |> List.splitAt Game.teamSize

                ( allies, enemies ) =
                    case st.player of
                        A ->
                            ( left, right )

                        B ->
                            ( right, left )

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
            let
                untoggled =
                    { st | toggled = Nothing }
            in
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
                            untoggled

                        else
                            { st | toggled = Just skill }

                Enact Add act ->
                    withSound Sound.ApplySkill <|
                        recalculateChakra
                            { untoggled | acts = st.acts ++ [ act ] }

                Enact Delete act ->
                    withSound Sound.Cancel <|
                        recalculateChakra
                            { untoggled | acts = List.remove act st.acts }

                Spend chakras ->
                    withSound Sound.Click <|
                        recalculateChakra
                            { st
                                | randoms =
                                    Chakras.sum [ st.randoms, chakras ]
                                , chakras =
                                    Chakras.sum [ st.chakras, Chakras.negate chakras ]
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
                                | exchanged = Chakras.sum [ st.exchanged, chakras ]
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
                            { untoggled
                                | exchange = False
                                , exchanged = Chakras.none
                            }
                        , Http.get
                            { url =
                                st.url ++ "api/practiceact/" ++ enactUrl st
                            , expect =
                                Http.expectJson ReceivePractice <|
                                    D.list Model.jsonDecTurn
                            }
                        )

                    else
                        ( st
                        , Cmd.batch
                            [ ports.sound Sound.StartTurn
                            , ports.websocket <| enactUrl st
                            ]
                        )

                ReceivePractice (Ok [ x, y ]) ->
                    setGameAnd x
                        st
                        [ ports.progress 1500 0 1
                        , Process.sleep 1500
                            |> Task.perform
                                (always << ReceivePractice <| Ok [ y ])
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


renderTop : Model -> List Character -> Html Msg
renderTop st characters =
    let
        vsWar =
            Maybe.map warInverse st.war

        ( playerInactive, vsInactive ) =
            st.game.inactive
    in
    H.section [ A.id "top" ]
        [ lazy4 renderUserBox "account0" st.user st.war playerInactive
        , lazy2 renderView characters st.viewing
        , lazy4 renderUserBox "account1" st.vs vsWar vsInactive
        ]


renderUserBox : String -> User -> Maybe War -> Int -> Html Msg
renderUserBox id user war inactive =
    H.section
        [ A.id id
        , E.onMouseOver << View <| ViewUser user
        ]
        [ H.section []
            [ H.h3 []
                [ H.text user.name ]
            , H.p []
                [ H.text <| User.rank user ]
            , H.p [ A.class "inactive" ] << List.repeat inactive <| H.text "X"
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
    if List.isEmpty st.game.victor then
        [ renderChakraModule st
        , renderActs st
        ]

    else
        renderGameOver st.player st.dna st.game.victor


renderChakraButton : String -> msg -> Bool -> Html msg
renderChakraButton text msg condition =
    H.button
        (A.id text :: clickIf condition "chakraButton" msg)
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
    let
        classes =
            "chakra " ++ chakra

        meta =
            if exchange then
                clickIf (Chakras.affordable chakras spend) classes <|
                    Exchange (Conclude spend)

            else
                [ A.class classes ]
    in
    H.div []
        [ H.div meta []
        , H.span []
            [ H.text <| String.fromInt amount ]
        , H.button (clickIf (turn && random > 0) "more" << Spend <| Chakras.negate spend)
            [ H.text "+" ]
        , H.button (clickIf (turn && amount > 0) "less" <| Spend spend)
            [ H.text "—" ]
        , H.div [ A.class "chakra rand" ] []
        , H.span []
            [ H.text <| String.fromInt random ]
        ]


renderActs : Model -> Html Msg
renderActs { ownTurn, chakraSums, ninjas, acts } =
    let
        readyMeta =
            if not ownTurn then
                [ A.class "noclick" ]

            else if chakraSums.rand /= 0 then
                [ A.class "noChakra" ]

            else
                [ A.class "click", E.onClick Ready ]
    in
    H.section [ A.id "playqueuecont" ]
        [ H.div [ A.id "playqueue" ] <|
            List.map (renderAct ninjas) acts
        , H.div (A.id "ready" :: readyMeta) []
        ]


renderAct : List Character -> Act -> Html Msg
renderAct characters x =
    let
        skill =
            x.skill
    in
    H.div [ A.class "act click", E.onClick <| Enact Delete x ]
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
            , A.class "playButton parchment click"
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
        , A.style anchor <|
            String.fromInt (health * 93 // 100)
                ++ "%"
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
        , E.onMouseOver << View <| ViewDestructible x
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
        >> renderAll "charbarrier" barrier
        >> Tuple.first


type alias SkillData =
    { user : Ninja
    , freeChakras : Chakras
    , active : Bool
    , characters : List Character
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
        slot =
            user.slot

        key =
            skillKey skill

        icon =
            Render.skillIcon (Characters.root characters skill) skill []

        charge =
            Dict.get key user.charges
                |> Maybe.withDefault 0

        noclick =
            not active
                || List.isEmpty targets
                || Chakras.lacks freeChakras skill.cost
    in
    if noclick then
        let
            cooldown =
                if user.health > 0 && skill.cooldown > 0 then
                    Dict.get key user.cooldowns
                        |> Maybe.withDefault 0

                else
                    0
        in
        H.div
            [ A.class "charmove noclick"
            , E.onMouseOver << View <| ViewSkill [] charge skill
            , E.onMouseLeave Unhighlight
            ]
        <|
            if cooldown <= 0 then
                [ icon ]

            else
                [ icon
                , H.span [] [ H.text <| String.fromInt cooldown ]
                ]

    else
        let
            act : Act
            act =
                { user = slot
                , skill = skill
                , target = slot
                , button = button
                , targets = targets
                }
        in
        H.div
            [ A.class "charmove click"
            , E.onMouseOver << View <| ViewSkill targets charge skill
            , E.onMouseLeave Unhighlight
            , E.onClick <|
                if Skill.targets slot skill == [ slot ] then
                    Enact Add act

                else
                    Toggle act
            ]
            [ icon ]


renderDetail : Bool -> Int -> List Character -> Detail -> Html Msg
renderDetail onTeam slot characters detail =
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
        [ E.onMouseOver << View <| ViewDetail removable detail
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
                    && not (Set.member "Unremovable" detail.classes)
              )
            , ( "invis"
              , Set.member "Invisible" detail.classes
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
            if Set.member "Continues" detail.classes then
                [ H.text "•" ]

            else
                [ H.text <| Render.duration "\u{00A0}" detail.dur ]
        ]


type alias NinjaData =
    { characters : List Character
    , acted : List Int
    , toggle : Maybe Act
    , highlight : List Int
    , freeChakras : Chakras
    , ownTurn : Bool
    }


createNinjaData : Model -> NinjaData
createNinjaData st =
    { characters = st.ninjas
    , acted = List.map .user st.acts
    , toggle = st.toggled
    , highlight = st.highlight
    , freeChakras = st.chakraSums.free
    , ownTurn = st.ownTurn
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

        toggled =
            List.member ninja.slot (Act.toggles toggle)

        fullMeta =
            let
                mainMeta =
                    [ A.classList
                        [ ( "highlighted", List.member ninja.slot highlight )
                        , ( "toggled skill", toggled )
                        ]
                    , E.onMouseOver << View <| ViewCharacter character
                    ]

                onClick =
                    toggle
                        |> Maybe.filter (always toggled)
                        >> Maybe.map
                            (E.onClick << Enact Add << Act.targeted ninja.slot)
            in
            case onClick of
                Just onclick ->
                    onclick :: mainMeta

                Nothing ->
                    mainMeta

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
                    && not (List.member ninja.slot acted)
            }

        render =
            renderDetail onTeam ninja.slot characters

        renderDetails attrs =
            H.aside attrs << List.map render
    in
    H.section [ A.classList [ ( "dead", ninja.health == 0 ) ] ]
        [ renderDetails [ A.class "channels" ] <|
            List.map Detail.copy (Maybe.values ninja.copies)
                ++ List.map (Detail.channel ninja.slot) ninja.channels
        , H.section fullMeta
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


renderViewDestructible : List Character -> Destructible -> Html msg
renderViewDestructible characters { amount, dur, skill, user } =
    let
        source =
            Characters.get characters user

        name =
            skill.name
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


renderViewDetail : List Character -> (Effect -> Bool) -> Detail -> List (Html msg)
renderViewDetail characters removable detail =
    [ H.section []
        [ Render.detailIcon (Characters.get characters detail.source)
            detail
            [ A.class "char" ]
        , H.dl [] <|
            [ H.h4 [] [ H.span [] [ H.text detail.name ] ]
            , Render.classes <| Set.diff detail.classes viewIgnoredClasses
            , H.dt [] [ H.text "Source" ]
            , H.dd [] << Render.name <| Characters.get characters detail.user
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
        >> List.map (Render.effect characters removable)
        >> H.ul []
    ]


renderAlternateButton : String -> Skill -> Html Msg
renderAlternateButton class skill =
    H.button
        [ A.class <| class ++ " click"
        , E.onClick
            << View
          <|
            ViewSkill []
                0
                { skill | charges = 0 }
        ]
        []


renderViewSkill : List Character -> Int -> Skill -> List (Html Msg)
renderViewSkill characters charge skill =
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
    , H.p [] <|
        if skill.charges == 0 then
            Render.desc skill.desc

        else
            Render.desc skill.desc
                ++ [ H.span [ A.class "extra" ]
                        [ H.text <|
                            case skill.charges - charge of
                                1 ->
                                    "1 charge."

                                y ->
                                    String.fromInt y ++ " charges."
                        ]
                   ]
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
            , H.dd [] [ H.text << String.fromInt <| User.level user ]
            , H.dt [] [ H.text "Record" ]
            , H.dd [] [ Render.streak user ]
            ]
        ]


renderView : List Character -> Viewable -> Html Msg
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
