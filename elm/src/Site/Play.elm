module Site.Play exposing (Model, Msg(..), component)

import Dict
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Lazy exposing (lazy3)
import Http
import Json.Decode as D
import List.Extra as List
import Maybe.Extra as Maybe
import Process
import Set exposing (Set)
import Task

import Game.Chakra as Chakra exposing (none)
import Game.Detail as Detail exposing (Detail)
import Game.Game as Game exposing (Act)
import Import.Flags exposing (Characters, Flags, printFailure)
import Import.Model as Model exposing (Barrier, Chakras, Channeling(..), Character, Defense, Effect, GameInfo, Message(..), Ninja, Player(..), Requirement(..), Reward, Skill, Turn, User, War(..))
import Ports exposing (Ports)
import Site.Render as Render exposing (icon)
import Sound exposing (Sound)
import Util exposing (ListChange(..), elem, pure, showErr)


type Viewable
    = ViewBarrier Barrier
    | ViewCharacter Character
    | ViewDefense Defense
    | ViewDetail (Effect -> Bool) Detail
    | ViewSkill (List Int) Int Skill
    | ViewUser User


type ChakraPair
    = ChakraPair String Chakras Int Int


type alias Bundle =
    { character : Character
    , ninja     : Ninja
    , targets   : List (List Int)
    }


type alias Model =
    { url        : String
    , practice   : Bool
    , player     : Player
    , user       : User
    , vs         : User
    , characters : Characters
    , game       : Turn
    , chakras    : Chakras
    , randoms    : Chakras
    , exchanged  : Chakras
    , exchange   : Bool
    , viewing    : Viewable
    , highlight  : List Int
    , toggled    : Maybe Act
    , acts       : List Act
    , dna        : List Reward
    , visibles   : Set String
    , war        : Maybe War
    , error      : String
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


component :
    Ports Msg
    ->
        { init   : Flags -> Bool -> GameInfo -> Model
        , update : Msg -> Model -> ( Model, Cmd Msg )
        , view   : Model -> Html Msg
        }
component ports =
    let
        init : Flags -> Bool -> GameInfo -> Model
        init flags practice info =
            { url        = flags.url
            , practice   = practice
            , player     = info.player
            , user       = Maybe.withDefault info.opponent flags.user
            , vs         = info.opponent
            , characters = flags.characters
            , game       = info.turn
            , chakras    = info.turn.chakra
            , randoms    = Chakra.none
            , exchanged  = Chakra.none
            , exchange   = False
            , viewing    = ViewUser info.opponent
            , highlight  = []
            , toggled    = Nothing
            , acts       = []
            , dna        = []
            , visibles   = flags.visibles
            , war        = info.war
            , error      = ""
            }

        view : Model -> Html Msg
        view st =
            let
                characters =
                    st.game.ninjas
                        |> List.map (Game.merge st.characters)

                costs =
                    st.acts
                        |> List.map (.skill >> .cost)
                        >> Chakra.sum

                net =
                    Chakra.sum [ st.exchanged, st.chakras, Chakra.negate costs ]

                netUnrand =
                    { net | rand = 0 }

                ownTurn =
                    st.player == st.game.playing

                rand =
                    Chakra.total st.randoms
                        + net.rand
                        - Chakra.rate
                        * Chakra.total st.exchanged

                free =
                    { net | rand = Chakra.total netUnrand + rand }

                acted =
                    List.map .user st.acts

                renderNinja =
                    renderCharacter
                        characters
                        acted
                        st.toggled
                        st.highlight
                        free
                        ownTurn

                ( left, right ) =
                    List.map3 Bundle characters st.game.ninjas st.game.targets
                        |> List.splitAt Game.teamSize

                ( allies, enemies ) =
                    case st.player of
                        A -> ( left, right )
                        B -> ( right, left )
            in
            H.div [ A.id "game"
                  , A.classList [ ("over", not <| List.isEmpty st.game.victor) ]
                  ] <|
            [ H.div [ A.id "error" ]
              [ H.text st.error ]
            , renderTop st characters
            , H.section [ A.id "player0", A.class "player" ] <|
              List.map (renderNinja True) allies
            , H.section [ A.id "player1", A.class "player" ] <|
              List.map (renderNinja False) enemies
            ] ++ case st.game.victor of
                [] ->
                    renderChakraModule st characters ownTurn free net rand

                [ victor ] ->
                    renderVictoryModule st victor

                _ ->
                    renderTieModule st

        withSound : Sound -> Model -> ( Model, Cmd Msg )
        withSound sfx st =
            ( st, ports.sound sfx )

        setGameAnd : Turn -> Model -> List (Cmd Msg) -> ( Model, Cmd Msg )
        setGameAnd game st cmds =
            ( { st
                | game      = game
                , chakras   = game.chakra
                , randoms   = Chakra.none
                , exchanged = Chakra.none
                , acts      = []
              }
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
                    withSound Sound.ApplySkill
                        { untoggled | acts = st.acts ++ [ act ] }

                Enact Delete act ->
                    withSound Sound.Cancel
                        { untoggled | acts = List.remove act st.acts }

                Spend chakras ->
                    withSound Sound.Click
                        { st
                            | randoms =
                                Chakra.sum [ st.randoms, chakras ]
                            , chakras =
                                Chakra.sum [ st.chakras, Chakra.negate chakras ]
                        }

                Exchange Begin ->
                    withSound Sound.Target
                        { st | exchange = not st.exchange }

                Exchange Reset ->
                    withSound Sound.Cancel
                        { st
                            | chakras   = st.game.chakra
                            , randoms   = Chakra.none
                            , exchanged = Chakra.none
                            , exchange  = False
                        }

                Exchange (Conclude chakras) ->
                    withSound Sound.Click
                        { st
                            | exchanged = Chakra.sum [ st.exchanged, chakras ]
                            , exchange  = False
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
                    case D.decodeString Model.jsonDecMessage json of
                        Err err ->
                            pure { st | error = D.errorToString err }

                        Ok (Fail failure) ->
                            pure { st | error = printFailure failure }

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

                        Ok _ ->
                            pure st

                Ready ->
                    if st.practice then
                        ( { untoggled
                            | exchange  = False
                            , exchanged = Chakra.none
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


renderTop : Model -> List Character -> Html Msg
renderTop st characters =
    let
        ( playerInactive, vsInactive ) = st.game.inactive
    in
    H.section [ A.id "top" ]
    [ H.section
      [ A.id "account0"
      , E.onMouseOver << View <| ViewUser st.user
      ]
      [ H.section []
        [ H.h3 []
          [ H.text st.user.name ]
        , H.p []
          [ H.text <| Game.rank st.user ]
        , H.p [A.class "inactive"] << List.repeat playerInactive <| H.text "X"
        ]
      , H.div [ A.class "charWrapper" ]
        [ H.img [ A.class "charicon", A.src st.user.avatar ] []
        , case st.war of
              Just Red  -> H.div [A.class "red"] []
              Just Blue -> H.div [A.class "blue"] []
              Nothing   -> H.div [] []
        ]
      ]
    , lazy3 renderView st.visibles characters st.viewing
    , H.section
      [ A.id "account1"
      , E.onMouseOver << View <| ViewUser st.vs
      ]
      [ H.div [ A.class "charWrapper" ]
        [ H.img [ A.class "charicon", A.src st.vs.avatar ] []
        , case st.war of
              Just Blue -> H.div [A.class "red"] []
              Just Red  -> H.div [A.class "blue"] []
              Nothing   -> H.div [] []
        ]
      , H.section []
        [ H.h3 []
          [ H.text st.vs.name ]
        , H.p []
          [ H.text <| Game.rank st.vs ]
        , H.p [A.class "inactive"] << List.repeat vsInactive <| H.text "X"
        ]
      ]
    ]


renderChakraModule :
    Model
    -> List Character
    -> Bool
    -> Chakras
    -> Chakras
    -> Int
    -> List (Html Msg)
renderChakraModule st characters ownTurn free net rand =
    let
        chakraButton text msg condition =
            H.button
                (A.id text :: clickIf condition "chakraButton" msg)
                [ H.text text ]

        pair name get chakra =
            ChakraPair name chakra (get net) (get st.randoms)

        chakraPairs =
            [ pair "blood" .blood { none | blood = 1 }
            , pair "gen"   .gen   { none | gen = 1 }
            , pair "nin"   .nin   { none | nin = 1 }
            , pair "tai"   .tai   { none | tai = 1 }
            ]

        readyMeta =
            if not ownTurn then
                [ A.id "ready", A.class "noclick" ]

            else if rand /= 0 then
                [ A.id "ready", A.class "noChakra" ]

            else
                [ A.id "ready"
                , A.class "click"
                , E.onClick Ready
                ]
    in
    [ H.section [ A.id "playchakra" ] <|
      List.map (renderChakra ownTurn st.exchange net) chakraPairs ++
      [ Render.rands (Chakra.total { net | rand = 0 }) rand
      , chakraButton "exchange" (Exchange Begin) <|
          free.rand >= Chakra.rate && Chakra.canExchange net && ownTurn
      , chakraButton "reset" (Exchange Reset) <|
          st.exchanged /= Chakra.none || st.randoms /= Chakra.none
      , chakraButton "forfeit" Forfeit ownTurn
      ]
    , H.section [ A.id "playqueuecont" ]
      [ H.div [ A.id "playqueue" ] <|
        List.map (renderAct characters) st.acts
      , H.div readyMeta []
      ]
    ]


renderVictoryModule : Model -> Player -> List (Html Msg)
renderVictoryModule st victor =
    [ H.div [ A.id "endgame" ]
      [ H.p []
        [ H.text <| if victor == st.player then "Victory" else "Defeat" ]
      , H.a
        [ A.id "return"
        , A.class "playButton parchment click"
        , A.href "/"
        ]
        [ H.text "Return" ]
      , H.dl [] <| List.concatMap renderDna st.dna
      ]
    ]


renderTieModule : Model -> List (Html Msg)
renderTieModule st =
    [ H.div [ A.id "endgame" ]
      [ H.p []
        [ H.text "Tie" ]
      , H.a
        [ A.id "return"
        , A.class "playButton parchment click"
        , A.href "/"
        ]
        [ H.text "Return" ]
      , H.dl [] <| List.concatMap renderDna st.dna
      ]
    ]


enactUrl : Model -> String
enactUrl st =
    let
        chakras =
            toUrl [ st.randoms, st.exchanged ] <|
                \x -> [ x.blood, x.gen, x.nin, x.tai ]

        acts =
            toUrl st.acts <|
                \x -> [ x.user, x.button, x.target ]
    in
    String.join "/" <| chakras ++ acts


toUrl : List a -> (a -> List Int) -> List String
toUrl xs f =
    List.map (f >> List.map String.fromInt >> String.join ",") xs


clickIf : Bool -> String -> msg -> List (H.Attribute msg)
clickIf succeeds class command =
    if succeeds then
        [ A.class <| class ++ " click", E.onClick command ]

    else
        [ A.class <| class ++ " noclick" ]


renderDna : Reward -> List (Html Msg)
renderDna x =
    [ H.dt [] [ H.text x.reason ]
    , H.dd [ A.class "dna" ] [ H.text <| String.fromInt x.amount ]
    ]


renderChakra : Bool -> Bool -> Chakras -> ChakraPair -> Html Msg
renderChakra turn exchange chakras (ChakraPair chakra spend amount random) =
    let
        classes =
            "chakra " ++ chakra

        meta =
            if exchange then
                clickIf (Chakra.affordable chakras spend) classes
                    << Exchange <| Conclude spend

            else
                [ A.class classes ]
    in
    H.div []
    [ H.div meta []
    , H.span []
      [ H.text <| String.fromInt amount ]
    , H.a (clickIf (turn && random > 0) "more" << Spend <| Chakra.negate spend)
      [ H.text "+" ]
    , H.a (clickIf (turn && amount > 0) "less" <| Spend spend)
      [ H.text "—" ]
    , H.div [ A.class "chakra rand" ] []
    , H.span []
      [ H.text <| String.fromInt random ]
    ]


renderAct : List Character -> Act -> Html Msg
renderAct characters x =
    H.div [ A.class "act click", E.onClick <| Enact Delete x ]
    [ icon (Game.root characters x.skill) x.skill.name []
    , H.div [ A.class "actcost" ] <|
      Render.chakras x.skill.cost
    ]


renderBarrier : Int -> String -> Int -> List Barrier -> List (Html Msg)
renderBarrier slot anchor track barriers =
    case List.uncons barriers of
        Nothing ->
            []

        Just ( x, xs ) ->
            H.div [ A.class "charbarrier"
                  , A.style anchor <| String.fromInt track ++ "%"
                  , A.style "width" <| String.fromInt x.amount ++ "%"
                  , E.onMouseOver << View <| ViewBarrier x
                  ] []
            :: renderBarrier slot anchor (track + x.amount) xs


renderDefense :
    Int
    -> String
    -> Int
    -> List Barrier
    -> List Defense
    -> List (Html Msg)
renderDefense slot anchor track barriers defenses =
    case List.uncons defenses of
        Nothing ->
            renderBarrier slot anchor track barriers

        Just ( x, xs ) ->
            H.div [ A.classList [ ( "chardefense", True )
                                , ( "ghost", x.dur == -1 )
                                ]
                  , A.style anchor <| String.fromInt track ++ "%"
                  , A.style "width" <| String.fromInt x.amount ++ "%"
                  , E.onMouseOver << View <| ViewDefense x
                  ] []
            :: renderDefense slot anchor (track + x.amount) barriers xs


renderSkill :
    Ninja
    -> Chakras
    -> Bool
    -> List Character
    -> Int
    -> List Int
    -> Skill
    -> Html Msg
renderSkill user chakras able characters button targets skill =
    let
        key =
            String.cons (Char.fromCode <| skill.owner + 48) skill.name
        image =
            icon (Game.root characters skill) skill.name []
        cooldown =
            if user.health > 0 then
                Dict.get key user.cooldowns
                    |> Maybe.withDefault 0

            else
                0
        charge =
            Dict.get key user.charges
                |> Maybe.withDefault 0

    in
    if not able
        || List.isEmpty targets
        || Chakra.lacks chakras skill.cost
    then
        H.div [ A.class "charmove noclick"
              , E.onMouseOver << View <| ViewSkill [] charge skill
              , E.onMouseLeave Unhighlight
              ] <|
        image ::
        ( if cooldown <= 0 then
            []

        else
            [ H.span [] [ H.text << String.fromInt << max 1 <| cooldown // 2 ] ]
        )

    else
        let
            toggler =
                if Game.targets user.slot skill == [ user.slot ] then
                    Enact Add

                else
                    Toggle
        in
        H.div
        [ A.class "charmove click"
        , E.onMouseOver << View <| ViewSkill targets charge skill
        , E.onMouseLeave Unhighlight
        , E.onClick <|
            toggler
                { user    = user.slot
                , skill   = skill
                , target  = user.slot
                , button  = button
                , targets = targets
                }
          ]
        [ image ]


renderDetail : Bool -> Int -> List Character -> Detail -> Html Msg
renderDetail team slot characters detail =
    let
        removable =
            if Detail.allied slot detail then
                always False

            else
                Game.removable team

        amount xs =
            if detail.amount > 1 then
                H.span [] [ H.text <| String.fromInt detail.amount ] :: xs

            else
                xs
    in
    H.div [ E.onMouseOver << View <| ViewDetail removable detail
          , A.classList
            [ ( "detail"
              , True
              )
            , ( "trap"
              , detail.trap
              )
            , ( "ghost"
              , detail.dur == -1
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
          amount [ icon (Game.get characters detail.source) detail.name [] ]
        , H.p [] <|
          if Set.member "Continues" detail.classes then
              [ H.text "\u{2022}"]

          else
              Render.duration "\u{00A0}" detail.dur
        ]


renderCharacter :
    List Character
    -> List Int
    -> Maybe Act
    -> List Int
    -> Chakras
    -> Bool
    -> Bool
    -> Bundle
    -> Html Msg
renderCharacter characters acted toggle highlighted chakras turn onTeam b =
    let
        render =
            renderDetail onTeam b.ninja.slot characters

        anchor =
            if onTeam then "left" else "right"

        live xs =
            if b.ninja.health > 0 then xs else []

        channels =
            b.ninja.channels
                |> List.reverse
                >> List.map (Detail.channel b.ninja.slot >> render)
                >> live

        copies =
            []
            {-
            b.ninja.copies
                |> List.filterMap identity
                >> List.reverse
                >> List.map (Detail.copy >> render)
                >> live
                -}

        defenses =
            renderDefense b.ninja.slot
                anchor
                b.ninja.health
                (List.reverse b.ninja.barrier)
                (List.reverse b.ninja.defense)
                |> live

        details =
            Detail.get b.ninja
                |> (if onTeam then List.reverse else identity)
                >> List.map render
                >> live

        active =
            onTeam
                && turn
                && b.ninja.health > 0
                && not (b.ninja.slot |> elem acted)

        toggled =
            b.ninja.slot |> elem (Game.toggles toggle)

        mainMeta =
            [ A.classList
              [ ( "highlighted", b.ninja.slot |> elem highlighted )
              , ( "toggled skill", toggled )
              ]
            , E.onMouseOver << View <| ViewCharacter b.character
            ]

        fullMeta =
            case toggle of
                Just tog ->
                    if toggled then
                        (E.onClick <| Enact Add { tog | target = b.ninja.slot })
                            :: mainMeta

                    else
                        mainMeta

                Nothing ->
                    mainMeta

        faceIcon =
            case b.ninja.face of
                Nothing ->
                    icon b.character "icon"

                Just face ->
                    icon (Game.get characters face.user) <| "icon" ++ face.icon

        mainBar =
            (if onTeam then identity else List.reverse ) <|
            [ H.section fullMeta
              [ faceIcon [ A.class "charicon" ] ]
            , H.div [ A.class "charmoves" ]
                <| List.map3
                    (renderSkill b.ninja chakras active characters)
                    (List.range 0 10) -- doesn't matter, not the limiting factor
                    b.targets
                    b.ninja.skills
            ]
    in
    H.section [ A.classList [ ( "dead", b.ninja.health == 0 ) ] ] <|
    H.aside [ A.class "channels" ] (channels ++ copies)
    :: mainBar ++
    [ H.div [ A.class "charhealth" ] <|
      [ H.div [ A.style "width" <| String.fromInt b.ninja.health ++ "%" ]
        []
      , H.span [ A.class "charhealthtext"
               , A.style anchor <|
                   String.fromInt (b.ninja.health * 93 // 100)
                       ++ "%"
               ] <|
        live [ H.text <| String.fromInt b.ninja.health ]
      ]
      ++ defenses
    , H.aside [ A.class "statuses" ]
        details
    ]


bar : Character -> String -> Int -> Int -> List (Html msg)
bar source name amount dur =
    [ H.section []
      [ icon source name [ A.class "char" ]
      , H.dl []
        [ H.h4 []
          [ H.text name ]
        , H.dt [] [ H.text "Amount" ]
        , H.dd [] [ H.text <| String.fromInt amount ]
        , H.dt [] [ H.text "Duration" ]
        , H.dd [] << Render.duration "Permanent" <| dur
        , H.dt [] [ H.text "Source" ]
        , H.dd [] <| Render.name source
        ]
      ]
    ]


renderView : Set String -> List Character -> Viewable -> Html Msg
renderView visibles characters viewing =
    H.article [ A.class "parchment" ] <| case viewing of
        ViewBarrier x ->
            bar (Game.get characters x.user) x.name x.amount x.dur

        ViewDefense x ->
            bar (Game.get characters x.user) x.name x.amount x.dur

        ViewCharacter x ->
            [ H.section []
              [ icon x "icon" [ A.class "char" ]
              , H.section []
                [ H.h4 [] <|
                  Render.name x
                , H.p [] <|
                  Render.desc x.bio
                ]
              ]
            ]

        ViewDetail removable x ->
            [ H.section []
              [ icon (Game.get characters x.source) x.name [ A.class "char" ]
              , H.dl [] <|
                [ H.h4 [] [ H.span [] [ H.text x.name ] ]
                , Render.classes True <| Set.intersect visibles x.classes
                , H.dt [] [ H.text "Source" ]
                , H.dd [] << Render.name <| Game.get characters x.user
                , H.dt [] [ H.text "Duration" ]
                , H.dd [] << Render.duration "Permanent" <| x.dur
                ] ++
                if x.amount > 0 then
                    [ H.dt [] [ H.text "Amount" ]
                    , H.dd [] [ H.text <| String.fromInt x.amount ]
                    ]
                else
                    []
              ]
            , x.effects
                |> List.filter .visible
                >> List.map (Render.effect characters removable)
                >> H.ul []
            ]

        ViewSkill _ charge x ->
            let
                character =
                    Game.get characters x.owner

                cooldown =
                    case x.cooldown of
                        0 ->
                            "None"

                        y ->
                            String.fromInt <| abs y

                cost =
                    case Chakra.total x.cost of
                        0 ->
                            [ H.text "Free" ]

                        _ ->
                            Render.chakras x.cost

                duration =
                    case x.dur of
                        Instant ->
                            "Instant"

                        Passive ->
                            "Instant"

                        Action 0 ->
                            "Action"

                        Control 0 ->
                            "Control"

                        Ongoing 0 ->
                            "Ongoing"

                        Action y ->
                            "Action " ++ String.fromInt (abs y)

                        Control y ->
                            "Control " ++ String.fromInt (abs y)

                        Ongoing y ->
                            "Ongoing " ++ String.fromInt (abs y)

                charges =
                    if x.charges == 0 then
                        []

                    else
                        [ H.span [ A.class "extra" ]
                          [ H.text <|
                              case x.charges - charge of
                                  1 ->
                                      "1 charge."

                                  y ->
                                      String.fromInt y ++ " charges."
                          ]
                        ]

                match y =
                    y.name == x.name

                varyButtons =
                    List.find
                    (List.any match)
                    character.skills
                    |> Maybe.andThen (\matches ->
                      List.findIndex match matches
                    |> Maybe.map (\i ->
                      Maybe.values
                      [ vPrev matches i
                        |> Maybe.map (\v ->
                          H.a
                          [ A.class "prevSkill click"
                          , E.onClick << View <| ViewSkill [] charge
                            { v | owner = x.owner }
                          ] []
                        )
                      , vNext matches i
                        |> Maybe.map (\v ->
                          H.a [ A.class "nextSkill click"
                              , E.onClick << View <| ViewSkill [] charge
                                { v | owner = x.owner }
                              ] []
                        )
                      ]
                    )
                  )
            in
            [ H.section []
              [ H.div [] <|
                icon character x.name [ A.class "char" ]
                :: Maybe.withDefault [] varyButtons
              , H.dl []
                [ H.h4 []
                  [ H.text x.name ]
                , Render.classes False <| Set.intersect visibles x.classes
                , H.dt [] [ H.text "Cost" ]
                , H.dd [] cost
                , H.dt [] [ H.text "Duration" ]
                , H.dd [] [ H.text duration ]
                , H.dt [] [ H.text "Cooldown" ]
                , H.dd [] [ H.text cooldown ]
                ]
              ]
            , H.p [] <|
              Render.desc x.desc ++ charges
            ]

        ViewUser x ->
            [ H.section []
              [ H.img [ A.class "char", A.src x.avatar ] []
              , H.dl []
                [ H.h4 []
                  [ H.text x.name ]
                , H.p [ A.class << String.toLower <| Game.rank x ]
                  [ H.text <| Game.rank x ]
                , H.dt [] [ H.text "Clan" ]
                , H.dd [] [ H.text <| Maybe.withDefault "Clanless" x.clan ]
                , H.dt [] [ H.text "Level" ]
                , H.dd [] [ H.text << String.fromInt <| x.xp // 1000 ]
                , H.dt [] [ H.text "Record" ]
                , H.dd [] [ Render.streak x ]
                ]
              ]
            ]


vNext : List Skill -> Int -> Maybe Skill
vNext skills i =
    List.getAt i skills
        |> Maybe.andThen
            (\x ->
                skills
                    |> List.drop i
                    >> List.find (not << (\y -> y.name == x.name))
            )


vPrev : List Skill -> Int -> Maybe Skill
vPrev skills i =
    List.getAt i skills
        |> Maybe.andThen
            (\x ->
                skills
                    |> List.take i
                    >> List.reverse
                    >> List.find (not << (\y -> y.name == x.name))
            )
