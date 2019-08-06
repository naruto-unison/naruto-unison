module Site.Play exposing (Model, Msg(..), component)

import Html            as H exposing (Html)
import Html.Events     as E
import Html.Attributes as A
import Http
import Json.Decode     as D
import Html.Lazy exposing (..)
import List.Extra      as List
import Maybe.Extra     as Maybe
import Process
import Task

import Import.Flags exposing (Flags)
import Game.Chakra as Chakra exposing (none)
import Game.Detail as Detail exposing (Detail)
import Game.Game as Game exposing (Act)
import Import.Model as Model exposing (Barrier, Chakras, Channel, Channeling(..), Character, Defense, Effect, Game, GameInfo, Ninja, Player, Requirement(..), Skill, User)
import Import.Model as Player
import Ports exposing (Ports)
import Site.Render as Render exposing (icon)
import Sound exposing (Sound)
import Util exposing (ListChange(..), elem, pure, showErr)

type Viewable
    = ViewBarrier   Barrier
    | ViewCharacter Character
    | ViewDefense   Defense
    | ViewDetail    (Effect -> Bool) Detail
    | ViewSkill     Int (List Int) Int Skill
    | ViewUser      User

type ChakraPair = ChakraPair String Chakras Int Int

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
    , characters : List Character
    , game       : Game
    , chakras    : Chakras
    , randoms    : Chakras
    , exchanged  : Chakras
    , exchange   : Bool
    , viewing    : Viewable
    , highlight  : List Int
    , toggled    : Maybe Act
    , acts       : List Act
    , error      : String
    }

type ExchangeMsg
    = Begin
    | Conclude Chakras
    | Reset

type Msg
    = Receive String
    | Enact ListChange Act
    | Exchange ExchangeMsg
    | Ready
    | ReceivePractice (Result Http.Error (List Game))
    | Spend Chakras
    | Toggle Act
    | Unhighlight
    | View Viewable
    | DoNothing

component ports =
  let
    init : Flags -> Bool -> GameInfo -> Model
    init flags practice info =
        { url        = flags.url
        , practice   = practice
        , player     = info.player
        , user       = Maybe.withDefault info.opponent flags.user
        , vs         = info.opponent
        , characters = info.characters
        , game       = info.game
        , chakras    = Chakra.get info.player info.game
        , randoms    = Chakra.none
        , exchanged  = Chakra.none
        , exchange   = False
        , viewing    = ViewUser info.opponent
        , highlight  = []
        , toggled    = Nothing
        , acts       = []
        , error      = ""
        }

    view : Model -> Html Msg
    view st =
      let
        characters  = List.map2 Game.merge st.characters st.game.ninjas
        costs       = Chakra.sum <| List.map (.cost << .skill) st.acts
        net         = Chakra.sum [st.exchanged, st.chakras, Chakra.negate costs]
        netUnrand   = { net | rand = 0 }
        ownTurn     = st.player == st.game.playing
        rand        = Chakra.total st.randoms + net.rand
                      - Chakra.rate * Chakra.total st.exchanged
        free        = { net | rand = Chakra.total netUnrand + rand }
        acted       = List.map .user st.acts
        renderNinja = renderCharacter characters acted st.toggled st.highlight
                      free ownTurn
        pair name get chakra = ChakraPair name chakra (get net) (get st.randoms)
        chakraPairs = [ pair "blood" .blood { none | blood = 1 }
                      , pair "gen"   .gen   { none | gen   = 1 }
                      , pair "nin"   .nin   { none | nin   = 1 }
                      , pair "tai"   .tai   { none | tai   = 1 }
                      ]
        ally bundle = (bundle.ninja.slot |> remainderBy 2) == case st.player of
            Player.A -> 0
            Player.B -> 1
        (allies, enemies) =
            List.partition ally <|
            List.map3 Bundle characters st.game.ninjas st.game.targets
      in
        H.div [A.id "game"] <|
        [ H.div [A.id "error"] [H.text st.error]
        , H.section [A.id "top"]
          [ H.section
            [ A.id "account0"
            , E.onMouseOver << View <| ViewUser st.user
            ]
            [ H.section []
              [ H.h1 [] [H.text st.user.name]
              , H.p [] [H.text <| Game.rank st.user]
              ]
            , H.img [A.class "charicon", A.src st.user.avatar] []
            ]
          , lazy2 renderView characters st.viewing
          , H.section [A.id "account1", E.onMouseOver << View <| ViewUser st.vs]
            [ H.img [A.class "charicon", A.src st.vs.avatar] []
            , H.section []
              [ H.h1 [] [H.text st.vs.name]
              , H.p [] [H.text <| Game.rank st.vs]
              ]
            ]
          ]
        , H.section [A.id "player0", A.class "player"] <|
            List.map (renderNinja True) allies
        , H.section [A.id "player1", A.class "player"] <|
            List.map (renderNinja False) enemies
        ] ++ case st.game.victor of
            [] ->
              let
                chakraButton text msg condition =
                    H.button
                    ( A.id (String.toLower text)
                      :: clickIf condition "chakraButton" (Exchange msg)
                    ) [H.text text]
                readyMeta =
                    if not ownTurn then
                        [A.id "ready", A.class "noclick"]
                    else if rand /= 0 then
                        [A.id "ready", A.class "noChakra"]
                    else
                        [ A.id "ready"
                        , A.class "click"
                        , E.onClick Ready
                        ]
              in
                [ H.section [A.id "playchakra"] <|
                  List.map (renderChakra ownTurn st.exchange net) chakraPairs ++
                  [ Render.rands (Chakra.total netUnrand) rand
                  , chakraButton "Exchange" Begin <|
                    free.rand >= 5 && Chakra.canExchange net && ownTurn
                  , chakraButton "Reset" Reset <|
                        st.exchanged /= Chakra.none || st.randoms /= Chakra.none
                  ]
                , H.section [A.id "playqueuecont"]
                  [ H.div [A.id "playqueue"] <|
                    List.map (renderAct characters) st.acts
                  , H.div readyMeta []
                  ]
                ]
            [victor] ->
                [ H.section [A.id "endgame"]
                  [ H.p []
                    [ H.text <|
                      if victor == st.player then "Victory" else "Defeat"
                    ]
                  , H.a
                    [ A.id "return"
                    , A.class "playButton parchment click"
                    , A.href "/"
                    ]
                    [H.text "Return"]
                  ]
                ]
            _ ->
                [ H.section [A.id "endgame"]
                  [ H.p [] [H.text "Tie"]
                  , H.a
                    [ A.id "return"
                    , A.class "playButton parchment click"
                    , A.href "/"
                    ]
                    [H.text "Return"]
                  ]
                ]


    withSound : Sound -> Model -> (Model, Cmd Msg)
    withSound sfx st = (st, ports.sound sfx)

    setGameAnd : Game -> Model -> List (Cmd Msg) -> (Model, (Cmd Msg))
    setGameAnd game st cmds =
        ( { st
          | game      = game
          , chakras   = Chakra.get st.player game
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

    update : Msg -> Model -> (Model, Cmd Msg)
    update msg st =
      let
        untoggled = { st | toggled = Nothing }
      in
        case msg of
            View (ViewSkill _ targets _ _ as viewing) ->
                pure { st | viewing = viewing, highlight = targets }
            View viewing     -> pure { st | viewing = viewing }
            DoNothing        -> pure st
            Unhighlight      -> pure { st | highlight = [] }
            Toggle skill     -> withSound Sound.Target <|
              let
                justSkill = Just skill
              in
                if st.toggled == justSkill then
                    untoggled
                else
                    { st | toggled = justSkill }
            Enact Add act    -> withSound Sound.ApplySkill
                { untoggled | acts = st.acts ++ [act] }
            Enact Delete act -> withSound Sound.Cancel
                { untoggled | acts = List.remove act st.acts }
            Spend chakras    -> withSound Sound.Click
                { st
                | randoms = Chakra.sum [st.randoms, chakras]
                , chakras = Chakra.sum [st.chakras, Chakra.negate chakras]
                }
            Exchange Begin   -> withSound Sound.Target
                { st | exchange = not st.exchange }
            Exchange Reset   -> withSound Sound.Cancel
                { st
                | chakras   = Chakra.get st.player st.game
                , randoms   = Chakra.none
                , exchanged = Chakra.none
                , exchange  = False
                }
            Exchange (Conclude chakras) -> withSound Sound.Click
                { st
                | exchanged = Chakra.sum [st.exchanged, chakras]
                , exchange  = False
                }
            Receive json -> case D.decodeString Model.jsonDecGame json of
                Err err -> pure { st | error = D.errorToString err }
                Ok game -> case game.victor of
                    [victor] -> withSound
                        (if victor == st.player then Sound.Win else Sound.Lose)
                        st
                    [] ->
                        setGameAnd game st <|
                        [ ports.sound Sound.StartTurn
                        , if game.playing == st.player then
                            ports.progress 60000 1 0
                          else
                            ports.progress 60000 0 1
                        ]
                    _ -> (st, ports.sound Sound.Death)
            Ready ->
                if st.practice then
                    ( { untoggled
                      | exchange  = False
                      , exchanged = Chakra.none
                      }
                      , Http.get
                          { url    = st.url ++ "api/practiceact/" ++ enactUrl st
                          , expect = Http.expectJson ReceivePractice <|
                                    D.list Model.jsonDecGame
                          }
                    )
                else
                    ( st
                    , Cmd.batch [ ports.sound Sound.StartTurn
                                , ports.websocket <| enactUrl st
                                ]
                    )
            ReceivePractice (Ok([x, y])) ->
                setGameAnd x st
                [ ports.progress 1500 0 1
                , Process.sleep 1500
                  |> Task.perform (always << ReceivePractice <| Ok [y])
                ]
            ReceivePractice (Ok([y])) ->
                setGameAnd y st
                [ ports.sound Sound.StartTurn
                , ports.progress 0 1 1
                ]
            ReceivePractice (Ok(_)) ->
                pure { st | error = "Invalid response from server" }
            ReceivePractice (Err err) ->
                pure { st | error = showErr err }
  in
    { init = init, view = view, update = update }

enactUrl : Model -> String
enactUrl st =
  let
    chakras = toUrl [st.randoms, st.exchanged] <| \x ->
              [x.blood, x.gen, x.nin, x.tai]
    acts    = toUrl st.acts <| \x -> [x.user, x.button, x.target]
  in
    String.join "/" <| chakras ++ acts

toUrl : List a -> (a -> List Int) -> List String
toUrl xs f = List.map (String.join "," << List.map String.fromInt << f) xs

clickIf : Bool -> String -> msg -> List (H.Attribute msg)
clickIf succeeds class command =
  if succeeds then
    [A.class <| class ++ " click", E.onClick command]
  else
    [A.class <| class ++ " noclick"]


renderChakra : Bool -> Bool -> Chakras -> ChakraPair -> Html Msg
renderChakra turn exchange chakras (ChakraPair chakra spend amount random) =
  let
    classes = "chakra " ++ chakra
    meta =
      if exchange then
        clickIf (Chakra.affordable chakras spend) classes <<
        Exchange <| Conclude spend
      else
        [A.class classes]
  in
    H.div []
    [ H.div meta []
    , H.span [] [H.text <| String.fromInt amount]
    , H.a
      (clickIf (turn && random > 0) "more" << Spend <| Chakra.negate spend)
      [H.text "+"]
    , H.a
      (clickIf (turn && amount > 0) "less" <| Spend spend)
      [H.text "—"]
    , H.div [A.class "chakra rand"] []
    , H.span [] [H.text <| String.fromInt random]
    ]

renderAct : List Character -> Act -> Html Msg
renderAct characters x =
    H.div
    [A.class "act click", E.onClick <| Enact Delete x]
    [ icon (Game.root characters x.skill x.user) x.skill.name []
    , H.div [A.class "actcost"] <| Render.chakras x.skill.cost
    ]

renderBarrier : Int -> String -> Int -> List Barrier -> List (Html Msg)
renderBarrier slot anchor track barriers = case List.uncons barriers of
    Nothing      -> []
    Just (x, xs) ->
        H.div
        [ A.class "charbarrier"
        , A.style anchor  <| String.fromInt track ++ "%"
        , A.style "width" <| String.fromInt x.amount ++ "%"
        , E.onMouseOver << View <| ViewBarrier x
        ] [] :: renderBarrier slot anchor (track + x.amount) xs

renderDefense : Int -> String -> Int -> List Barrier -> List Defense
             -> List (Html Msg)
renderDefense slot anchor track barriers defenses = case List.uncons defenses of
    Nothing      -> renderBarrier slot anchor track barriers
    Just (x, xs) ->
        H.div
        [ A.class "chardefense"
        , A.style anchor <| String.fromInt track ++ "%"
        , A.style "width" <| String.fromInt x.amount ++ "%"
        , E.onMouseOver << View <| ViewDefense x
        ] [] :: renderDefense slot anchor (track + x.amount) barriers xs

renderSkill : Int -> Chakras -> Bool -> List Character -> Int -> List Int -> Int
           -> Skill -> Int -> Html Msg
renderSkill user chakras able characters button targets charge skill cooldown =
  let
    image = icon (Game.root characters skill user) skill.name []
  in
    if cooldown > 0
    || not able
    || skill.require == Unusable
    || List.isEmpty targets
    || Chakra.lacks chakras skill.cost then
        H.div
        [ A.class "charmove noclick"
        , E.onMouseOver << View <| ViewSkill user [] charge skill
        , E.onMouseLeave Unhighlight
        ] <| image :: if cooldown <= 0 then [] else
             [H.span [] [H.text << String.fromInt << max 1 <| cooldown // 2]]
    else
        H.div
        [ A.class "charmove click"
        , E.onMouseOver << View <| ViewSkill user targets charge skill
        , E.onMouseLeave Unhighlight
        , E.onClick <|
          (if Game.targets user skill == [user] then Enact Add else Toggle)
              { user    = user
              , skill   = skill
              , target  = user
              , button  = button
              , targets = targets
              }
        ] [image]

renderDetail : Bool -> Int -> List Character -> Detail -> Html Msg
renderDetail team slot characters detail =
  let
    removable =
      if Detail.allied slot detail then
          always False
      else
          Game.removable team
    amount =
      if detail.amount > 1 then
        (::) <| H.span [] [H.text <| String.fromInt detail.amount]
      else
        identity
  in
    H.div
    [ E.onMouseOver << View <| ViewDetail removable detail
    , A.classList [ ("detail"   , True)
                  , ("trap"     , detail.trap)
                  , ("tag"      , detail.ghost || detail.dur == 1)
                  , ("reflected", "Shifted" |> elem detail.classes)
                  , ("remove"   , List.any removable detail.effects
                                  && not ("Unremovable" |> elem detail.classes))
                  ]
    ]
    [ H.div [] <| amount
      [icon (Game.get characters detail.source) detail.name []]
    , H.p [] << Render.duration "\u{00A0}" <| detail.dur
    ]

renderCharacter : List Character -> List Int -> Maybe Act -> List Int -> Chakras
               -> Bool -> Bool -> Bundle -> Html Msg
renderCharacter characters acted toggle highlighted chakras turn onTeam b =
  let
    render   = renderDetail onTeam b.ninja.slot characters
    anchor   = if onTeam then "left" else "right"
    hp       = String.fromInt b.ninja.health ++ "%"
    live     = if b.ninja.health > 0 then identity else always []
    channels = live << List.map (render << Detail.channel) <|
               List.reverse b.ninja.channels
    defenses = live <| renderDefense b.ninja.slot anchor b.ninja.health
               (List.reverse b.ninja.barrier) (List.reverse b.ninja.defense)
    details  = live << List.map render <|
               if onTeam then
                  List.reverse <| Detail.get b.ninja
               else
                  Detail.get b.ninja
    active   = onTeam && turn && b.ninja.health > 0
               && not (b.ninja.slot |> elem acted)
    toggled   = b.ninja.slot |> elem (Game.toggles toggle)
    mainMeta = [ A.classList
                 [ ("highlighted", b.ninja.slot |> elem highlighted)
                 , ("toggled skill", toggled)
                 ]
               , E.onMouseOver << View <| ViewCharacter b.character
               ]
    fullMeta = case toggle of
        Just tog ->
            if toggled then
                (E.onClick <| Enact Add { tog | target = b.ninja.slot })
                :: mainMeta
            else
                mainMeta
        Nothing  -> mainMeta
    faceIcon = case List.head b.ninja.face of
        Nothing   -> icon b.character "icon"
        Just face -> icon (Game.get characters face.user) <| "icon" ++ face.icon
    cooldown = if b.ninja.health > 0 then b.ninja.cooldowns else []
    mainBar  = (if onTeam then identity else List.reverse)
               [ H.section fullMeta [faceIcon [A.class "charicon"]]
               , H.div [A.class "charmoves"] <<
                 List.map5 (renderSkill b.ninja.slot chakras active characters)
                 (List.range 0 3) b.targets (b.ninja.charges ++ [0, 0, 0, 0])
                 b.ninja.skills <| cooldown ++ [0, 0, 0, 0]
               ]
  in
    H.section [A.classList [("dead", b.ninja.health == 0)]] <|
      H.aside [A.class "channels"] channels :: mainBar ++
        [ H.div [A.class "charhealth"] <|
          [ H.div [A.style "width" hp] []
          , H.span [A.class "charhealthtext", A.style anchor hp] <|
            live [H.text <| String.fromInt b.ninja.health]
          ] ++ defenses
        , H.aside [A.class "statuses"] details
        ]

label : String -> Html msg
label name = H.span [A.class "label"] [H.text name]

bar : Character -> String -> Int -> Int -> List (Html msg)
bar source name amount dur =
  [ H.section []
    [ icon source name [A.class "char"]
    , H.section []
      [ H.h1 [] [H.text name]
      , label "Amount: "
      , H.span [] [H.text <| String.fromInt amount]
      , label "Duration: "
      , H.span [] << Render.duration "Permanent" <| dur
      , label "Source: "
      , H.span [] <| Render.name source
      ]
    ]
  ]

renderView : List Character -> Viewable -> Html Msg
renderView characters viewing = H.article [A.class "parchment"] <| case viewing of
    ViewBarrier x   -> bar (Game.get characters x.user) x.name x.amount x.dur
    ViewDefense x   -> bar (Game.get characters x.user) x.name x.amount x.dur
    ViewCharacter x ->
        [ H.section []
          [ icon x "icon" [A.class "char"]
          , H.section []
            [ H.h1 [] <| Render.name x
            , H.p [] [H.text x.bio]
            ]
          ]
        ]
    ViewDetail removable x ->
      let
        source = Game.get characters x.source
        count  =
          if x.amount > 1 then
              x.name ++ " (" ++ String.fromInt x.amount ++ ")"
          else
              x.name
        name   =
          if "Shifted" |> elem x.classes then
              H.span [A.class "reflected"] [H.text <| count ++ " (Reflected)"]
          else
              H.span [] [H.text count]
      in
        [ H.section []
          [ icon source "icon" [A.class "char"]
          , H.section []
            [ H.h1 [] [name]
            , Render.classes True x.classes
            , label "Source: "
            , H.span [] <| Render.name source
            , label "Duration: "
            , H.span [] << Render.duration "Permanent" <| x.dur
            ]
          ]
        , H.ul [] <| List.map (Render.effect removable) x.effects
        , H.p [] <| Render.desc x.desc
        ]
    ViewSkill user _ charge x ->
      let
        cooldown = case x.cooldown of
            0 -> "None"
            y -> String.fromInt <| y // 2
        cost = case Chakra.total x.cost of
            0 -> [H.text "Free"]
            _ -> Render.chakras x.cost
        duration = case x.channel of
            Instant     -> "Instant"
            Passive     -> "Instant"
            (Action 0)  -> "Action"
            (Control 0) -> "Control"
            (Ongoing 0) -> "Ongoing"
            (Action y)  -> "Action "  ++ String.fromInt y
            (Control y) -> "Control " ++ String.fromInt y
            (Ongoing y) -> "Ongoing " ++ String.fromInt y
        charges =
          if x.charges == 0 then
            []
          else
            [ H.span [A.class "extra"]
              [ H.text <| case x.charges - charge of
                  1 -> "1 charge."
                  y -> String.fromInt y ++ " charges."
              ]
            ]
        match y = y.name == x.name
        varyButtons =
            List.find (List.any match) (Game.root characters x user).skills
            |> Maybe.andThen (\matches ->
               List.findIndex match matches
            |> Maybe.map (\i ->
                Maybe.values
                  [ vPrev matches i
                    |> Maybe.map (\v ->
                      H.a
                      [ A.class "prevSkill click"
                      , E.onClick << View <| ViewSkill user [] charge v
                      ] []
                    )
                  , vNext matches i
                    |> Maybe.map (\v ->
                      H.a
                      [ A.class "nextSkill click"
                      , E.onClick << View <| ViewSkill user [] charge v
                      ] []
                    )
                  ]
              )
            )
      in
        [ H.section []
          [ H.div [] <|
            icon (Game.root characters x user) x.name [A.class "char"]
            :: Maybe.withDefault [] varyButtons
          , H.section []
            [ H.h1 [] [H.text x.name]
            , Render.classes False x.classes
            , label "Cost: "
            , H.span [] cost
            , label "Duration: "
            , H.span [] [H.text duration]
            , label "Cooldown: "
            , H.span [] [H.text cooldown]
            ]
          ]
        , H.p [] <| Render.desc x.desc ++ charges
        ]
    ViewUser x ->
        [ H.section []
          [ H.img [A.class "char", A.src x.avatar] []
          , H.section []
            [ H.h1 [] [H.text x.name]
            , H.span [] [H.text <| Game.rank x]
            , label "Clan: "
            , H.span [] [H.text <| Maybe.withDefault "Clanless" x.clan]
            , label "Level: "
            , H.span [] [H.text << String.fromInt <| x.xp // 1000]
            ]
          ]
        ]

vNext : List Skill -> Int -> Maybe Skill
vNext skills i =
    List.getAt i skills
    |> Maybe.andThen
       (\x -> List.find (not << \y -> y.name == x.name) <| List.drop i skills)

vPrev : List Skill -> Int -> Maybe Skill
vPrev skills i =
    List.getAt i skills
    |> Maybe.andThen
       (\x -> List.find (not << \y -> y.name == x.name) <<
              List.reverse <| List.take i skills)
