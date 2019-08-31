module Site.Select exposing (Model, Msg(..), Stage(..), component)

import Browser.Dom        as Dom
import Browser.Navigation as Navigation
import Dict
import Html               as H exposing (Html)
import Html.Events        as E
import Html.Attributes    as A
import Html.Keyed         as Keyed
import Http
import Json.Decode exposing (Value)
import Html.Lazy exposing (..)
import List.Extra         as List
import List.Nonempty      as Nonempty exposing (Nonempty(..))
import Maybe.Extra        as Maybe
import Task exposing (Task)
import Tuple exposing (first, second)
import Url

import Game.Game exposing (rank)
import Import.Flags exposing (Characters, Flags, characterName)
import Import.Model as Model exposing (Character, GameInfo, Skill, User)
import Ports exposing (Ports)
import Site.Render as Render exposing (icon)
import Sound exposing (Sound)
import Util exposing (ListChange(..), elem, pure, showErr)

for : List a -> (a -> b) -> List b
for xs f = List.map f xs

wraparound : Bool -> Int -> List a -> List a
wraparound wrapping i xs =
  let
    (before, after) = List.splitAt i xs
  in
    if wrapping then after else after ++ before

type Previewing
    = NoPreview
    | PreviewUser User
    | PreviewChar Character

type alias Form =
    { name       : String
    , background : String
    , condense   : Bool
    , avatar     : String
    }

type FormUpdate
    = Name String
    | Background String
    | Condense Bool
    | Avatar String

updateForm : FormUpdate -> Form -> Form
updateForm msg form = case msg of
    Name x       -> { form | name = x }
    Background x -> { form | background = x }
    Condense x   -> { form | condense = x }
    Avatar x     -> { form | avatar = x }

formUrl : Form -> String
formUrl form = "api/update/" ++ form.name ++ "/"
               ++ (if form.condense then "True" else "False")
               ++ "/b" ++ form.background ++ "/"
               ++ Url.percentEncode form.avatar

type Stage = Browsing | Queued | Practicing

type alias Model =
    { error      : Maybe String
    , stage      : Stage
    , url        : String
    , team       : List Character
    , vs         : List Character
    , user       : Maybe User
    , avatars    : List String
    , chars      : Characters
    , csrf       : String
    , csrfParam  : String
    , showLogin  : Bool
    , index      : Int
    , cols       : Int
    , toggled    : Maybe Character
    , previewing : Previewing
    , variants   : List Int
    , pageSize   : Int
    , form       : Form
    }

type Queue
    = Quick
    | Practice
    | Private

type Msg
    = Fail String
    | Page Int
    | Scroll Int
    | Vary Int Int
    | SwitchLogin
    | TryUpdate
    | Enqueue Queue
    | SetStage Stage
    | Preview Previewing
    | Team ListChange Character
    | Vs ListChange Character
    | Untoggle
    | UpdateForm FormUpdate
    | ReceiveUpdate (Result Http.Error ())
    | ReceiveGame (Result Http.Error GameInfo)
    | DoNothing

component ports =
  let
    init : Flags -> Model
    init flags =
        { error      = Nothing
        , stage      = Browsing
        , url        = flags.url
        , team       = flags.userTeam
        , vs         = flags.userPractice
        , user       = flags.user
        , chars      = flags.characters
        , avatars    = flags.avatars
        , csrf       = flags.csrf
        , csrfParam  = flags.csrfParam
        , showLogin  = True
        , index      = 0
        , cols       = 11
        , toggled    = Nothing
        , previewing = NoPreview
        , variants   = [0, 0, 0, 0]
        , pageSize   = 36
        , form       = case flags.user of
            Nothing   ->
                { name       = ""
                , background = ""
                , condense   = False
                , avatar     = ""
                }
            Just user ->
                { name = user.name
                , background = Maybe.withDefault "" user.background
                , condense   = user.condense
                , avatar     = user.avatar
                }
        }

    view : Model -> Html Msg
    view st =
        H.section [A.id "charSelect"] <|
        lazy5 userBox st.user st.csrf st.csrfParam st.showLogin st.team ::
        case st.stage of
            Queued     -> []
            _          ->
              let
                wrapping = st.index + st.pageSize >= size st
                displays =
                    if condense st then
                        List.map Nonempty.head <|
                            wraparound wrapping st.index st.chars.groupList
                    else
                        wraparound wrapping st.index st.chars.list
                charClass char = -- god I wish this language had pattern guards
                    if List.member char <| case st.stage of
                      Practicing -> st.vs
                      _          -> st.team
                  then
                        "char disabled"
                    else case st.toggled of
                        Nothing      -> "char click"
                        Just toggled ->
                            if toggled == char then
                                "char click selected"
                            else
                                "char"
                (topModule, teamOp) = case st.stage of
                    Browsing -> (previewBox st, Team)
                    _        -> (vsBox st,      Vs)
              in
                topModule ::
                [ H.section
                  [ A.id "characterButtons"
                  , A.classList
                    [ ("parchment", True)
                    , ("full",      st.stage == Practicing)
                    ]
                  ]
                  [ H.nav
                    [ A.id "prevPage"
                    , A.classList
                      [("click", True), ("wraparound", st.index == 0)]
                    , E.onClick <| Page (-1)
                    ] []
                  , H.nav
                    [ A.id "nextPage"
                    , A.classList [("click", True), ("wraparound", wrapping)]
                    , E.onClick <| Page 1
                    ] []
                  , Keyed.node "div"
                    [ A.id "charScroll"
                    , E.onMouseOver Untoggle
                    ] <| for displays <| \char ->
                        ( characterName char
                        , icon char "icon"
                          [ E.onMouseOver << Preview <| PreviewChar char
                          , E.onClick <| teamOp Add char
                          , A.class <| charClass char
                          ]
                        )
                  ]
                ]

    withSound : Sound -> Model -> (Model, Cmd Msg)
    withSound sound st = (st, ports.sound sound)

    update : Msg -> Model -> (Model, Cmd Msg)
    update msg st = case msg of
        DoNothing    -> pure st
        Fail x       -> pure { st | error = Just x }
        SwitchLogin  -> pure { st | showLogin = not st.showLogin }
        Untoggle     -> pure { st | toggled = Nothing }
        Page x       -> (st, scroll x)
        UpdateForm x -> pure { st | form = updateForm x st.form }
        Scroll x     -> withSound Sound.Scroll <|
          let
            index = x + st.index
          in
            if index < 0 then
              let
                rem = size st |> remainderBy (-x)
              in
                { st | index = if rem == 0 then 0 else size st - rem }
            else if index >= size st then
                { st | index = 0 }
            else
                { st | index = x + st.index }
        Preview x    -> pure <|
            if Maybe.isJust st.toggled then
                st
            else
                { st | previewing = x, variants = [0,0,0,0] }
        Vary slot i -> withSound Sound.Click
            { st | variants = List.updateAt slot (always i) st.variants }
        Team Add char -> withSound Sound.Click <|
              if char |> elem st.team then
                  { st | toggled = Nothing }
              else if st.toggled /= Just char then
                  { st | toggled = Just char }
              else if List.length st.team < 3 then
                  { st | toggled = Nothing, team = char :: st.team }
              else
                  { st | toggled = Nothing }
        Team Delete char -> withSound Sound.Cancel
            { st | team = List.remove char st.team }
        Vs Add char -> withSound Sound.Click <|
            if char |> elem st.vs then
                st
            else if List.length st.vs < 3 then
                { st | vs = st.vs ++ [char] }
            else
                st
        Vs Delete char -> withSound Sound.Cancel
            { st | vs = List.remove char st.vs }
        TryUpdate   ->
            ( st
            , Http.get
                 { url    = st.url ++ formUrl st.form
                 , expect = Http.expectWhatever ReceiveUpdate
                 }
            )
        ReceiveUpdate (Ok ()) -> (st, Navigation.reload)
        ReceiveUpdate (Err (Http.BadStatus code)) ->
          let
            error = case code of
                500 -> "Username already taken"
                400 -> "Name can only contain letters and numbers"
                _   -> "Error: Code " ++ String.fromInt code
          in
            pure { st | error = Just error }
        ReceiveUpdate (Err err) -> pure { st | error = Just <| showErr err }
        Enqueue Practice       ->
          let
            team = (++) (st.url ++ "api/practicequeue/") << String.join "/" <<
                   List.map characterName <| st.team ++ st.vs
          in
            ( st
            , Http.get
                 { url    = team
                 , expect = Http.expectJson ReceiveGame Model.jsonDecGameInfo
                 }
            )
        ReceiveGame _   -> pure st
        SetStage stage  -> withSound Sound.Click { st | stage = stage }
        Enqueue Private -> pure st
        Enqueue Quick   ->
          ( { st | stage = Queued }
          , ports.websocket << String.join "/" <| List.map characterName st.team
          )


  in
    { init = init, view = view, update = update }

condense : Model -> Bool
condense = Maybe.withDefault False << Maybe.map .condense << .user

size : Model -> Int
size st =
    if condense st then
        List.length st.chars.groupList
    else
        List.length st.chars.list

characterButtons : Model -> List Character
characterButtons st =
  let
      wrapping = st.index + st.pageSize >= size st
  in
    if condense st then
        List.map Nonempty.head <|
            wraparound wrapping st.index st.chars.groupList
    else
        wraparound wrapping st.index st.chars.list

userBox : Maybe User -> String -> String -> Bool -> List Character -> Html Msg
userBox mUser csrf csrfParam showLogin team =
    let
      meta onClick =
          if List.length team == 3 then
              [A.class "parchment playButton click", E.onClick onClick]
          else
              [A.class "parchment playButton"]
      preview    = E.onClick << Preview
      nav        = case mUser of
          Just _ ->
              [ H.a
                [ A.id    "mainsite"
                , A.class "playButton parchment click blacked"
                , A.href  "/home"
                ] [H.text "Main Site"]
              , H.a (meta <| Enqueue Quick)
                [H.text "Start Quick Match"]
              , H.a (meta <| Enqueue Private)
                [H.text "Start Private Match"]
              , H.a (meta <| SetStage Practicing)
                [H.text "Start Practice Match"]
              ]
          Nothing ->
              [ H.a
                [ A.id    "mainsite"
                , A.class "playButton parchment click blacked"
                , A.href  "/home"
                ] [H.text "Main Site"]
              ]
      box        = case mUser of
          Just user ->
              H.div
              [ A.id "userBox", A.class "parchment loggedin"
              , E.onMouseOver << Preview <| PreviewUser user
              ]
              [ H.img [A.class "userimg", A.src user.avatar] []
              , H.strong [] [H.text user.name]
              , H.br [] []
              , H.text <| rank user
              , H.br [] []
              , H.strong [] [H.text "Clan: "]
              , H.text <| Maybe.withDefault "Clanless" user.clan
              , H.br [] []
              , H.strong [] [H.text "Level: "]
              , H.text <| String.fromInt (user.xp // 1000) ++ " ("
                          ++ String.fromInt (user.xp |> remainderBy 1000)
                          ++ " XP)"
              , H.br [] []
              , H.strong [] [H.text "Ladder Rank: "]
              , H.text "None"
              , H.br [] []
              , H.strong [] [H.text "Record: "]
              , H.text <| String.fromInt user.wins ++ " - "
                ++ String.fromInt (user.wins + user.losses)
                ++ " (+" ++ String.fromInt user.streak ++ ")"
              ]
          Nothing ->
              H.div [A.id "userBox", A.class "parchment"]
              [ H.form
                [ A.id <| if showLogin then "loginForm" else "registerForm"
                , A.class "userForm"
                , A.method "POST"
                , A.action <| "/auth/page/email/"
                              ++ if showLogin then "login" else "register"
                ] << List.map second <| List.filter first
                  [ ( True, H.input
                      [ A.type_         "hidden"
                      , A.name          csrfParam
                      , A.value         csrf
                      ] []
                    )
                  , ( True, H.div []
                      [ H.input
                        [ A.class       "email"
                        , A.name        "email"
                        , A.type_       "email"
                        , A.required    True
                        -- , A.autofocus   True
                        , A.placeholder "Email"
                        ] []
                      ]
                    )
                  , ( showLogin, H.div []
                      [ H.input
                        [ A.class       "password"
                        , A.name        "password"
                        , A.type_       "password"
                        , A.required    True
                        , A.placeholder "Password"
                        ] []
                      ]
                    )
                  , ( showLogin, H.div [A.id "controls"]
                      [ H.button
                        [ A.class       "playButton click"
                        , A.type_       "submit"
                        ] [H.text "Log in"]
                      , H.a
                        [ A.class       "click"
                        , E.onClick     SwitchLogin
                        ] [H.text "Register"]
                      ]
                    )
                  , ( not showLogin, H.div [A.id "controls"]
                      [ H.a
                        [ A.class       "click"
                        , E.onClick     SwitchLogin
                        ] [H.text "Log in"]
                      , H.button
                        [ A.class       "playButton click"
                        , A.type_       "submit"
                        ] [H.text "Register"]
                      ]
                    )
                  ]
              ]
    in
      H.header []
      [ H.nav [A.id "playButtons"] <| nav
      , H.div [A.class "space"] []
      , H.section [A.id "teamContainer"]
        [ H.div [A.class "space"] []
        , Keyed.node "div" [A.id "teamButtons", A.class "select"] <<
          for team <| \char ->
              ( characterName char
              , icon char "icon"
                [ A.class "char click"
                , E.onMouseOver << Preview <| PreviewChar char
                , E.onClick <| Team Delete char
                ]
              )
        , H.div [A.id "underTeam", A.class "parchment"] []
        ]
      , box
      ]

failWarning : Maybe String -> List (Html msg) -> List (Html msg)
failWarning x xs = case x of
    Nothing      -> xs
    Just warning -> xs ++ [H.span [A.id "userfail"] [H.text warning]]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  H.input [ A.type_ t, A.placeholder p, A.value v, E.onInput toMsg ] []

vsBox : Model -> Html Msg
vsBox st =
  let
    meta =
        if List.length st.vs == 3 then
            [ A.class "parchment playButton click"
            , E.onClick <| Enqueue Practice
            ]
        else
            [A.class "parchment playButton"]
  in
    H.section [A.id "vs", A.class "parchment"]
    [ H.nav []
      [ H.button meta [H.text "Ready"]
      , H.button
        [A.class "parchment playButton click", E.onClick <| SetStage Browsing]
        [H.text "Cancel"]
      ]
    , H.span [] [H.text "VS: "]
    , Keyed.node "div" [A.id "vsButtons", A.class "select"] <<
      for st.vs <| \char ->
          ( characterName char
          , icon char "icon"
            [ A.class "char click"
            , E.onClick <| Vs Delete char
            ]
          )
    ]

previewBox : Model -> Html Msg
previewBox st = case st.previewing of
    NoPreview ->
      H.article [A.class "parchment", A.style "display" "none"] []
    PreviewUser user ->
      H.article [A.class "parchment"]
      [ H.div [A.id "accountSettings"]
        [ H.p [] <| failWarning st.error
          [ H.label [] [H.text "Name"]
          , H.input
            [ A.type_ "text"
            , A.name "name"
            , A.value st.form.name
            , E.onInput <| UpdateForm << Name
            ] []
          ]
        , H.p []
          [ H.label [] [H.text "Background"]
          , H.input
            [ A.type_ "text"
            , A.name "background"
            , A.value st.form.background
            , E.onInput <| UpdateForm << Background
            ] []
          ]
        , H.p []
          [ H.input
            [ A.type_   "checkbox"
            , A.name    "condense"
            , A.checked st.form.condense
            , E.onInput <| always <<
              UpdateForm << Condense <| not st.form.condense
            ] []
          , H.label []
            [H.text "Show only the first version of each character in the selection grid"]
          ]
        , H.p [] [H.span [] [H.text "Avatars"]]
        , H.section [A.id "avatars"] << for st.avatars <| \avatar ->
            if st.form.avatar == avatar then
                H.img
                [ A.src avatar
                , A.class "noclick"
                ] []
            else
                H.img
                [ A.src avatar
                , A.class "click"
                , E.onClick << UpdateForm <| Avatar avatar
                ] []
        , H.button [A.id "updateButton", A.class "click", E.onClick TryUpdate]
          [H.text "Update"]
        , H.a [A.href "auth/logout"]
          [ H.button [A.id "logoutButton", A.class "click"] [H.text "Log out"] ]
        ]
      ]
    PreviewChar char ->
        H.article [A.class "parchment"] <|
        [ Keyed.node "aside" [] <| if not <| condense st then [] else
          case Dict.get (st.chars.shortName char) st.chars.groupDict of
            Nothing              -> []
            Just (Nonempty _ []) -> []
            Just (Nonempty x xs) -> for (x :: xs) <| \char_ ->
                (characterName char, icon char_ "icon" <|
                  if List.member char <| case st.stage of
                      Practicing -> st.vs
                      _          -> st.team
                  then
                    [ A.classList
                      [("on", char == char_), ("noclick disabled char", True)]
                    , E.onMouseOver << Preview <| PreviewChar char_
                    ]
                  else
                    [ A.classList
                      [("on", char == char_), ("click char", True)]
                    , E.onMouseOver << Preview <| PreviewChar char_
                    , E.onClick <| Team Add char_
                    ])
        , H.h1 [] <| icon char "icon" [A.class "char"] :: Render.name char
        , H.p [] [H.text char.bio]
        ] ++
        List.map3 (previewSkill char) (List.range 0 3) char.skills st.variants

previewSkill : Character -> Int -> List Skill -> Int -> Html Msg
previewSkill char slot skills i = case List.getAt i skills of
    Nothing -> H.section [] []
    Just skill ->
      let
        findDifferent xs x = List.findIndices (\y -> y.name /= x.name) xs
        vPrev = List.getAt i skills
                |> Maybe.andThen
                   (List.last << findDifferent (List.take i skills))
                >> Maybe.map (\v -> H.a [ A.class "prevSkill click"
                                        , E.onClick <| Vary slot v
                                        ] [])
        vNext = List.getAt i skills
                |> Maybe.andThen
                   (List.head << findDifferent (List.drop i skills))
                >> Maybe.map (\v -> H.a [ A.class "nextSkill click"
                                        , E.onClick << Vary slot <| v + i
                                        ] [])
      in
        H.section []
        [ H.div [] <| Maybe.values
          [ Just <| icon char skill.name [A.class "char"]
          , vPrev
          , vNext
          ]
        , H.h1 [] <| H.text skill.name :: Render.chakras skill.cost ++
          [ Render.classes False skill.classes
          ]
        , H.p [] << (++) (Render.desc skill.desc) << List.map
          (H.span [A.class "extra"] << List.singleton << H.text << second) <|
          List.filter first
          [ (skill.charges > 1,  String.fromInt skill.charges ++ " charges.")
          , (skill.charges == 1, String.fromInt skill.charges ++ " charge.")
          , (skill.cooldown > 0, "CD: " ++ String.fromInt (skill.cooldown // 2))
          ]
        ]

calcSize : Dom.Viewport -> Int
calcSize dom =
    floor (dom.viewport.width / 68) * floor (dom.viewport.height / 64)

scrollTask : Int -> Task Dom.Error Int
scrollTask signum =
    Dom.getViewportOf "charScroll" |> Task.map ((*) signum << calcSize)

scrollCase : Result Dom.Error Int -> Msg
scrollCase x = case x of
    Ok val               -> Scroll val
    Err (Dom.NotFound e) -> Fail <| "Not found: " ++ e

scroll : Int -> Cmd Msg
scroll = Task.attempt scrollCase << scrollTask
