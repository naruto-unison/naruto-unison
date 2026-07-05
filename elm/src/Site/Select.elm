module Site.Select exposing (Model, Msg(..), Stage(..), component)

import Accessibility.Role as Role
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Dict
import Game.Chakras as Chakras
import Game.Characters as Characters exposing (Characters)
import Game.Game as Game
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Lazy exposing (lazy2)
import Http
import Import.Flags exposing (Csrf, Flags, War)
import Import.Model as Model exposing (Chakras, Character, GameInfo, ObjectiveProgress, Skill, User)
import Json.Decode as D
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Maybe.Extra as Maybe
import Ports exposing (Ports)
import Process
import Set exposing (Set)
import Site.Render as Render
import Sound exposing (Sound)
import Task
import Url
import Util exposing (ListChange(..), pure, showBool, showErr)


type Previewing
    = PreviewWar
    | PreviewUser User
    | PreviewChar Character


type alias Form =
    { name : String
    , background : String
    , condense : Bool
    , avatar : String
    }


createForm : Maybe User -> Form
createForm muser =
    case muser of
        Nothing ->
            { name = ""
            , background = ""
            , condense = False
            , avatar = ""
            }

        Just { avatar, background, condense, name } ->
            { name = name
            , background = Maybe.withDefault "" background
            , condense = condense
            , avatar = avatar
            }


type FormUpdate
    = Name String
    | Background String
    | Condense Bool
    | Avatar String


updateForm : FormUpdate -> Form -> Form
updateForm msg form =
    case msg of
        Name name ->
            { form | name = name }

        Background background ->
            { form | background = background }

        Condense condense ->
            { form | condense = condense }

        Avatar avatar ->
            { form | avatar = avatar }


type Stage
    = Browsing
    | Queued
    | Practicing
    | Searching


type UserBoxFormType
    = Login
    | Register


swapFormType : UserBoxFormType -> UserBoxFormType
swapFormType formType =
    case formType of
        Login ->
            Register

        Register ->
            Login


type alias Team =
    { list : List Character
    , set : Set String
    , costs : Chakras
    }


createTeam : List Character -> Team
createTeam list =
    { list = list
    , set = Set.fromList <| List.map .ident list
    , costs =
        list
            |> List.concatMap (.skills >> List.filterMap List.head)
            |> List.map .cost
            |> Chakras.sum
    }


type alias Model =
    { error : Maybe String
    , stage : Stage
    , url : String
    , team : Team
    , vs : List Character
    , unlocked : Set String
    , user : Maybe User
    , avatars : List String
    , chars : Characters
    , war : War
    , csrf : Csrf
    , userBoxFormType : UserBoxFormType
    , index : Int
    , cols : Int
    , previewing : Previewing
    , mission : List ObjectiveProgress
    , alternates : List Int
    , pageSize : Int
    , search : String
    , condense : Bool
    , form : Form
    }


alterTeam : (List Character -> List Character) -> Model -> Model
alterTeam update st =
    { st | team = createTeam <| update st.team.list }


describeError : Http.Error -> String
describeError err =
    case err of
        Http.BadStatus 500 ->
            "Username already taken"

        Http.BadStatus 400 ->
            "Name can only contain letters and numbers"

        Http.BadStatus code ->
            "Error: Code " ++ String.fromInt code

        _ ->
            showErr err


type Queue
    = Quick
    | Practice
    | Private


showQueue : Queue -> String
showQueue queue =
    case queue of
        Quick ->
            "quick"

        Practice ->
            "practice"

        Private ->
            "private"


type Msg
    = Dequeue
    | DoNothing
    | Enqueue Queue
    | Fail String
    | GetMission Character
    | Page Int
    | Preview Previewing
    | Reanimate Character
    | ReceiveGame (Result Http.Error GameInfo)
    | ReceiveMission Character (Result Http.Error (List ObjectiveProgress))
    | ReceiveReanimate Character (Result Http.Error Int)
    | ReceiveUpdate (Result Http.Error ())
    | Scroll Int
    | Search
    | SetSearch String
    | SetStage Stage
    | SwitchLogin
    | ToggleTeam Character
    | TryUpdate
    | Alternate Int Int
    | Vs ListChange Character
    | UpdateForm FormUpdate


apiUrl : String -> String -> List String -> String
apiUrl baseUrl endpoint fragments =
    String.join "/" <| (baseUrl ++ "api") :: endpoint :: fragments


formUrl : String -> Form -> String
formUrl baseUrl { name, condense, background, avatar } =
    apiUrl baseUrl
        "update"
        [ name
        , showBool condense
        , "b" ++ background
        , Url.percentEncode avatar
        ]


updateUrl : Model -> String
updateUrl { url, form } =
    formUrl url form


missionUrl : Model -> Character -> String
missionUrl { url } { ident } =
    apiUrl url "mission" [ ident ]


reanimateUrl : Model -> Character -> String
reanimateUrl { url } { ident } =
    apiUrl url "reanimate" [ ident ]


practiceUrl : Model -> String
practiceUrl { url, team, vs } =
    apiUrl url "practicequeue" <|
        List.map .ident (team.list ++ vs)


queueMessage : Queue -> Team -> String
queueMessage queue team =
    String.join "/" (showQueue queue :: List.map .ident team.list)


scrollSizeFromViewport : Dom.Viewport -> Int
scrollSizeFromViewport { viewport } =
    floor (viewport.width / 68) * floor (viewport.height / 64)


scrollViewport : Int -> Cmd Msg
scrollViewport signum =
    let
        handleResult domAttempt =
            case domAttempt of
                Ok viewport ->
                    Scroll <| signum * scrollSizeFromViewport viewport

                Err (Dom.NotFound e) ->
                    Fail <| "Not found: " ++ e
    in
    Dom.getViewportOf "teamScroll"
        |> Task.attempt handleResult


component :
    Ports Msg
    ->
        { init : Flags -> Model
        , update : Msg -> Model -> ( Model, Cmd Msg )
        , view : Model -> Html Msg
        }
component ports =
    let
        withSound : Sound -> Model -> ( Model, Cmd Msg )
        withSound sound st =
            ( st, ports.sound sound )

        init : Flags -> Model
        init flags =
            let
                getCharacter ident =
                    Dict.get ident flags.characters.dict

                getCharacters =
                    List.filterMap getCharacter
            in
            { error = Nothing
            , stage = Browsing
            , url = flags.url
            , team = createTeam <| getCharacters flags.userTeam
            , vs = getCharacters flags.userPractice
            , user = flags.user
            , chars = flags.characters
            , avatars = flags.avatars
            , war = flags.war
            , csrf = flags.csrf
            , userBoxFormType = Login
            , index = 0
            , cols = 11
            , previewing = PreviewWar
            , mission = []
            , alternates = [ 0, 0, 0, 0 ]
            , pageSize = 36
            , search = ""
            , unlocked =
                if Maybe.isJust flags.user then
                    flags.unlocked

                else
                    Set.empty
            , condense =
                case flags.user of
                    Just user ->
                        user.condense

                    Nothing ->
                        False
            , form = createForm flags.user
            }

        view : Model -> Html Msg
        view st =
            H.section [ A.id "charSelect" ] <|
                lazy2 (renderUserBox st)
                    st.userBoxFormType
                    st.team
                    :: (case st.stage of
                            Queued ->
                                []

                            _ ->
                                [ case st.stage of
                                    Practicing ->
                                        renderVsBox st.stage st.vs

                                    Searching ->
                                        renderSearchBox st.error st.search

                                    _ ->
                                        renderPreviewBox st
                                , renderCharList st
                                , renderVsList st
                                ]
                       )

        update : Msg -> Model -> ( Model, Cmd Msg )
        update msg st =
            case msg of
                DoNothing ->
                    pure st

                Fail x ->
                    pure { st | error = Just x }

                SwitchLogin ->
                    pure { st | userBoxFormType = swapFormType st.userBoxFormType }

                Page x ->
                    ( st, scrollViewport x )

                UpdateForm x ->
                    pure { st | form = updateForm x st.form }

                Scroll x ->
                    withSound Sound.Scroll <|
                        { st
                            | index = x + st.index
                            , pageSize = abs x
                        }

                Preview x ->
                    withSound Sound.Click
                        { st
                            | previewing = x
                            , alternates = [ 0, 0, 0, 0 ]
                            , mission = []
                        }

                Alternate slot i ->
                    withSound Sound.Click
                        { st | alternates = List.updateAt slot ((+) i) st.alternates }

                ToggleTeam char ->
                    if Set.member char.ident st.team.set then
                        case st.stage of
                            Practicing ->
                                pure st

                            _ ->
                                withSound Sound.Cancel <|
                                    alterTeam (List.remove char) st

                    else if Set.size st.team.set == Game.teamSize then
                        pure st

                    else
                        withSound Sound.Click <|
                            alterTeam ((::) char) st

                Vs Add char ->
                    withSound Sound.Click <|
                        if List.length st.vs == Game.teamSize || List.member char st.vs then
                            st

                        else
                            { st | vs = st.vs ++ [ char ] }

                Vs Delete char ->
                    withSound Sound.Cancel
                        { st | vs = List.remove char st.vs }

                TryUpdate ->
                    ( st
                    , Http.get
                        { url = updateUrl st
                        , expect = Http.expectWhatever ReceiveUpdate
                        }
                    )

                ReceiveUpdate (Ok ()) ->
                    ( st, Navigation.reload )

                ReceiveUpdate (Err err) ->
                    pure { st | error = Just <| describeError err }

                Enqueue Practice ->
                    ( st
                    , Http.get
                        { url = practiceUrl st
                        , expect = Http.expectJson ReceiveGame Model.jsonDecGameInfo
                        }
                    )

                -- handled in Application.elm
                ReceiveGame _ ->
                    pure st

                SetSearch name ->
                    pure { st | search = name }

                SetStage stage ->
                    ( { st | stage = stage, error = Nothing }
                    , if st.stage == Queued && stage == Browsing then
                        Cmd.batch
                            [ ports.websocket "cancel", ports.sound Sound.Click ]

                      else
                        ports.sound Sound.Click
                    )

                Search ->
                    ( st, ports.websocket st.search )

                Dequeue ->
                    pure { st | stage = Browsing }

                Enqueue Private ->
                    ( { st | stage = Queued }
                    , Cmd.batch
                        [ ports.websocket <| queueMessage Private st.team
                        , Process.sleep 1000 |> Task.perform (always Search)
                        ]
                    )

                Enqueue Quick ->
                    ( { st | stage = Queued }
                    , ports.websocket <| queueMessage Quick st.team
                    )

                GetMission char ->
                    ( st
                    , Http.get
                        { url = missionUrl st char
                        , expect =
                            Http.expectJson (ReceiveMission char) <|
                                D.list Model.jsonDecObjectiveProgress
                        }
                    )

                ReceiveMission char (Ok mission) ->
                    if st.previewing == PreviewChar char then
                        pure { st | mission = mission }

                    else
                        pure st

                ReceiveMission _ (Err err) ->
                    pure { st | error = Just <| showErr err }

                Reanimate char ->
                    ( st
                    , Http.get
                        { url = reanimateUrl st char
                        , expect = Http.expectJson (ReceiveReanimate char) D.int
                        }
                    )

                ReceiveReanimate char (Ok dna) ->
                    case st.user of
                        Just user ->
                            withSound Sound.Win <|
                                { st
                                    | user = Just { user | dna = dna }
                                    , unlocked = Set.insert char.ident st.unlocked
                                }

                        Nothing ->
                            pure st

                ReceiveReanimate _ (Err err) ->
                    pure { st | error = Just <| showErr err }
    in
    { init = init, view = view, update = update }


locked : Set String -> Character -> Bool
locked set char =
    not <| Set.isEmpty set || Set.member char.ident set


belongsTo : Set String -> Character -> Bool
belongsTo war char =
    not <| Set.isEmpty <| Set.intersect war char.groups


affordable : Maybe User -> Character -> Bool
affordable muser { price } =
    if price == 0 then
        False

    else
        case muser of
            Nothing ->
                False

            Just user ->
                user.dna >= price


renderWarning : Maybe String -> List (Html msg)
renderWarning mwarning =
    case mwarning of
        Nothing ->
            []

        Just warning ->
            [ H.span [ A.id "userfail" ]
                [ H.text warning ]
            ]



-- CHARWRAPPER


renderWarBadge : War -> Character -> Maybe (Html msg)
renderWarBadge { red, blue } char =
    let
        isRed =
            char |> belongsTo red

        isBlue =
            char |> belongsTo blue
    in
    if isRed && isBlue then
        Just <| H.div [ A.class "redblue" ] []

    else if isRed then
        Just <| H.div [ A.class "red" ] []

    else if isBlue then
        Just <| H.div [ A.class "blue" ] []

    else
        Nothing


charWrapper : Maybe Character -> Model -> Character -> Html Msg
charWrapper mchar { team, unlocked, user, war } char =
    let
        isOn =
            case mchar of
                Just onChar ->
                    onChar.ident == char.ident

                Nothing ->
                    False

        charClass =
            if isOn then
                "char on"

            else if not <| locked unlocked char then
                "char"

            else if affordable user char then
                "char locked buy"

            else
                "char locked"
    in
    H.div [ A.class "charWrapper" ] <|
        Render.charIcon char
            [ A.class charClass
            , E.onClick <| Preview <| PreviewChar char
            , Role.button
            ]
            :: Maybe.values
                [ if Maybe.isNothing user || locked unlocked char then
                    Nothing

                  else if Set.member char.ident team.set then
                    Just <| H.button [ A.class "remove", E.onClick <| ToggleTeam char ] []

                  else if Set.size team.set == Game.teamSize then
                    Nothing

                  else
                    Just <| H.button [ A.class "add", E.onClick <| ToggleTeam char ] []
                , if Maybe.isJust mchar then
                    Nothing

                  else
                    renderWarBadge war char
                ]



-- USERBOX


renderUserBoxNav : { loggedIn : Bool, teamFull : Bool } -> Html Msg
renderUserBoxNav { loggedIn, teamFull } =
    let
        playButton name onClick =
            H.button
                [ A.class "parchment playButton"
                , E.onClick onClick
                , A.disabled <| not teamFull
                ]
                [ H.text name ]
    in
    H.nav [ A.id "playButtons" ] <|
        H.a
            [ A.id "mainsite"
            , A.class "playButton parchment click"
            , A.href "/home"
            ]
            [ H.text "Main Site" ]
            :: (if loggedIn then
                    [ playButton "Start Quick Match" <| Enqueue Quick
                    , playButton "Start Private Match" <| SetStage Searching
                    , playButton "Start Practice Match" <| SetStage Practicing
                    ]

                else
                    []
               )


renderUserBoxLoggedOut : UserBoxFormType -> Csrf -> Html Msg
renderUserBoxLoggedOut formType csrf =
    H.div [ A.id "userBox", A.class "parchment" ]
        [ H.form
            [ A.id <|
                case formType of
                    Login ->
                        "loginForm"

                    Register ->
                        "registerForm"
            , A.class "userForm"
            , A.method "POST"
            , A.action <|
                "/auth/page/email/"
                    ++ (case formType of
                            Login ->
                                "login"

                            Register ->
                                "register"
                       )
            ]
          <|
            [ H.input
                [ A.type_ "hidden"
                , A.name csrf.param
                , A.value csrf.token
                ]
                []
            , H.div []
                [ H.input
                    [ A.class "email"
                    , A.name "email"
                    , A.type_ "email"
                    , A.required True

                    -- , A.autofocus   True
                    , A.placeholder "Email"
                    ]
                    []
                ]
            ]
                ++ (case formType of
                        Login ->
                            [ H.div []
                                [ H.input
                                    [ A.class "password"
                                    , A.name "password"
                                    , A.type_ "password"
                                    , A.required True
                                    , A.placeholder "Password"
                                    ]
                                    []
                                ]
                            , H.div [ A.class "space" ] []
                            , H.div [ A.id "controls" ]
                                [ H.button
                                    [ A.class "playButton click"
                                    , A.type_ "submit"
                                    ]
                                    [ H.text "Log in" ]
                                , H.button
                                    [ A.class "playButton click switch"
                                    , A.type_ "button"
                                    , E.onClick SwitchLogin
                                    ]
                                    [ H.text "Register" ]
                                ]
                            ]

                        Register ->
                            [ H.div [ A.class "space" ] []
                            , H.div [ A.id "controls" ]
                                [ H.button
                                    [ A.class "playButton click switch"
                                    , E.onClick SwitchLogin
                                    , A.type_ "button"
                                    ]
                                    [ H.text "Log in" ]
                                , H.button
                                    [ A.class "playButton click"
                                    , A.type_ "submit"
                                    ]
                                    [ H.text "Register" ]
                                ]
                            ]
                   )
        ]


renderUserBoxLoggedIn : User -> Html Msg
renderUserBoxLoggedIn ({ avatar, clan, dna, level, name, rank, xp } as user) =
    H.div
        [ A.id "userBox"
        , A.class "parchment loggedin"
        , E.onClick <| Preview <| PreviewUser user
        ]
        [ H.img
            [ A.class "userimg"
            , A.src avatar
            ]
            []
        , H.h4 []
            [ H.aside [ A.class "dna" ]
                [ H.text <| String.fromInt dna ]
            , H.text name
            ]
        , H.p [] [ H.text rank ]
        , H.dt [] [ H.text "Clan" ]
        , H.dd [] [ H.text <| Maybe.withDefault "Clanless" clan ]
        , H.dt [] [ H.text "Level" ]
        , H.dd [] [ H.text <| String.fromInt level ++ " (" ++ String.fromInt xp ++ " XP)" ]
        , H.dt [] [ H.text "Rank" ]
        , H.dd [] [ H.text "None" ]
        , H.dt [] [ H.text "Record" ]
        , H.dd [] [ Render.userStreak user ]
        ]


renderUserBox :
    Model
    -> UserBoxFormType
    -> Team
    -> Html Msg
renderUserBox st formType team =
    H.header []
        [ renderUserBoxNav
            { loggedIn = Maybe.isJust st.user
            , teamFull = Set.size team.set == Game.teamSize
            }
        , H.div [ A.class "space" ] []
        , H.section [ A.id "teamContainer" ]
            [ Characters.keyed "div"
                [ A.id "teamButtons"
                , A.class "select"
                ]
                (charWrapper Nothing st)
                team.list
            , H.div [ A.class "space" ] []
            , H.div [ A.id "underTeam", A.class "parchment" ] <|
                Render.chakraTotals team.costs
            ]
        , case st.user of
            Just user ->
                renderUserBoxLoggedIn user

            Nothing ->
                renderUserBoxLoggedOut formType st.csrf
        ]



-- VSBOX


renderVsIcon : Character -> Html Msg
renderVsIcon char =
    Render.charIcon char
        [ A.class "char click"
        , E.onClick <| Vs Delete char
        , Role.button
        ]


renderVsBox : Stage -> List Character -> Html Msg
renderVsBox stage vs =
    let
        meta =
            if List.length vs == Game.teamSize then
                [ A.class "parchment playButton click"
                , E.onClick <| Enqueue Practice
                ]

            else
                [ A.class "parchment playButton" ]
    in
    H.section
        [ A.id "vs"
        , A.classList
            [ ( "parchment", True )
            , ( "vsPractice", stage == Practicing )
            ]
        ]
        [ H.nav []
            [ H.button meta
                [ H.text "Ready" ]
            , H.button
                [ A.class "parchment playButton click"
                , E.onClick <| SetStage Browsing
                ]
                [ H.text "Cancel" ]
            ]
        , H.span [] [ H.text "VS: " ]
        , Characters.keyed "div"
            [ A.id "vsButtons"
            , A.class "select"
            ]
            renderVsIcon
            vs
        ]



-- SEARCHBOX


renderSearchBox : Maybe String -> String -> Html Msg
renderSearchBox error search =
    H.section [ A.id "vs", A.class "parchment" ] <|
        [ H.button
            [ A.class "parchment playButton click"
            , E.onClick <| Enqueue Private
            ]
            [ H.text "Ready" ]
        , H.button
            [ A.class "parchment playButton click"
            , E.onClick <| SetStage Browsing
            ]
            [ H.text "Cancel" ]
        , H.span [] [ H.text "VS: " ]
        , H.input
            [ A.type_ "text"
            , A.name "search"
            , A.value search
            , E.onInput SetSearch
            ]
            []
        ]
            ++ renderWarning error



-- PREVIEWBOX


renderPreviewBox : Model -> Html Msg
renderPreviewBox st =
    case st.previewing of
        PreviewWar ->
            renderWarPreview st.war

        PreviewUser _ ->
            renderUserPreview st.avatars st.error st.form

        PreviewChar char ->
            renderCharPreview st char


renderWar : List (H.Attribute msg) -> Set String -> Html msg
renderWar attrs war =
    war
        |> Set.toList
        |> List.map (H.text >> List.singleton >> H.p [])
        |> H.div attrs


renderWarPreview : War -> Html msg
renderWarPreview { red, blue } =
    H.article [ A.class "parchment war" ]
        [ H.section []
            [ renderWar [ A.class "red" ] red
            , H.h1 [] [ H.text "Today's War" ]
            , renderWar [ A.class "blue" ] blue
            ]
        , H.p [] [ H.text "Choose a side! Make a full team from one side and earn bonus DNA for defeating full teams from the other side." ]
        ]


renderUserPreview : List String -> Maybe String -> Form -> Html Msg
renderUserPreview avatars error { avatar, background, condense, name } =
    H.article [ A.class "parchment" ]
        [ H.div [ A.id "accountSettings" ]
            [ H.p [] <|
                [ H.label [] [ H.text "Name" ]
                , H.input
                    [ A.type_ "text"
                    , A.name "name"
                    , A.value name
                    , E.onInput <| UpdateForm << Name
                    ]
                    []
                ]
                    ++ renderWarning error
            , H.p []
                [ H.label [] [ H.text "Background" ]
                , H.input
                    [ A.type_ "text"
                    , A.name "background"
                    , A.value background
                    , E.onInput <| UpdateForm << Background
                    ]
                    []
                ]
            , H.p []
                [ H.input
                    [ A.type_ "checkbox"
                    , A.name "condense"
                    , A.checked condense
                    , E.onInput <| always <| UpdateForm <| Condense <| not condense
                    ]
                    []
                , H.label []
                    [ H.text "Show only the first version of each character in the selection grid" ]
                ]
            , H.p []
                [ H.span [] [ H.text "Avatars" ] ]
            , H.section [ A.id "avatars" ] <|
                List.map
                    (\ava ->
                        H.button
                            [ A.disabled <| avatar == ava
                            , E.onClick <| UpdateForm <| Avatar ava
                            ]
                            [ H.img [ A.src ava ] [] ]
                    )
                    avatars
            , H.button
                [ A.id "updateButton"
                , E.onClick TryUpdate
                ]
                [ H.text "Update" ]
            , H.a [ A.href "auth/logout" ]
                [ H.button [ A.id "logoutButton" ]
                    [ H.text "Log out" ]
                ]
            ]
        ]


renderCharPreview : Model -> Character -> Html Msg
renderCharPreview st char =
    H.article [ A.class "parchment" ] <|
        [ Characters.keyed "aside" [] (charWrapper (Just char) st) <|
            case Characters.getGroup st.chars char of
                Nothing ->
                    []

                Just (Nonempty _ []) ->
                    []

                Just (Nonempty x xs) ->
                    x :: xs
        , H.h3 [ A.class "charBanner" ] <|
            [ Render.charIcon char [ A.class "char" ]
            , if not <| locked st.unlocked char then
                H.aside [] []

              else if char.price > 0 then
                H.aside [ A.class "dna" ] <|
                    if affordable st.user char then
                        [ H.button [ E.onClick <| Reanimate char ]
                            [ H.text "Reanimate" ]
                        , H.text <| String.fromInt char.price
                        ]

                    else
                        [ H.text <| String.fromInt char.price ]

              else
                H.aside [ A.class "locked" ] <|
                    if List.isEmpty st.mission then
                        [ H.button [ E.onClick <| GetMission char ]
                            [ H.text "Show Mission" ]
                        ]

                    else
                        [ H.button
                            [ E.onClick <| Preview <| PreviewChar char ]
                            [ H.text "Hide Mission" ]
                        ]
            ]
                ++ Render.name char
        , H.p [] <|
            if List.isEmpty st.mission then
                Render.desc char.bio

            else
                [ H.section []
                    [ H.ul [] <| List.map renderObjectivePreview st.mission ]
                ]
        ]
            ++ List.map3 (renderSkillPreview char)
                -- doesn't matter, not the limiting factor
                (List.range 0 10)
                char.skills
                st.alternates


renderObjectivePreview : ObjectiveProgress -> Html Msg
renderObjectivePreview { character, desc, goal, progress } =
    H.li [] <|
        List.concat
            [ case character of
                Nothing ->
                    []

                Just char ->
                    [ H.text <| "As " ++ char ++ ": " ]
            , Render.desc desc
            , if progress < goal then
                [ H.span [ A.class "incomplete" ]
                    [ H.text <|
                        " "
                            ++ String.fromInt progress
                            ++ "/"
                            ++ String.fromInt goal
                    ]
                ]

              else
                [ H.span [ A.class "complete" ]
                    [ H.text <|
                        " "
                            ++ String.fromInt goal
                            ++ "/"
                            ++ String.fromInt goal
                    ]
                ]
            ]


renderSkillPreview : Character -> Int -> List Skill -> Int -> Html Msg
renderSkillPreview char slot skills i =
    case List.getAt i skills of
        Nothing ->
            H.section [] []

        Just skill ->
            let
                skillDesc =
                    Render.skillDesc 0 skill
            in
            H.section []
                [ H.div []
                    [ Render.skillIcon char
                        skill
                        [ A.class "char" ]
                    , H.button
                        [ A.class "prevSkill click"
                        , E.onClick <| Alternate slot -1
                        , A.hidden <| i <= 0
                        ]
                        []
                    , H.button
                        [ A.class "nextSkill click"
                        , E.onClick <| Alternate slot 1
                        , A.hidden <| i + 1 >= List.length skills
                        ]
                        []
                    ]
                , H.h4 [] <|
                    H.text skill.name
                        :: Render.chakras skill.cost
                        ++ [ Render.classes skill.classes ]
                , H.p [] <|
                    if skill.cooldown > 0 then
                        skillDesc ++ [ Render.extra <| "CD: " ++ String.fromInt skill.cooldown ]

                    else
                        skillDesc
                ]



-- LISTCHARS


renderCharList : Model -> Html Msg
renderCharList ({ chars, condense, index, pageSize, stage } as st) =
    let
        hasMore =
            (index + pageSize)
                < (if condense then
                    chars.groupSize

                   else
                    chars.size
                  )

        wrap xs =
            xs
                |> List.drop index
                |> List.take pageSize
    in
    H.section
        [ A.class "chars"
        , A.class "parchment"
        , A.id "forTeam"
        , A.hidden <| stage == Practicing
        ]
        [ Render.scroll [ A.id "prevPage" ] "left" (index /= 0) <| Page -1
        , Render.scroll [ A.id "nextPage" ] "right" hasMore <| Page 1
        , Characters.keyed "div"
            [ A.id "teamScroll"
            , A.class "charScroll"
            ]
            (charWrapper Nothing st)
          <|
            if condense then
                chars.groupList
                    |> wrap
                    |> List.map Nonempty.head

            else
                chars.list
                    |> wrap
        ]



-- LISTVS


renderVsChar : List Character -> Character -> Html Msg
renderVsChar vs char =
    H.div [ A.class "charWrapper", A.title char.name ]
        [ Render.charIcon char
            [ A.class "char"
            , E.onClick <| Vs Add char
            , A.disabled <| List.member char vs
            , Role.button
            ]
        ]


renderVsList : Model -> Html Msg
renderVsList { chars, stage, vs } =
    H.section
        [ A.id "forVs"
        , A.class "parchment chars"
        , A.hidden <| stage /= Practicing
        ]
        [ Characters.keyed "div" [ A.class "charScroll" ] (renderVsChar vs) <|
            chars.list
        ]
