module Site.Render exposing
    ( chakraTotals
    , chakras
    , charIcon
    , class
    , classes
    , desc
    , detailIcon
    , duration
    , effect
    , icon
    , name
    , rands
    , scroll
    , skillIcon
    , streak
    )

import Game.Detail exposing (Detail)
import Game.Game as Game
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Import.Flags exposing (characterName, clean)
import Import.Model exposing (Category(..), Chakras, Channel, Channeling(..), Character, Effect, Skill, User)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import String.Extra as String
import Util exposing (shorten)


scroll : String -> String -> msg -> Html msg
scroll id src cmd =
    H.button [ A.id id, A.class "scroll click" ]
        [ H.div [] []
        , H.img [ A.src <| "/img/ui/scroll/" ++ src ++ ".png", E.onClick cmd ] []
        ]


streak : User -> Html msg
streak user =
    H.text <|
        String.fromInt user.wins
            ++ " - "
            ++ String.fromInt user.losses
            ++ " (+"
            ++ String.fromInt user.streak
            ++ ")"


chakras : Chakras -> List (Html msg)
chakras =
    fromChakras
        >> List.map chakra


chakra : String -> Html msg
chakra s =
    H.div [ A.class <| "chakra " ++ s ] []


fromChakras : Chakras -> List String
fromChakras x =
    List.repeat x.blood "blood"
        ++ List.repeat x.gen "gen"
        ++ List.repeat x.nin "nin"
        ++ List.repeat x.tai "tai"
        ++ List.repeat x.rand "rand"


rands : Int -> Int -> Html msg
rands amount random =
    H.div [ A.class "randbar" ]
        [ H.span [ A.class "randT" ]
            [ H.text "T" ]
        , H.span []
            [ H.text <| String.fromInt amount ]
        , H.a [ A.class "more noclick" ]
            [ H.text "+" ]
        , H.a [ A.class "less noclick" ]
            [ H.text "—" ]
        , H.div [ A.class "chakra rand" ]
            []
        , H.span []
            [ H.text <| String.fromInt random ]
        ]


chakraTotals : Chakras -> List (Html msg)
chakraTotals x =
    let
        named chak total =
            H.span [] [ chakra chak, H.text <| String.fromInt total ]
    in
    [ named "blood" x.blood
    , named "gen" x.gen
    , named "nin" x.nin
    , named "tai" x.tai
    , named "rand" x.rand
    ]


iconBase : String -> String -> List (H.Attribute msg) -> Html msg
iconBase char path attrs =
    let
        src =
            "/img/ninja/"
                ++ char
                ++ "/"
                ++ shorten path
                ++ ".jpg"
    in
    H.img (A.src src :: attrs) []


icon : Character -> String -> List (H.Attribute msg) -> Html msg
icon char =
    iconBase (characterName char)


charIcon : Character -> List (H.Attribute msg) -> Html msg
charIcon character =
    icon character "icon"


getReanimationName : String -> Set String -> Maybe String
getReanimationName skillName skillClasses =
    if Set.member "Reanimation" skillClasses then
        (List.head <| String.indexes ":" skillName)
            |> Maybe.map (\index -> String.left index skillName)

    else
        Nothing


cleanReanimationName : String -> String
cleanReanimationName reanimationName = clean reanimationName ++ "-(r)"


skillIcon : Character -> Skill -> List (H.Attribute msg) -> Html msg
skillIcon character skill =
    case getReanimationName skill.name skill.classes of
        Just reanimationName ->
            iconBase (cleanReanimationName reanimationName) <|
                String.dropLeft (String.length reanimationName + 2) skill.name

        Nothing ->
            icon character skill.name


detailIcon : Character -> Detail -> List (H.Attribute msg) -> Html msg
detailIcon character detail =
    case getReanimationName detail.skillName detail.classes of
        Just reanimationName ->
            iconBase (cleanReanimationName reanimationName) <|
                if String.startsWith reanimationName detail.name then
                    String.dropLeft (String.length reanimationName + 2) detail.name
                else
                    detail.name
        Nothing ->
            icon character detail.name


name : Character -> List (Html msg)
name char =
    case char.category of
        Original ->
            [ H.text char.name ]

        Shippuden ->
            [ H.text char.name
            , H.sup []
                [ H.text "𝕊" ]
            ]

        Reanimated ->
            [ H.text char.name
            , H.sup []
                [ H.text "ℝ" ]
            ]


duration : String -> Int -> List (Html msg)
duration ifEmpty x =
    case x of
        0 ->
            [ H.text ifEmpty ]

        1 ->
            [ H.text "1" ]

        _ ->
            [ H.text << String.fromInt <| x // 2 ]


class : Channel -> String -> H.Attribute msg
class x others =
    A.class <|
        case x.dur of
            Action _ ->
                others ++ " action"

            Control _ ->
                others ++ " control"

            _ ->
                others


classes : Bool -> Set String -> Html msg
classes hideMore xs =
    (if hideMore then
        Set.diff xs moreHidden

     else
        xs
    )
        |> Set.toList
        >> String.join ", "
        >> H.text
        >> List.singleton
        >> H.p [ A.class "skillClasses" ]


effect : List Character -> (Effect -> Bool) -> Effect -> Html msg
effect characters removable x =
    let
        meta =
            if x.trap then
                [ A.class "trap" ]

            else if removable x then
                [ A.class "remove" ]

            else
                []
    in
    H.li meta
        << desc
    <|
        case x.slot of
            Nothing ->
                x.desc

            Just slot ->
                x.desc ++ (Game.get characters slot).name ++ "."


moreHidden : Set String
moreHidden =
    Set.fromList
        [ "Bypassing"
        , "Uncounterable"
        , "Unreflectable"
        ]


parseBreak : Parser (Html msg)
parseBreak =
    Parser.succeed (H.br [] [])
        |. Parser.token "\n"


parseChakra : String -> Parser (Html msg)
parseChakra kind =
    let
        tag =
            case String.uncons kind of
                Just ( head, _ ) ->
                    String.fromList [ '[', head, ']' ]

                Nothing ->
                    ""
    in
    Parser.succeed (chakra kind)
        |. Parser.symbol tag


parseShippuden : Parser (Html msg)
parseShippuden =
    Parser.succeed (H.sup [] [ H.text "𝕊" ])
        |. Parser.symbol "(S)"


parseReanimated : Parser (Html msg)
parseReanimated =
    Parser.succeed (H.sup [] [ H.text "ℝ" ])
        |. Parser.symbol "(R)"


parseParen : Parser (Html msg)
parseParen =
    Parser.succeed (H.text "(")
        |. Parser.symbol "("


parseName : Parser (Html msg)
parseName =
    Parser.succeed (H.i [] << List.singleton << H.text)
        |. Parser.symbol "["
        |= Parser.getChompedString (Parser.chompWhile ((/=) ']'))
        |. Parser.symbol "]"


parseText : Parser (Html msg)
parseText =
    Parser.succeed H.text
        |= Parser.getChompedString
            (Parser.chompWhile <| \c -> c /= '[' && c /= '\n' && c /= '(')


parseSuccess : Parser (Html msg)
parseSuccess =
    Parser.oneOf
        [ parseBreak
        , parseShippuden
        , parseReanimated
        , parseChakra "blood"
        , parseChakra "gen"
        , parseChakra "nin"
        , parseChakra "tai"
        , parseChakra "rand"
        , parseName
        , parseShippuden
        , parseReanimated
        , parseParen
        , parseText
        ]


parseFail : Parser (Html msg)
parseFail =
    Parser.succeed H.text
        |= Parser.getChompedString (Parser.chompUntilEndOr "\n")


parseDesc : Parser (List (Html msg))
parseDesc =
    Parser.loop [] <|
        \acc ->
            Parser.oneOf
                [ Parser.succeed ()
                    |. Parser.end
                    |> Parser.map (\_ -> Parser.Done (List.reverse acc))
                , Parser.succeed (\stmt -> Parser.Loop (stmt :: acc))
                    |= Parser.oneOf
                        [ Parser.backtrackable parseSuccess
                        , parseFail
                        ]
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse acc))
                ]


desc : String -> List (Html msg)
desc s =
    case Parser.run parseDesc s of
        Ok els ->
            els

        Err e ->
            [ H.text <| Parser.deadEndsToString e ]
