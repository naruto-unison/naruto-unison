module Site.Render exposing
  ( chakras
  , class
  , classes
  , duration
  , effect
  , rands
  , desc
  , icon
  , name
  )

import Dict            as Dict exposing (Dict)
import Html            as H exposing (Html)
import Html.Attributes as A
import Parser exposing (Parser, (|=), (|.))
import Set exposing (Set)
import String.Extra as String
import Tuple exposing (first)

import Import.Flags exposing (Flags, characterName)
import Game.Game as Game
import Import.Model exposing (Category(..), Chakras, Channel, Channeling(..), Character, Effect)
import Util exposing (elem)

chakras : Chakras -> List (Html msg)
chakras = List.map chakra << fromChakras

chakra : String -> Html msg
chakra s = H.div [A.class <| "chakra " ++ s] []

fromChakras : Chakras -> List String
fromChakras x = List.repeat x.blood "blood"
             ++ List.repeat x.gen   "gen"
             ++ List.repeat x.nin   "nin"
             ++ List.repeat x.tai   "tai"
             ++ List.repeat x.rand  "rand"

rands : Int -> Int -> Html msg
rands amount random =
    H.div [A.class "randbar"]
    [ H.span [A.class "randT"] [H.text "T"]
    , H.span [] [H.text <| String.fromInt amount]
    , H.a [A.class "more noclick"] [H.text "+"]
    , H.a [A.class "less noclick"] [H.text "—"]
    , H.div [A.class "chakra rand"] []
    , H.span [] [H.text <| String.fromInt random]
    ]

icon : Character -> String -> List (H.Attribute msg) -> Html msg
icon char path attrs =
  let
    src = "/img/ninja/" ++ shorten (characterName char)
          ++ "/" ++ shorten path ++ ".jpg"
  in
    H.img (A.src src :: attrs) []

shorten : String -> String
shorten = String.map simplify <<
          String.filter (not << elem illegal)

illegal : List Char
illegal = [' ','-',':','(',')','®','.','/','?', '\'']

simplify : Char -> Char
simplify c = case c of
    'ō' -> 'o'
    'Ō' -> 'O'
    'ū' -> 'u'
    'Ū' -> 'U'
    'ä' -> 'a'
    _   -> c

name : Character -> List (Html msg)
name char = case char.category of
    Original   -> [H.text char.name]
    Shippuden  -> [H.text char.name, H.sup [] [H.text "𝕊"]]
    Reanimated -> [H.text char.name, H.sup [] [H.text "ℝ"]]

duration : String -> Int -> List (Html msg)
duration ifEmpty x = case x of
    0 -> [H.text ifEmpty]
    _ -> [H.text << String.fromInt <| (x + 1) // 2]

class : Channel -> String -> H.Attribute msg
class x others = A.class <| case x.dur of
    Action _  -> others ++ " action"
    Control _ -> others ++ " control"
    _         -> others

classes : Bool -> Set String -> Html msg
classes hideMore xs =
    H.p [A.class "skillClasses"] << List.singleton <<
    H.text << String.join ", " << Set.toList << Set.diff xs <|
    if hideMore then moreHidden else hidden

effect : (Effect -> Bool) -> Effect -> Html msg
effect removable x =
  let
    meta =
      if x.trap then
        [A.class "trap"]
      else if removable x then
        [A.class "remove"]
      else
        []
  in
    H.li meta [H.text x.desc]

hidden : Set String
hidden = Set.fromList
    [ "Non-stacking"
    , "Extending"

    , "Necromancy"

    , "All"
    , "Healing"
    , "Hidden"
    , "Affliction"
    , "Non-affliction"
    , "Non-mental"
    , "Non-mental"
    , "Resource"
    , "Direct"

    , "Bloodline"
    , "Genjutsu"
    , "Ninjutsu"
    , "Taijutsu"
    , "Random"
    ]

moreHidden : Set String
moreHidden = Set.union hidden <| Set.fromList
    [ "Bypassing"
    , "Uncounterable"
    , "Unreflectable"
    ]

parseChakra : String -> Parser (Html msg)
parseChakra kind =
  let
    tag = case String.uncons kind of
        Just (head, _) -> String.fromList ['[', head, ']']
        Nothing        -> ""
  in
    Parser.succeed (chakra kind)
    |. Parser.symbol tag

parseName : Parser (Html msg)
parseName =
    Parser.succeed (H.i [] << List.singleton << H.text)
    |. Parser.symbol "["
    |= Parser.getChompedString (Parser.chompWhile ((/=) ']'))
    |. Parser.symbol "]"

parseText : Parser (Html msg)
parseText =
    Parser.succeed H.text
    |= Parser.getChompedString (Parser.chompWhile ((/=) '['))

parseDesc : Parser (List (Html msg))
parseDesc = Parser.loop [] <| \acc ->
    Parser.oneOf
      [ Parser.succeed ()
        |. Parser.end
        |> Parser.map (\_ -> Parser.Done (List.reverse acc))
      , Parser.succeed (\stmt -> Parser.Loop (stmt :: acc)) |= Parser.oneOf
        [ parseChakra "blood"
        , parseChakra "gen"
        , parseChakra "nin"
        , parseChakra "tai"
        , parseChakra "rand"
        , parseName
        , parseText
        ]
      , Parser.succeed ()
          |> Parser.map (\_ -> Parser.Done (List.reverse acc))
      ]

desc : String -> List (Html msg)
desc s = case Parser.run parseDesc s of
    Ok els -> els
    Err e  -> [H.text <| Parser.deadEndsToString e]
