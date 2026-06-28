module Util exposing
    ( ListChange(..)
    , clickIf
    , groupBy
    , pure
    , shorten
    , showBool
    , showErr
    , unaccent
    , withKey
    )

import Dict exposing (Dict)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Http
import List.Nonempty exposing (Nonempty(..))
import Set exposing (Set)


clickIf : Bool -> String -> msg -> List (H.Attribute msg)
clickIf condition class command =
    if condition then
        [ A.class <| class ++ " click", E.onClick command ]

    else
        [ A.class <| class ++ " noclick" ]


groupBy : (a -> a -> Bool) -> List a -> List (Nonempty a)
groupBy pred xxs =
    case xxs of
        [] ->
            []

        x :: xs ->
            (Nonempty x <| List.filter (pred x) xs)
                :: (groupBy pred <| List.filter (not << pred x) xs)


type ListChange
    = Add
    | Delete


pure : a -> ( a, Cmd msg )
pure x =
    ( x, Cmd.none )


showBool : Bool -> String
showBool b =
    if b then
        "True"

    else
        "False"


showErr : Http.Error -> String
showErr err =
    case err of
        Http.BadUrl x ->
            "Bad url: " ++ x

        Http.Timeout ->
            "Connection timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus x ->
            "Received error " ++ String.fromInt x

        Http.BadBody x ->
            "Invalid response from server: " ++ x


withKey : (a -> b) -> List a -> List ( b, a )
withKey f =
    List.map <| \x -> ( f x, x )


shorten : String -> String
shorten =
    String.filter isLegal
        >> String.map unaccent


isLegal : Char -> Bool
isLegal c =
    not <| Set.member c illegal


illegal : Set Char
illegal =
    " -:()®./?'"
        |> String.toList
        >> Set.fromList


unaccentDict : Dict Char Char
unaccentDict =
    Dict.fromList
        [ ( 'ō', 'o' )
        , ( 'Ō', 'O' )
        , ( 'ū', 'u' )
        , ( 'Ū', 'U' )
        , ( 'ä', 'a' )
        ]


unaccent : Char -> Char
unaccent c =
    Dict.get c unaccentDict
        |> Maybe.withDefault c
