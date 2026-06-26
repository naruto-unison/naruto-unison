module Util exposing
    ( ListChange(..)
    , clickIf
    , groupBy
    , illegal
    , pure
    , reverseIf
    , shorten
    , showBool
    , showErr
    , unaccent
    )

import Html as H
import Html.Attributes as A
import Html.Events as E
import Http exposing (Error(..))
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))


clickIf : Bool -> String -> msg -> List (H.Attribute msg)
clickIf condition class command =
    if condition then
        [ A.class <| class ++ " click", E.onClick command ]

    else
        [ A.class <| class ++ " noclick" ]


reverseIf : Bool -> List a -> List a
reverseIf condition xs =
    if condition then
        List.reverse xs

    else
        xs


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


showErr : Error -> String
showErr err =
    case err of
        BadUrl x ->
            "Bad url: " ++ x

        Timeout ->
            "Connection timed out"

        NetworkError ->
            "Network error"

        BadStatus x ->
            "Received error " ++ String.fromInt x

        BadBody x ->
            "Invalid response from server: " ++ x


shorten : String -> String
shorten =
    String.filter (\c -> not <| List.member c illegal)
        >> String.map unaccent


illegal : List Char
illegal =
    String.toList " -:()®./?'"


unaccent : Char -> Char
unaccent c =
    case c of
        'ō' ->
            'o'

        'Ō' ->
            'O'

        'ū' ->
            'u'

        'Ū' ->
            'U'

        'ä' ->
            'a'

        _ ->
            c
