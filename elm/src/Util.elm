module Util exposing
    ( ListChange(..)
    , groupBy
    , illegal
    , pure
    , shorten
    , showBool
    , showErr
    , unaccent
    )

import Http exposing (Error(..))
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))


groupBy : (a -> a -> Bool) -> List a -> List (Nonempty a)
groupBy pred xxs =
    case List.uncons xxs of
        Nothing ->
            []

        Just ( x, xs ) ->
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
