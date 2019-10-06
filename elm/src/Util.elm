module Util exposing
    ( ListChange(..)
    , elem
    , groupBy
    , pure
    , showErr
    )

import Http exposing (Error(..))
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))


elem : List a -> a -> Bool
elem xs x =
    List.member x xs


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
