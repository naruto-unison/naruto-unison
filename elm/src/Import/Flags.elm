module Import.Flags exposing
    ( Characters
    , Csrf
    , Flags
    , War
    , clean
    , decode
    , failure
    , printFailure
    )

import Dict exposing (Dict)
import Game.Chakra as Chakra
import Import.Model as Model exposing (Category(..), Chakras, Character, Failure(..), Skill, User)
import Json.Decode as D
import Json.Helpers as D
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Set exposing (Set)
import String.Extra as String
import Util exposing (groupBy, unaccent)


type alias Csrf =
    { param : String
    , token : String
    }


decodeCsrf : D.Decoder Csrf
decodeCsrf =
    D.succeed Csrf
        |> D.required "param" D.string
        >> D.required "token" D.string


type alias War =
    { red : Set String
    , blue : Set String
    }


decodeWar =
    D.succeed War
        |> D.required "red" (D.list D.string |> D.map Set.fromList)
        >> D.required "blue" (D.list D.string |> D.map Set.fromList)


type alias Flags =
    { url : String
    , bg : String
    , userTeam : List String
    , userPractice : List String
    , unlocked : Set String
    , user : Maybe User
    , avatars : List String
    , characters : Characters
    , visibles : Set String
    , war : War
    , csrf : Csrf
    }


failure : Flags
failure =
    { url = ""
    , bg = ""
    , userTeam = []
    , userPractice = []
    , unlocked = Set.empty
    , user = Nothing
    , avatars = []
    , characters = makeCharacters []
    , visibles = Set.empty
    , war =
        { red = Set.empty
        , blue = Set.empty
        }
    , csrf = { param = "", token = "" }
    }


decode : D.Decoder Flags
decode =
    D.succeed Flags
        |> D.required "url" D.string
        >> D.required "bg" D.string
        >> D.required "userTeam" (D.list D.string)
        >> D.required "userPractice" (D.list D.string)
        >> D.required "unlocked" (D.list D.string |> D.map Set.fromList)
        >> D.required "user" (D.maybe Model.jsonDecUser)
        >> D.required "avatars" (D.list D.string)
        >> D.required "characters" (D.list Model.jsonDecCharacter |> D.map makeCharacters)
        >> D.required "visibles" (D.list D.string |> D.map Set.fromList)
        >> D.required "war" decodeWar
        >> D.required "csrf" decodeCsrf


type alias Characters =
    { list : List Character
    , dict : Dict String Character
    , groupList : List (Nonempty Character)
    , groupDict : Dict String (Nonempty Character)
    , costs : Dict String Chakras
    , shortName : Character -> String
    }


makeCharacters : List Character -> Characters
makeCharacters chars =
    let
        shortNames =
            chars
                |> List.map (\x -> ( x.ident, makeShortName x ))
                >> Dict.fromList

        shortName char =
            case Dict.get char.ident shortNames of
                Just name ->
                    name

                Nothing ->
                    makeShortName char

        groupList =
            groupBy (\x y -> shortName x == shortName y) chars
    in
    { list =
        chars
    , dict =
        Dict.fromList <|
            withKey .ident chars
    , groupList =
        groupList
    , groupDict =
        Dict.fromList <|
            withKey (Nonempty.head >> shortName) groupList
    , costs =
        Dict.fromList <|
            List.map (\char -> ( shortName char, characterCosts char )) chars
    , shortName =
        shortName
    }


withKey : (a -> b) -> List a -> List ( b, a )
withKey f =
    List.map <| \x -> ( f x, x )


clean : String -> String
clean =
    String.map cleanChar << String.toLower


cleanChar : Char -> Char
cleanChar x =
    case x of
        ' ' ->
            '-'

        _ ->
            unaccent x


makeShortName : Character -> String
makeShortName char =
    case char.name of
        "Killer B" ->
            "B"

        "Shukaku Gaara" ->
            "Gaara"

        "Sage Mode Kabuto" ->
            "Kabuto"

        "Tobi" ->
            "Obito"

        "Masked Man" ->
            "Obito"

        "Nagato" ->
            "Pain"

        _ ->
            char.skills
                |> List.getAt 3
                >> Maybe.andThen List.head
                >> Maybe.andThen shortFromInvuln
                >> Maybe.withDefault char.name


shortFromInvuln : Skill -> Maybe String
shortFromInvuln x =
    case String.words x.desc of
        "The" :: name :: _ ->
            Just name

        name :: _ ->
            Just name

        [] ->
            Nothing


characterCosts : Character -> Chakras
characterCosts char =
    char.skills
        |> List.filterMap List.head
        >> List.map .cost
        >> Chakra.sum


printFailure : Failure -> String
printFailure x =
    case x of
        AlreadyQueued ->
            "Your account is already queued"

        Canceled ->
            "Queue canceled"

        Locked chars ->
            "Characters not unlocked: " ++ String.join ", " chars

        InvalidTeam _ ->
            "Invalid team"

        SocketError e ->
            "Socket error: " ++ e

        NotFound ->
            "User not found"
