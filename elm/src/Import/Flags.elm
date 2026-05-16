module Import.Flags exposing
    ( Characters
    , Flags
    , characterName
    , decode
    , failure
    , printFailure
    )

import Dict as Dict exposing (Dict)
import Json.Decode as D
import Json.Helpers as D
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Set exposing (Set)
import String.Extra as String

import Game.Chakra as Chakra
import Import.Model as Model exposing (Category(..), Chakras, Character, Failure(..), Skill, User)
import Util exposing (groupBy, unaccent)


type alias Flags =
    { url          : String
    , bg           : String
    , userTeam     : List Character
    , userPractice : List Character
    , unlocked     : Set String
    , user         : Maybe User
    , avatars      : List String
    , characters   : Characters
    , visibles     : Set String
    , red          : Set String
    , blue         : Set String
    , csrf         : String
    , csrfParam    : String
    }


failure : Flags
failure =
    { url          = ""
    , bg           = ""
    , userTeam     = []
    , userPractice = []
    , unlocked     = Set.empty
    , user         = Nothing
    , avatars      = []
    , characters   = makeCharacters []
    , visibles     = Set.empty
    , red          = Set.empty
    , blue         = Set.empty
    , csrf         = ""
    , csrfParam    = ""
    }


decode : D.Decoder Flags
decode =
    D.succeed Flags
        |> D.required "url"          D.string
        >> D.required "bg"           D.string
        >> D.required "userTeam"     (D.list Model.jsonDecCharacter)
        >> D.required "userPractice" (D.list Model.jsonDecCharacter)
        >> D.required "unlocked"     (D.list D.string |> D.map Set.fromList)
        >> D.required "user"         (D.maybe Model.jsonDecUser)
        >> D.required "avatars"      (D.list D.string)
        >> D.required "characters"   (D.list Model.jsonDecCharacter |> D.map makeCharacters)
        >> D.required "visibles"     (D.list D.string |> D.map Set.fromList)
        >> D.required "red"          (D.list D.string |> D.map Set.fromList)
        >> D.required "blue"         (D.list D.string |> D.map Set.fromList)
        >> D.required "csrf"         D.string
        >> D.required "csrfParam"    D.string


type alias Characters =
    { list      : List Character
    , dict      : Dict String Character
    , groupList : List (Nonempty Character)
    , groupDict : Dict String (Nonempty Character)
    , costs     : Dict String Chakras
    , shortName : Character -> String
    }


makeCharacters : List Character -> Characters
makeCharacters chars =
    let
        shortNames =
            chars
                |> List.map (\x -> ( characterName x, makeShortName x ))
                >> Dict.fromList

        shortName char =
            case Dict.get (characterName char) shortNames of
                Just name -> name
                Nothing   -> makeShortName char

        groupList =
            groupBy (\x y -> shortName x == shortName y) chars
    in
    { list =
        chars
    , dict =
        Dict.fromList
            <| withKey characterName chars
    , groupList =
        groupList
    , groupDict =
        Dict.fromList
            <| withKey (Nonempty.head >> shortName) groupList
    , costs =
        Dict.fromList
            <| List.map (\char -> (shortName char, characterCosts char)) chars
    , shortName =
        shortName
    }


withKey : (a -> b) -> List a -> List ( b, a )
withKey f =
    List.map <| \x -> ( f x, x )


clean : Char -> Char
clean x =
    case x of
        ' ' -> '-'
        _   -> unaccent x


characterName : Character -> String
characterName char =
    String.map clean << String.toLower <| case char.category of
        Original ->
            char.name

        Shippuden ->
            char.name ++ " (S)"

        Reanimated ->
            char.name ++ " (R)"


makeShortName : Character -> String
makeShortName char =
    case char.name of
        "Killer B"         -> "B"
        "Shukaku Gaara"    -> "Gaara"
        "Sage Mode Kabuto" -> "Kabuto"
        "Tobi"             -> "Obito"
        "Masked Man"       -> "Obito"
        "Nagato"           -> "Pain"
        _                  ->
            char.skills
                |> List.getAt 3
                >> Maybe.andThen List.head
                >> Maybe.andThen shortFromInvuln
                >> Maybe.withDefault char.name


shortFromInvuln : Skill -> Maybe String
shortFromInvuln x =
    case String.words x.desc of
        "The" :: name :: _ -> Just name
        name :: _          -> Just name
        []                 -> Nothing


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

        NotFound ->
            "User not found"
