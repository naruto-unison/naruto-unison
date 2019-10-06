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
import Util exposing (groupBy)

import Import.Model as Model exposing (Category(..), Character, Failure(..), Skill, User)


type alias Flags =
    { url          : String
    , bg           : String
    , userTeam     : List Character
    , userPractice : List Character
    , unlocked     : Set String
    , user         : Maybe User
    , avatars      : List String
    , characters   : Characters
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
        >> D.required "csrf"         D.string
        >> D.required "csrfParam"    D.string


type alias Characters =
    { list      : List Character
    , dict      : Dict String Character
    , groupList : List (Nonempty Character)
    , groupDict : Dict String (Nonempty Character)
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
        Dict.fromList <| withKey characterName chars
    , groupList =
        groupList
    , groupDict =
        Dict.fromList <| withKey (Nonempty.head >> shortName) groupList
    , shortName =
        shortName
    }


withKey : (a -> b) -> List a -> List ( b, a )
withKey f =
    List.map <| \x -> ( f x, x )


characterName : Character -> String
characterName char =
    case char.category of
        Original   -> char.name
        Shippuden  -> char.name ++ " (S)"
        Reanimated -> char.name ++ " (R)"


makeShortName : Character -> String
makeShortName char =
    case char.name of
        "Nagato"           -> "Pain"
        "Sage Mode Kabuto" -> "Kabuto"
        "Shukaku Gaara"    -> "Gaara"
        "Tobi"             -> "Obito"
        "Killer B"         -> "B"
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


printFailure : Failure -> String
printFailure x =
    case x of
        AlreadyQueued ->
            "Your account is already queued"

        Canceled ->
            "Queue canceled"

        Locked ->
            "Character not unlocked"

        InvalidTeam ->
            "Invalid team"

        NotFound ->
            "User not found"
