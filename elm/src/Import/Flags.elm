module Import.Flags exposing
  ( Characters
  , Flags
  , decode, failure
  , characterName
  , printFailure
  )

import Dict          as Dict exposing (Dict)
import Json.Decode   as D exposing (Value)
import Json.Helpers  as D
import Html          as H exposing (Html)
import List.Extra    as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import String.Extra  as String

import Import.Model as Model exposing (Category(..), Character, Failure(..), Skill, User)
import Util exposing (groupBy)

type alias Flags =
    { url:          String
    , bg:           String
    , userTeam:     List Character
    , userPractice: List Character
    , user:         Maybe User
    , avatars:      List String
    , characters:   Characters
    , csrf:         String
    , csrfParam:    String
    }

failure : Flags
failure =
    { url          = ""
    , bg           = ""
    , userTeam     = []
    , userPractice = []
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
    |> D.required "bg"           D.string
    |> D.required "userTeam"     (D.list Model.jsonDecCharacter)
    |> D.required "userPractice" (D.list Model.jsonDecCharacter)
    |> D.required "user"         (D.maybe Model.jsonDecUser)
    |> D.required "avatars"      (D.list D.string)
    |> D.required "characters"   (D.map makeCharacters <| D.list Model.jsonDecCharacter)
    |> D.required  "csrf"        D.string
    |> D.required  "csrfParam"   D.string

type alias Characters =
    { list       : List Character
    , dict       : Dict String Character
    , groupList  : List (Nonempty Character)
    , groupDict  : Dict String (Nonempty Character)
    , shortName  : Character -> String
    }

makeCharacters : List Character -> Characters
makeCharacters chars =
  let
    shortNames     = Dict.fromList <|
                     List.map (\x -> (characterName x, makeShortName x)) chars
    shortName char = case Dict.get (characterName char) shortNames of
                         Just name -> name
                         Nothing   -> makeShortName char
    groupList      = groupBy (\x y -> shortName x == shortName y) chars
  in
    { list      = chars
    , dict      = Dict.fromList <| withKey characterName chars
    , groupList = groupList
    , groupDict = Dict.fromList <|
                  withKey (shortName << Nonempty.head) groupList
    , shortName = shortName
    }

withKey : (a -> b) -> List a -> List (b, a)
withKey f = List.map <| \x -> (f x, x)

characterName : Character -> String
characterName char = case char.category of
    Original   -> char.name
    Shippuden  -> char.name ++ " (S)"
    Reanimated -> char.name ++ " (R)"

makeShortName : Character -> String
makeShortName char = case char.name of
    "Nagato"           -> "Pain"
    "Sage Mode Kabuto" -> "Kabuto"
    "Shukaku Gaara"    -> "Gaara"
    "Tobi"             -> "Obito"
    "Killer B"         -> "B"
    _                  -> List.getAt 3 char.skills
                          |> Maybe.andThen List.head
                          >> Maybe.andThen shortFromInvuln
                          >> Maybe.withDefault char.name

shortFromInvuln : Skill -> Maybe String
shortFromInvuln x = case String.words x.desc of
    "The" :: name :: _ -> Just name
    name :: _          -> Just name
    []                 -> Nothing


printFailure : Failure -> String
printFailure x = case x of
    AlreadyQueued    -> "Your account is already queued"
    Canceled         -> "Queue canceled"
    InvalidTeam      -> "Invalid team"
    OpponentNotFound -> "User not found"
