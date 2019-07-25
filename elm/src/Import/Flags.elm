module Import.Flags exposing
  ( Characters
  , Flags
  , decode, failure
  , characterName
  )

import Dict          as Dict exposing (Dict)
import Json.Decode   as D exposing (Value)
import Html          as H exposing (Html)
import List.Extra    as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import String.Extra  as String

import Import.Model as Model exposing (Category(..), Character, User)
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
    }

decode : D.Decoder Flags
decode =
    D.map8 Flags
    (D.field "url"          <| D.string)
    (D.field "bg"           <| D.string)
    (D.field "userTeam"     <| D.list Model.jsonDecCharacter)
    (D.field "userPractice" <| D.list Model.jsonDecCharacter)
    (D.field "user"         <| D.maybe Model.jsonDecUser)
    (D.field "avatars"      <| D.list D.string)
    (D.field "characters"   << D.map makeCharacters
                            <| D.list Model.jsonDecCharacter)
    (D.field "csrf"         <| D.string)

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
    , dict      = Dict.fromList <| withKey shortName chars
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
    "Tobi"             -> "Obito"
    "The Masked Man"   -> "Obito"
    "Nagato"           -> "Pain"
    "Shukaku Gaara"    -> "Gaara"
    "Sage Mode Kabuto" -> "Kabuto"
    _                  -> List.getAt 3 char.skills
                          |> Maybe.andThen List.head
                          >> Maybe.map (String.leftOf " " << .desc)
                          >> Maybe.withDefault char.name
