module Import.Flags exposing
    ( Csrf
    , Flags
    , War
    , clean
    , decode
    , failure
    , printFailure
    )

import Game.Characters as Characters exposing (Characters)
import Import.Model as Model exposing (QueueFailure(..), User)
import Json.Decode as D
import Json.Helpers as D
import List.Nonempty exposing (Nonempty(..))
import Set exposing (Set)
import String.Extra as String
import Util exposing (unaccent)


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
    , characters = Characters.create []
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
        >> D.required "characters" (D.list Model.jsonDecCharacter |> D.map Characters.create)
        >> D.required "war" decodeWar
        >> D.required "csrf" decodeCsrf


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


printFailure : QueueFailure -> String
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
