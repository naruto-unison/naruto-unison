module Game.Characters exposing
    ( Characters
    , create
    , get
    , getGroup
    , merge
    , root
    )

import Dict exposing (Dict)
import Import.Model exposing (Category(..), Character, Ninja, Skill)
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty)
import Set
import Util exposing (groupBy, withKey)


type alias Characters =
    { list : List Character
    , dict : Dict String Character
    , groupList : List (Nonempty Character)
    , groupDict : Dict String (Nonempty Character)
    , shortName : Character -> String
    }


get : List Character -> Int -> Character
get xs slot =
    xs
        |> List.getAt slot
        >> Maybe.withDefault unknown


root : List Character -> Skill -> Character
root characters skill =
    get characters skill.owner


mergeSkill : Skill -> List Skill -> List Skill
mergeSkill skill =
    let
        { name, owner } =
            skill
    in
    List.map <|
        \otherSkill ->
            if otherSkill.name == name then
                skill

            else
                { otherSkill | owner = owner }


merge : Characters -> Ninja -> Character
merge chars n =
    let
        char =
            Dict.get n.character chars.dict
                |> Maybe.withDefault unknown
    in
    { char | skills = List.map2 mergeSkill n.skills char.skills }


create : List Character -> Characters
create chars =
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
            chars |> groupBy (\x y -> shortName x == shortName y)
    in
    { list = chars
    , dict = Dict.fromList <| withKey .ident chars
    , groupList = groupList
    , groupDict = Dict.fromList <| withKey (Nonempty.head >> shortName) groupList
    , shortName = shortName
    }


getGroup : Characters -> Character -> Maybe (Nonempty Character)
getGroup chars char =
    Dict.get (chars.shortName char) chars.groupDict


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


unknown : Character
unknown =
    { name = "unknown"
    , bio = ""
    , skills = []
    , category = Original
    , groups = Set.empty
    , price = 0
    , ident = ""
    }
