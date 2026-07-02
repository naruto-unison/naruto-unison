module Game.Characters exposing
    ( Characters
    , create
    , get
    , getGroup
    , merge
    , root
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Import.Model exposing (Category(..), Character, Ninja, Skill)
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty)
import Set
import Util exposing (buildDict, groupBy)


type alias Characters =
    { list : List Character
    , dict : Dict String Character
    , size : Int
    , groupList : List (Nonempty Character)
    , groupDict : Dict String (Nonempty Character)
    , groupSize : Int
    , shortName : Character -> String
    }


get : Array Character -> Int -> Character
get xs slot =
    xs
        |> Array.get slot
        >> Maybe.withDefault unknown


root : Array Character -> Skill -> Character
root characters { owner } =
    get characters owner


mergeSkill : Skill -> List Skill -> List Skill
mergeSkill ({ name, owner } as skill) =
    List.map <|
        \otherSkill ->
            if otherSkill.name == name then
                skill

            else
                { otherSkill | owner = owner }


merge : Characters -> Ninja -> Character
merge chars { character, skills } =
    let
        char =
            Dict.get character chars.dict
                |> Maybe.withDefault unknown
    in
    { char | skills = List.map2 mergeSkill skills char.skills }


create : List Character -> Characters
create chars =
    let
        idents =
            buildDict ( .ident, makeShortName ) chars

        shortName char =
            case Dict.get char.ident idents of
                Just name ->
                    name

                Nothing ->
                    makeShortName char

        groupList =
            chars |> groupBy (\x y -> shortName x == shortName y)
    in
    { list = chars
    , dict = buildDict ( .ident, identity ) chars
    , size = List.length chars
    , groupList = groupList
    , groupDict = buildDict ( Nonempty.head >> shortName, identity ) groupList
    , groupSize = List.length groupList
    , shortName = shortName
    }


getGroup : Characters -> Character -> Maybe (Nonempty Character)
getGroup chars char =
    Dict.get (chars.shortName char) chars.groupDict


makeShortName : Character -> String
makeShortName { name, skills } =
    case name of
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
            skills
                |> List.getAt 3
                >> Maybe.andThen List.head
                >> Maybe.andThen shortFromInvuln
                >> Maybe.withDefault name


shortFromInvuln : Skill -> Maybe String
shortFromInvuln { desc } =
    case String.words desc of
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
