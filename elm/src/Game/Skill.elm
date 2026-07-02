module Game.Skill exposing (channelDur, targets)

import Game.Game exposing (teamSize)
import Import.Model exposing (Channeling(..), Skill, Target(..))
import List.Extra as List


channelDur : Channeling -> Maybe Int
channelDur chan =
    case chan of
        Passive ->
            Nothing

        Instant ->
            Just 1

        Action x ->
            x

        Control x ->
            x

        Ongoing x ->
            x


team : List Int
team =
    List.range 0 <| teamSize - 1


allSlots : List Int
allSlots =
    List.range 0 <| 2 * teamSize - 1


targets : Int -> Skill -> List Int
targets slot { start, effects } =
    let
        possibleTargets =
            start ++ effects

        enemy =
            List.member Enemy possibleTargets

        ally =
            List.member Ally possibleTargets

        xally =
            List.member XAlly possibleTargets

        rem =
            if slot >= teamSize then
                teamSize

            else
                0
    in
    if enemy && ally then
        allSlots

    else if enemy && xally then
        List.remove slot allSlots

    else if enemy then
        List.map ((+) <| teamSize - rem) team

    else if ally then
        List.map ((+) rem) team

    else if xally then
        List.map ((+) rem) team |> List.remove slot

    else
        [ slot ]
