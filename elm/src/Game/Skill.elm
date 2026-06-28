module Game.Skill exposing (targets)

import Game.Game exposing (teamSize)
import Import.Model exposing (Skill, Target(..))
import List.Extra as List


team : List Int
team =
    List.range 0 <| teamSize - 1


allSlots : List Int
allSlots =
    List.range 0 <| 2 * teamSize - 1


targets : Int -> Skill -> List Int
targets slot skill =
    let
        possibleTargets =
            skill.start ++ skill.effects

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
