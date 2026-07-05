module Game.Skill exposing (affectedSlots, channelDur, isTargeted, targetSlots)

import Game.Game exposing (teamSize)
import Import.Model exposing (Channeling(..), Skill, Target(..))
import List.Extra as List
import Set exposing (Set)


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


teamA : Set Int
teamA =
    Set.fromList <| List.range 0 <| teamSize - 1


teamB : Set Int
teamB =
    Set.fromList <| List.range teamSize <| teamSize + teamSize - 1


allSlots : Set Int
allSlots =
    Set.union teamA teamB


alliesOf : Int -> Set Int
alliesOf user =
    if user < teamSize then
        teamA

    else
        teamB


enemiesOf : Int -> Set Int
enemiesOf user =
    if user < teamSize then
        teamB

    else
        teamA


isSpecific : Target -> Bool
isSpecific target =
    case target of
        Ally ->
            True

        XAlly ->
            True

        Enemy ->
            True

        _ ->
            False


isEnemy : Target -> Bool
isEnemy target =
    case target of
        Enemy ->
            True

        Enemies ->
            True

        REnemy ->
            True

        XEnemies ->
            True

        _ ->
            False


isAlly : Target -> Bool
isAlly target =
    case target of
        Ally ->
            True

        Allies ->
            True

        XAlly ->
            True

        XAllies ->
            True

        RAlly ->
            True

        RXAlly ->
            True

        _ ->
            False


isSelf : Target -> Bool
isSelf target =
    case target of
        Self ->
            True

        Ally ->
            True

        RAlly ->
            True

        _ ->
            False


isTargeted : Skill -> Bool
isTargeted { targets } =
    List.any isSpecific targets


affectedSlots : Int -> Skill -> Set Int
affectedSlots user { targets } =
    if List.member Everyone targets then
        allSlots

    else
        let
            hasAlly =
                List.any isAlly targets

            hasSelf =
                List.any isSelf targets

            hasEnemy =
                List.any isEnemy targets

            base =
                if hasEnemy then
                    enemiesOf user

                else
                    Set.empty
        in
        if hasAlly && hasSelf then
            Set.union base <| alliesOf user

        else if hasSelf then
            Set.insert user base

        else if hasAlly then
            Set.union base <| Set.remove user <| alliesOf user

        else
            base


targetSlots : Int -> Skill -> Set Int
targetSlots user { targets } =
    let
        enemy =
            List.member Enemy targets

        ally =
            List.member Ally targets

        xally =
            List.member XAlly targets
    in
    if enemy && ally then
        allSlots

    else if enemy && xally then
        Set.remove user allSlots

    else if enemy then
        enemiesOf user

    else if ally then
        alliesOf user

    else if xally then
        alliesOf user |> Set.remove user

    else
        Set.singleton user
