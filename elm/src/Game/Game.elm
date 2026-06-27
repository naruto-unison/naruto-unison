module Game.Game exposing
    ( Act
    , died
    , forfeit
    , removable
    , targeted
    , targets
    , teamSize
    , toggles
    , warInverse
    )

import Import.Model as Player exposing (Channeling(..), Effect, Ninja, Player(..), Skill, Target(..), Turn, War(..))
import List.Extra as List


type alias Act =
    { user : Int
    , button : Int
    , target : Int
    , skill : Skill
    , targets : List Int
    }


targeted : Int -> Act -> Act
targeted target act =
    { act | target = target }


died : Player -> Turn -> Turn -> Bool
died player turn1 turn2 =
    living player turn1 > living player turn2


teamSize : Int
teamSize =
    3


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


allied : Player -> Ninja -> Bool
allied player n =
    (n.slot < teamSize) == (player == Player.A)


living : Player -> Turn -> Int
living player game =
    let
        listOp =
            case player of
                Player.A ->
                    List.take

                Player.B ->
                    List.drop
    in
    game.ninjas
        |> listOp teamSize
        >> List.map (.health >> min 1)
        >> List.sum


opponent : Player -> Player
opponent player =
    case player of
        Player.A ->
            Player.B

        Player.B ->
            Player.A


warInverse : War -> War
warInverse war =
    case war of
        Red ->
            Blue

        Blue ->
            Red


forfeit : Player -> Turn -> Turn
forfeit player game =
    let
        forfeitN n =
            if allied player n then
                { n | health = 0 }

            else
                n
    in
    { game
        | ninjas = List.map forfeitN game.ninjas
        , victor = [ opponent player ]
    }


removable : Bool -> Effect -> Bool
removable onAlly ef =
    not ef.sticky && onAlly /= ef.helpful


toggles : Maybe Act -> List Int
toggles x =
    case x of
        Nothing ->
            []

        Just y ->
            targets y.user y.skill
                |> List.filter (\target -> List.member target y.targets)
