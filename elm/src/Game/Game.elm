module Game.Game exposing
    ( died
    , forfeit
    , split
    , teamSize
    )

import Import.Model as Player exposing (Ninja, Player, Turn)
import List.Extra as List


teamSize : Int
teamSize =
    3


allied : Player -> Ninja -> Bool
allied player n =
    (n.slot < teamSize) == (player == Player.A)


split : Player -> List a -> { allies : List a, enemies : List a }
split player xs =
    let
        ( left, right ) =
            List.splitAt teamSize xs
    in
    case player of
        Player.A ->
            { allies = left, enemies = right }

        Player.B ->
            { allies = right, enemies = left }


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


died : Player -> Turn -> Turn -> Bool
died player turn1 turn2 =
    living player turn1 > living player turn2


forfeit : Player -> Turn -> Turn
forfeit player game =
    let
        forfeitN n =
            if allied player n then
                { n | health = 0 }

            else
                n

        opponent =
            case player of
                Player.A ->
                    Player.B

                Player.B ->
                    Player.A
    in
    { game
        | ninjas = List.map forfeitN game.ninjas
        , victor = [ opponent ]
    }
