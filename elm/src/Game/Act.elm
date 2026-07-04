module Game.Act exposing
    ( Act
    , toPathPieces
    , toggles
    )

import Game.Skill as Skill
import Import.Model exposing (Skill)


type alias Act =
    { user : Int
    , button : Int
    , target : Int
    , skill : Skill
    , targets : List Int
    }


toPathPieces : Act -> List Int
toPathPieces { user, button, target } =
    [ user, button, target ]


toggles : Maybe Act -> List Int
toggles x =
    case x of
        Nothing ->
            []

        Just { user, skill, targets } ->
            Skill.targets user skill
                |> List.filter (\target -> List.member target targets)
