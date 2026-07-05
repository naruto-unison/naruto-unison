module Game.Act exposing
    ( Act
    , toPathPieces
    , toggles
    )

import Game.Skill as Skill
import Import.Model exposing (Skill)
import Set exposing (Set)


type alias Act =
    { user : Int
    , button : Int
    , target : Int
    , skill : Skill
    , targets : Set Int
    }


toPathPieces : Act -> List Int
toPathPieces { user, button, target } =
    [ user, button, target ]


toggles : Maybe Act -> Set Int
toggles x =
    case x of
        Nothing ->
            Set.empty

        Just { user, skill, targets } ->
            Skill.targetSlots user skill
                |> Set.intersect targets
