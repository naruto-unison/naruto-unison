module Game.Act exposing
    ( Act
    , targetSlots
    , toPathPieces
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


targetSlots : Act -> Set Int
targetSlots { user, skill, targets } =
    Skill.targetSlots user skill
        |> Set.intersect targets
