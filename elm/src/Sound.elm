module Sound exposing (Sound(..), enum, show)


type Sound
    = ApplySkill
    | Cancel
    | Click
    | Death
    | Lose
    | NextTurn
    | Scroll
    | StartFirst
    | StartTurn
    | StartSecond
    | Target
    | Win


enum : List Sound
enum =
    [ ApplySkill
    , Cancel
    , Click
    , Death
    , Lose
    , NextTurn
    , Scroll
    , StartFirst
    , StartTurn
    , StartSecond
    , Target
    , Win
    ]


show : Sound -> String
show x =
    case x of
        ApplySkill ->
            "ApplySkill"

        Cancel ->
            "Cancel"

        Click ->
            "Click"

        Death ->
            "Death"

        Lose ->
            "Lose"

        NextTurn ->
            "NextTurn"

        Scroll ->
            "Scroll"

        StartFirst ->
            "StartFirst"

        StartTurn ->
            "StartTurn"

        StartSecond ->
            "StartSecond"

        Target ->
            "Target"

        Win ->
            "Win"
