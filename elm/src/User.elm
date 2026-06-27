module User exposing
    ( level
    , rank
    )

import Array exposing (Array)
import Import.Model exposing (Privilege(..), User)


rank : User -> String
rank { privilege, xp } =
    case privilege of
        Guest ->
            "Guest"

        Normal ->
            ranks
                |> Array.get (xp // 5000)
                >> Maybe.withDefault "Hokage"

        Moderator ->
            "Moderator"

        Admin ->
            "Admin"


level : User -> Int
level { xp } =
    xp // 1000


ranks : Array String
ranks =
    Array.fromList
        [ "Academy Student"
        , "Genin"
        , "Chūnin"
        , "Missing-Nin"
        , "Anbu"
        , "Jōnin"
        , "Sannin"
        , "Jinchūriki"
        , "Akatsuki"
        , "Kage"
        , "Hokage"
        ]
