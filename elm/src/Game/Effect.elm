module Game.Effect exposing (removable)

import Import.Model exposing (Effect)


removable : Bool -> Effect -> Bool
removable onAlly { sticky, helpful } =
    not sticky && onAlly /= helpful
