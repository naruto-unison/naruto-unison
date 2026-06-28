module Game.Effect exposing (removable)

import Import.Model exposing (Effect)


removable : Bool -> Effect -> Bool
removable onAlly ef =
    not ef.sticky && onAlly /= ef.helpful
