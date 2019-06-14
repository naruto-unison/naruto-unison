module Sound exposing (Sound(..), showSound)

type Sound
    = SFXApplySkill
    | SFXCancel
    | SFXClick
    | SFXDeath
    | SFXLose
    | SFXNextTurn
    | SFXScroll
    | SFXStartFirst
    | SFXStartTurn
    | SFXStartSecond
    | SFXTarget
    | SFXWin

showSound : Sound -> String
showSound x = case x of
    SFXApplySkill  -> "SFXApplySkill"
    SFXCancel      -> "SFXCancel"
    SFXClick       -> "SFXClick"
    SFXDeath       -> "SFXDeath"
    SFXLose        -> "SFXLose"
    SFXNextTurn    -> "SFXNextTurn"
    SFXScroll      -> "SFXScroll"
    SFXStartFirst  -> "SFXStartFirst"
    SFXStartTurn   -> "SFXStartTurn"
    SFXStartSecond -> "SFXStartSecond"
    SFXTarget      -> "SFXTarget"
    SFXWin         -> "SFXWin"
