module Game.Detail exposing
    ( Detail
    , allied
    , channel
    , copy
    , get
    )

import Game.Game as Game
import Game.Skill as Skill
import Import.Model exposing (Channel, Channeling(..), Copy, Effect, Ninja, Skill, Status, Trap)
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Set exposing (Set)
import Util exposing (groupBy, sumBy)


type alias Detail =
    { name : String
    , desc : String
    , classes : Set String
    , dur : Maybe Int
    , source : Int
    , skillName : String
    , user : Int
    , effects : List Effect
    , trap : Bool
    , amount : Int
    }


get : Ninja -> List Detail
get { slot, statuses, traps } =
    let
        statusDetails =
            List.map status statuses

        reduce ((Nonempty x xs) as xxs) =
            case List.find (eq x) statusDetails of
                Just y ->
                    Nonempty y <| x :: xs

                Nothing ->
                    xxs

        trapDetails =
            traps
                |> List.map trap
                >> groupBy eq
                >> List.map (reduce >> concat)

        stats =
            statusDetails
                |> List.filter (\x -> not <| List.any (eq x) trapDetails)
                >> List.concatMap unfold

        ( self, others ) =
            (stats ++ trapDetails)
                |> List.partition (\{ user } -> user == slot)
    in
    others ++ self


eq : Detail -> Detail -> Bool
eq x y =
    (x.dur == y.dur)
        && (x.name == y.name)
        && (ignoreClasses x.classes == ignoreClasses y.classes)


ignoredClasses : Set String
ignoredClasses =
    Set.fromList
        [ "Bypassing"
        , "Non-stacking"
        , "Soulbound"
        , "Uncounterable"
        , "Unreflectable"
        , "Unremovable"
        , "Necromancy"
        , "Atemporal"
        ]


ignoreClasses : Set String -> Set String
ignoreClasses xs =
    Set.diff xs ignoredClasses


allied : Int -> Detail -> Bool
allied user x =
    (user < Game.teamSize) == (x.user < Game.teamSize)


concat : Nonempty Detail -> Detail
concat (Nonempty x xs) =
    let
        xxs =
            x :: xs
    in
    { x
        | effects =
            xxs
                |> List.concatMap .effects
                >> List.filter .visible
                >> List.uniqueBy .desc
        , trap =
            xxs
                |> List.any .trap
        , amount =
            xxs
                |> List.filter (not << .trap)
                >> sumBy .amount
    }


unfold : Detail -> List Detail
unfold ({ amount, classes } as detail) =
    if amount <= 1 || not (Set.member "Resource" classes) then
        [ detail ]

    else
        List.repeat amount { detail | amount = 1 }


skillBase : Maybe Int -> Skill -> Detail
skillBase dur { classes, desc, name, owner } =
    { name = name
    , desc = desc
    , classes = classes
    , dur = dur
    , source = owner
    , skillName = name
    , user = owner
    , effects = []
    , trap = False
    , amount = 1
    }


channel : Int -> Channel -> Detail
channel user { dur, skill } =
    let
        base =
            skillBase (Skill.channelDur dur) skill
    in
    { base
        | user = user
        , trap =
            case dur of
                Control _ ->
                    True

                _ ->
                    False
    }


copy : Copy -> Detail
copy { dur, skill } =
    skillBase dur skill


status : Status -> Detail
status { amount, classes, dur, effects, name, skill, user } =
    let
        base =
            skillBase dur skill
    in
    { base
        | name = name
        , classes = classes
        , user = user
        , effects = List.uniqueBy .desc effects
        , amount = amount
    }


trapEffects : Effect
trapEffects =
    { desc = ""
    , helpful = False
    , sticky = True
    , trap = True
    , visible = True
    , slot = Nothing
    }


trap : Trap -> Detail
trap { classes, dur, name, skill, trigger, user } =
    let
        base =
            skillBase dur skill
    in
    { base
        | name = name
        , classes = classes
        , user = user
        , effects = [ { trapEffects | desc = trigger } ]
        , trap = True
    }
