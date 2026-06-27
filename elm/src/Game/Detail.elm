module Game.Detail exposing
    ( Detail
    , allied
    , channel
    , copy
    , get
    )

import Game.Game as Game
import Import.Model exposing (Channel, Channeling(..), Copy, Effect, Ninja, Skill, Status, Trap)
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Set exposing (Set)
import Util exposing (groupBy)


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
            stats
                ++ trapDetails
                |> List.partition (\detail -> detail.user == slot)
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
                >> List.uniqueBy .desc
                >> List.filter .visible
        , trap =
            xxs
                |> List.any .trap
        , amount =
            xxs
                |> List.filter (not << .trap)
                >> List.map .amount
                >> List.sum
    }


unfold : Detail -> List Detail
unfold x =
    if x.amount <= 1 || not (Set.member "Resource" x.classes) then
        [ x ]

    else
        List.repeat x.amount { x | amount = 1 }


channelDur : Channeling -> Maybe Int
channelDur chan =
    case chan of
        Passive ->
            Nothing

        Instant ->
            Just 1

        Action x ->
            x

        Control x ->
            x

        Ongoing x ->
            x


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
            skillBase (channelDur dur) skill
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
    { name = name
    , desc = skill.desc
    , classes = classes
    , dur = dur
    , source = skill.owner
    , skillName = skill.name
    , user = user
    , effects = List.uniqueBy .desc effects
    , trap = False
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
    { name = name
    , desc = skill.desc
    , classes = classes
    , dur = dur
    , source = skill.owner
    , skillName = skill.name
    , user = user
    , effects = [ { trapEffects | desc = trigger } ]
    , trap = True
    , amount = 1
    }
