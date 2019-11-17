module Game.Game exposing
    ( Act
    , died
    , dur
    , forfeit
    , get
    , merge
    , rank
    , removable
    , root
    , targets
    , teamSize
    , toggles
    )

import Dict
import List.Extra as List
import Set

import Import.Flags exposing (Characters)
import Import.Model as Player exposing (Category(..), Channel, Channeling(..), Character, Effect, Ninja, Player(..), Privilege(..), Skill, Target(..), Turn, User)
import Util exposing (elem)


type alias Act =
    { user    : Int
    , button  : Int
    , target  : Int
    , skill   : Skill
    , targets : List Int
    }


died : Player -> Turn -> Turn -> Bool
died player turn1 turn2 =
    living player turn1 > living player turn2


teamSize : Int
teamSize =
    3


team : List Int
team =
    List.range 0 <| teamSize - 1


allSlots : List Int
allSlots =
    List.range 0 <| 2 * teamSize - 1


targets : Int -> Skill -> List Int
targets slot skill =
    let
        possibleTargets =
            skill.start ++ skill.effects

        enemy = Enemy |> elem possibleTargets
        ally  = Ally  |> elem possibleTargets
        xally = XAlly |> elem possibleTargets

        rem   = if slot >= teamSize then teamSize else 0
    in
    if enemy && ally then
        allSlots

    else if enemy && xally then
        List.remove slot allSlots

    else if enemy then
        List.map ((+) <| teamSize - rem) team

    else if ally then
        List.map ((+) rem) team

    else if xally then
        List.map ((+) rem) team |> List.remove slot

    else
        [ slot ]


allied : Player -> Ninja -> Bool
allied player n =
    (n.slot < teamSize) == (player == Player.A)


living : Player -> Turn -> Int
living player game =
    let
        listOp =
            case player of
                Player.A ->
                    List.take

                Player.B ->
                    List.drop
    in
    game.ninjas
        |> listOp teamSize
        >> List.map (.health >> min 1)
        >> List.sum


opponent : Player -> Player
opponent player =
    case player of
        Player.A -> Player.B
        Player.B -> Player.A


forfeit : Player -> Turn -> Turn
forfeit player game =
    let
        forfeitN n = if allied player n then { n | health = 0 } else n
    in
    { game
        | ninjas =
            List.map forfeitN game.ninjas
        , victor =
            [ opponent player ]
    }


unknown : Character
unknown =
    { name     = "unknown"
    , bio      = ""
    , skills   = []
    , category = Original
    , groups   = Set.empty
    , price    = 0
    }


get : List Character -> Int -> Character
get xs slot =
    xs
    |> List.getAt slot
    >> Maybe.withDefault unknown


dur : Channel -> Int
dur chan =
    case chan.dur of
        Passive   -> 0
        Instant   -> 1
        Action  x -> x
        Control x -> x
        Ongoing x -> x


removable : Bool -> Effect -> Bool
removable onAlly ef =
    not ef.sticky && onAlly /= ef.helpful


root : List Character -> Skill -> Character
root characters skill =
    get characters skill.owner


toggles : Maybe Act -> List Int
toggles x =
    case x of
        Nothing ->
            []

        Just y ->
            targets y.user y.skill
                |> List.filter (elem y.targets)


rank : User -> String
rank user =
    case user.privilege of
        Guest ->
            "Guest"

        Normal ->
            ranks
                |> List.getAt (user.xp // 5000)
                >> Maybe.withDefault "Hokage"

        Moderator ->
            "Moderator"

        Admin ->
            "Admin"


ranks : List String
ranks =
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


merge : Characters -> Ninja -> Character
merge chars n =
    let
        char =
            Dict.get n.character chars.dict
                |> Maybe.withDefault unknown

        zip cSkills nSkill =
            cSkills
                |> List.map
                    (\cSkill ->
                        if cSkill.name == nSkill.name then nSkill else cSkill
                    )
    in
    { char | skills = List.map2 zip char.skills n.skills }
