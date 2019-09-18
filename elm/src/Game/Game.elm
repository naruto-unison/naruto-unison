module Game.Game exposing
  ( Act
  , skillSize, teamSize
  , died
  , dur
  , forfeit
  , get
  , merge
  , rank
  , removable
  , source
  , root
  , targets
  , toggles
  )

import Dict
import List.Extra as List
import Tuple exposing (first, second)

import Import.Flags exposing (Characters)
import Import.Model as Player exposing (Category(..), Chakras, Channel, Channeling(..), Character, Copying(..), Effect, Game, Ninja, Player(..), Privilege(..), Skill, Target(..), User)
import Util exposing (elem)

type alias Act =
    { user    : Int
    , button  : Int
    , target  : Int
    , skill   : Skill
    , targets : List Int
    }

died : Player -> Game -> Game -> Bool
died player oldGame newGame = living player oldGame > living player newGame

skillSize : Int
skillSize = 4

teamSize : Int
teamSize = 3

team : List Int
team = List.range 0 <| teamSize - 1

allSlots : List Int
allSlots = List.range 0 <| 2 * teamSize - 1

targets : Int -> Skill -> List Int
targets slot skill =
  let
    possibleTargets = skill.start ++ skill.effects
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
        List.remove slot <| List.map ((+) rem) team
    else
        [slot]

allied : Player -> Ninja -> Bool
allied player n = (n.slot < teamSize) == (player == Player.A)

living : Player -> Game -> Int
living player game = List.sum << List.map (min 1 << .health) <| case player of
    Player.A -> List.take teamSize game.ninjas
    Player.B -> List.drop teamSize game.ninjas


opponent : Player -> Player
opponent player = case player of
    Player.A -> Player.B
    Player.B -> Player.A

forfeit : Player -> Game -> Game
forfeit player game =
  let
    forfeitN n = if allied player n then { n | health = 0 } else n
  in
    { game
    | ninjas = List.map forfeitN game.ninjas
    , victor = [opponent player]
    }

unknown : Character
unknown = { name = "unknown", bio = "", skills = [], category = Original }

get : List Character -> Int -> Character
get xs slot = Maybe.withDefault unknown <| List.getAt slot xs

dur : Channel -> Int
dur chan = case chan.dur of
    Passive   -> 0
    Instant   -> 1
    Action x  -> x
    Control x -> x
    Ongoing x -> x

removable : Bool -> Effect -> Bool
removable onAlly ef = not ef.sticky && onAlly /= ef.helpful

source : Skill -> Int -> Int
source skill slot = case skill.copying of
    NotCopied   -> slot
    Shallow x _ -> x
    Deep    x _ -> x

root : List Character -> Skill -> Int -> Character
root characters skill slot = get characters <| source skill slot

toggles : Maybe Act -> List Int
toggles x = case x of
    Nothing -> []
    Just y  -> List.filter (elem y.targets) <| targets y.user y.skill

rank : User -> String
rank user = case user.privilege of
    Normal -> Maybe.withDefault "Hokage" <| List.getAt (user.xp // 5000) ranks
    Moderator -> "Moderator"
    Admin     -> "Admin"

ranks : List String
ranks = [ "Academy Student"
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
    char = Maybe.withDefault unknown <| Dict.get n.character chars.dict
    zip cSkills nSkill = cSkills |> List.map (\cSkill ->
        if cSkill.name == nSkill.name then nSkill else cSkill)
  in
    { char | skills = List.map2 zip char.skills n.skills }
