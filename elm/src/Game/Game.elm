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
  , toggles
  )

import List.Extra as List
import Tuple exposing (first, second)

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

allied : Player -> Ninja -> Bool
allied player n = ((n.slot |> remainderBy 2) == 0) == (player == Player.A)

living : Player -> Game -> Int
living player game =
  let
    health n = if allied player n then min 1 n.health else 0
  in
    List.sum <| List.map health game.ninjas

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

root : List Character -> Skill -> Int -> Character
root characters skill slot = get characters <| case skill.copying of
    NotCopied   -> slot
    Shallow x _ -> x
    Deep x _    -> x

targets : Int -> Skill -> List Int
targets slot skill =
  let
    possibleTargets = skill.start ++ skill.effects
    enemy = Enemy |> elem possibleTargets
    ally  = Ally  |> elem possibleTargets
    xally = XAlly |> elem possibleTargets
  in
    if enemy && ally then
        List.range 0 5
    else if enemy && xally then
        List.remove slot <| List.range 0 5
    else if enemy then
        List.map ((+) <| 1 - remainderBy 2 slot) [0, 2, 4]
    else if ally then
        List.map ((+) <| remainderBy 2 slot) [0, 2, 4]
    else if xally then
        List.remove slot <| List.map ((+) <| remainderBy 2 slot) [0, 2, 4]
    else
        [slot]

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

merge : Character -> Ninja -> Character
merge char n =
  let
    zip cSkills nSkill = cSkills |> List.map (\cSkill ->
        if cSkill.name == nSkill.name then nSkill else cSkill)
    in
      { char | skills = List.map2 zip char.skills n.skills }
