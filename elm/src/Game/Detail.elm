module Game.Detail exposing
  ( Detail
  , allied
  , get
  )

import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))

import Game.Game as Game
import Import.Model exposing (Channel, Effect, Ninja, Status, Trap)
import Util exposing (elem, groupBy)

type alias Detail =
    { name    : String
    , desc    : String
    , classes : List String
    , dur     : Int
    , source  : Int
    , effects : List Effect
    , trap    : Bool
    , ghost   : Bool
    , amount  : Int
    }

get : Ninja -> List Detail
get n =
  let
    statuses = List.map status n.statuses
    reduce (Nonempty x xs as xxs) = case List.find (eq x) statuses of
        Just y  -> Nonempty y <| x :: xs
        Nothing -> xxs
    traps = List.map (concat << reduce) << groupBy eq <| List.map trap n.traps
    stats = List.concatMap unfold <|
            List.filter (\x -> not <| List.any (eq x) traps) statuses
    (self, others) = List.partition ((==) n.slot << .source) <| stats ++ traps
  in
    self ++ others

eq : Detail -> Detail -> Bool
eq x y = x.desc == y.desc && x.classes == y.classes && x.dur == y.dur

allied : Int -> Detail -> Bool
allied user x = (user + x.source |> remainderBy 2) == 0

concat : Nonempty Detail -> Detail
concat (Nonempty x xs) =
  let
    xxs = x :: xs
  in
    { x
    | effects = List.uniqueBy .desc <| List.concatMap .effects xxs
    , trap    = List.any .trap xxs
    , ghost   = List.all .ghost xxs
    , amount  = List.sum <| List.map .amount xxs
    }

unfold : Detail -> List Detail
unfold x =
  if x.amount <= 1 || not ("Resource" |> elem x.classes) then
      [x]
  else
      List.repeat x.amount { x | amount = 1 }

channel : Channel -> Detail
channel x =
    { name    = x.skill.name
    , desc    = x.skill.desc
    , classes = x.skill.classes
    , dur     = Game.dur x
    , source  = x.source
    , effects = []
    , trap    = False
    , ghost   = False
    , amount  = 1
    }

status : Status -> Detail
status x =
    { name    = x.name
    , desc    = x.skill.desc
    , classes = x.classes
    , dur     = x.dur
    , source  = x.source
    , effects = x.effects
    , trap    = False
    , ghost   = False
    , amount  = x.amount
    }

trap : Trap -> Detail
trap x =
  let
    effects =
        { desc    = x.trigger
        , helpful = False
        , sticky  = True
        , trap    = True
        }
  in
    { name    = x.name
    , desc    = x.desc
    , classes = x.classes
    , dur     = x.dur
    , source  = x.user
    , effects = [effects]
    , trap    = True
    , ghost   = False
    , amount  = 1
    }
