module Game.Chakra exposing
    ( affordable
    , canExchange
    , lacks
    , negate
    , none
    , rate
    , sum
    , total
    )

import Import.Model exposing (Chakras)


sum : List Chakras -> Chakras
sum xs =
    { blood = List.map .blood xs |> List.sum
    , gen   = List.map .gen   xs |> List.sum
    , nin   = List.map .nin   xs |> List.sum
    , tai   = List.map .tai   xs |> List.sum
    , rand  = List.map .rand  xs |> List.sum
    }


none : Chakras
none =
    { blood = 0, gen = 0, nin = 0, tai = 0, rand = 0 }


negate : Chakras -> Chakras
negate x =
    { blood = -x.blood
    , gen   = -x.gen
    , nin   = -x.nin
    , tai   = -x.tai
    , rand  = -x.rand
    }


rate : Int
rate =
    5


total : Chakras -> Int
total x =
    x.blood + x.gen + x.nin + x.tai + x.rand


affordable : Chakras -> Chakras -> Bool
affordable x y =
    rate <= total { blood = if y.blood /= 0 then 0 else x.blood
                  , gen   = if y.gen   /= 0 then 0 else x.gen
                  , nin   = if y.nin   /= 0 then 0 else x.nin
                  , tai   = if y.tai   /= 0 then 0 else x.tai
                  , rand  = 0
                  }


lacks : Chakras -> Chakras -> Bool
lacks x y =
    x.blood   < y.blood
    || x.gen  < y.gen
    || x.nin  < y.nin
    || x.tai  < y.tai
    || x.rand < y.rand
    || x.rand < total y


canExchange : Chakras -> Bool
canExchange x =
    List.any (affordable x)
        [ { none | blood = 1 }
        , { none | gen   = 1 }
        , { none | nin   = 1 }
        , { none | tai   = 1 }
        ]
