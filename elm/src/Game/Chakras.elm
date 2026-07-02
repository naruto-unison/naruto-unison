module Game.Chakras exposing
    ( add
    , affordable
    , canExchange
    , lacks
    , negate
    , none
    , rate
    , sub
    , sum
    , toPathPieces
    , total
    )

import Import.Model exposing (Chakras)
import Util exposing (sumBy)


none : Chakras
none =
    { blood = 0, gen = 0, nin = 0, tai = 0, rand = 0 }


add : Chakras -> Chakras -> Chakras
add x y =
    { blood = x.blood + y.blood
    , gen = x.gen + y.gen
    , nin = x.nin + y.nin
    , tai = x.tai + y.tai
    , rand = x.rand + y.rand
    }


sub : Chakras -> Chakras -> Chakras
sub x y =
    { blood = x.blood - y.blood
    , gen = x.gen - y.gen
    , nin = x.nin - y.nin
    , tai = x.tai - y.tai
    , rand = x.rand - y.rand
    }


sum : List Chakras -> Chakras
sum xs =
    { blood = sumBy .blood xs
    , gen = sumBy .gen xs
    , nin = sumBy .nin xs
    , tai = sumBy .tai xs
    , rand = sumBy .rand xs
    }


negate : Chakras -> Chakras
negate { blood, gen, nin, tai, rand } =
    { blood = -blood
    , gen = -gen
    , nin = -nin
    , tai = -tai
    , rand = -rand
    }


rate : Int
rate =
    5


total : Chakras -> Int
total { blood, gen, nin, tai, rand } =
    blood + gen + nin + tai + rand


affordable : Chakras -> Chakras -> Bool
affordable x y =
    let
        afford getter =
            if getter y /= 0 then
                0

            else
                getter x
    in
    rate <= (afford .blood + afford .gen + afford .nin + afford .tai)


lacks : Chakras -> Chakras -> Bool
lacks x { blood, gen, nin, tai, rand } =
    (x.blood < blood)
        || (x.gen < gen)
        || (x.nin < nin)
        || (x.tai < tai)
        || (x.rand < rand)
        || (x.rand < blood + gen + nin + tai + rand)


canExchange : Chakras -> Bool
canExchange x =
    List.any (affordable x)
        [ { none | blood = 1 }
        , { none | gen = 1 }
        , { none | nin = 1 }
        , { none | tai = 1 }
        ]


toPathPieces : Chakras -> List Int
toPathPieces { blood, gen, nin, tai } =
    [ blood, gen, nin, tai ]
