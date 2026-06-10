module Application.Model.User (Privilege(..), User(..), UserId, new) where

import ClassyPrelude

import Application.Model.Internal (Privilege(..), User(..), UserId)

new :: Text -> Maybe Text -> Day -> User
new ident verkey day = User
    { ident      = ident
    , password   = Nothing
    , verkey     = verkey
    , verified   = False
    , joined     = day
    , privilege  = Normal
    , name       = ident
    , avatar     = "/img/icon/default.jpg"
    , background = Nothing
    , xp         = 0
    , wins       = 0
    , losses     = 0
    , streak     = 0
    , record     = 0
    , latestWin  = Nothing
    , latestGame = Nothing
    , clan       = Nothing
    , team       = Nothing
    , practice   = ["Naruto Uzumaki", "Sakura Haruno", "Sasuke Uchiha"]
    , condense   = False
    , rating     = 0.0
    , deviation  = 350.0 / 173.7178
    , volatility = 0.06
    , dna        = 0
    }
