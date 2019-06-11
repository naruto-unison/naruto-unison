module Model.User
  ( User(..)
  , Privilege
  , rank, level, xp
  ) where

import Prelude
import Data.Array ((!!))
import Data.Maybe (Maybe, fromMaybe)
import Generic as G
import Data.String as String


newtype User = User { privilege  :: Privilege
                    , name       :: String
                    , avatar     :: String
                    , background :: Maybe String
                    , xp         :: Int
                    , wins       :: Int
                    , losses     :: Int
                    , streak     :: Int
                    , clan       :: Maybe String
                    , condense   :: Boolean
                    }

data Privilege
    = Normal
    | Moderator
    | Admin

level :: User -> Int
level (User u) = u.xp / 1000

xp :: User -> Int
xp (User u) = u.xp `mod` 1000

rank :: User -> String
rank (User u)
  | u.privilege == Normal = fromMaybe "Hokage" $ ranks !! (u.xp / 5000)
  | otherwise             = show u.privilege

ranks :: Array String
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

instance _showPrivilege_ :: Show Privilege where
    show = String.drop 1 <<< String.dropWhile (_ /= period) <<< G.genericShow
      where
        period = String.codePointFromChar '.'

derive instance _200_ :: G.Generic Privilege _
instance _201_ :: G.Decode Privilege where
    decode = G.decodeEnum
derive instance _202_ :: Eq Privilege

derive instance _210_ :: G.Generic User _
instance _211_ :: G.Decode User where
    decode = G.decodeObj
derive instance _213_ :: G.Newtype User _
