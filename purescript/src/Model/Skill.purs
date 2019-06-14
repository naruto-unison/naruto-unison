module Model.Skill
  ( Skill(..)
  , Requirement(..)
  , Copy(..)
  , Copying(..)
  , Target(..)
  , match
  , root, dur
  , targets
  , parseDesc
  ) where

import Prelude
import Data.Array ((:), delete)
import Data.Foldable (elem)
import Data.Function.Memoize (memoize)
import Data.Maybe (fromMaybe)
import Data.String as String
import Data.String (Pattern(..))
import Generic as G
import Halogen.HTML as H
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML.Properties as P

import Model.Untuple (Untuple, untuple)
import Model.Chakra (Chakras)
import Model.Class (Class)
import Model.Channeling (Channeling(..))
import Model.Slot as Slot
import Model.Slot (Slot)
import Util as Util

data Requirement
    = Usable
    | Unusable
    | HasI Int String
    | HasU String

newtype Skill = Skill { name      :: String
                      , desc      :: String
                      , require   :: Requirement
                      , classes   :: Array Class
                      , cost      :: Chakras
                      , cooldown  :: Int
                      , varicd    :: Boolean
                      , charges   :: Int
                      , channel   :: Channeling
                      , start     :: Array (Untuple Target)
                      , effects   :: Array (Untuple Target)
                      , interrupt :: Array (Untuple Target)
                      , copying   :: Copying
                      , pic       :: Boolean
                      }
instance _showSkill_ :: Show Skill where
    show (Skill s) = s.name

data Target
    = Self
    | Ally
    | Allies
    | RAlly
    | XAlly
    | XAllies
    | Enemy
    | Enemies
    | REnemy
    | XEnemies
    | Everyone
    | Specific Slot

newtype Copy = Copy { skill :: Skill
                    , dur   :: Int
                    }

data Copying
    = Shallow Slot Int
    | Deep Slot Int
    | NotCopied

dur :: Skill -> String
dur (Skill s) = case s.channel of
    Instant     -> "Instant"
    Passive     -> "Instant"
    (Action 0)  -> "Action"
    (Control 0) -> "Control"
    (Ongoing 0) -> "Ongoing"
    (Action x)  -> "Action " <> show x
    (Control x) -> "Control " <> show x
    (Ongoing x) -> "Ongoing " <> show x

icon :: Skill -> String
icon (Skill s) = Util.shorten s.name <> if s.pic then "*" else ""

match :: Skill -> Skill -> Boolean
match (Skill x) (Skill y) = x.name == y.name

root :: Skill -> Int -> Int
root (Skill s) slot = case s.copying of
    NotCopied   -> slot
    Shallow a _ -> a
    Deep a    _ -> a

targets :: Int -> Skill -> Array Int
targets = memoize go
  where
    go c (Skill s) = go'
      where
        -- Since PureScript doesn't yet support where-binding across
        -- guard patterns
        go'
          | enemy && ally  = Slot.all
          | enemy && xally = delete c Slot.all
          | enemy          = (_ + 1 - c `mod` 2) <$> Slot.team
          | ally           = (_ + c `mod` 2) <$> Slot.team
          | xally          = delete c $ (_ + c `mod` 2) <$> Slot.team
          | otherwise      = [c]
      -- where
        ts    = untuple <$> (s.start <> s.effects)
        enemy = Enemy `elem` ts
        ally  = Ally  `elem` ts
        xally = XAlly `elem` ts

parseDesc :: ∀ a b. String -> Array (HTML a b)
parseDesc = memoize parseBefore'
  where
    chakraEl x = H.div [P.class_ <<< ClassName $ "chakra " <> x] []
    splitBy p s = fromMaybe { before: s, after: ""} do
        i <- String.indexOf p s
        pure $ String.splitAt i s
    parseBefore' str = parseBefore before (String.drop 1 after)
      where
        {before, after} = splitBy (Pattern "[") str
        parseBefore "" "" = []
        parseBefore "" y  = parseAfter' y
        parseBefore x ""  = [H.text x]
        parseBefore x y   = H.text x : parseAfter' y
        parseAfter' y     = parseAfter before (String.drop 1 after)
          where
            {before, after} = splitBy (Pattern "]") y
        parseAfter "" ""  = []
        parseAfter "" y   = parseAfter' y
        parseAfter x ""  = [H.text x]
        parseAfter "b" y = chakraEl "blood" : parseBefore' y
        parseAfter "g" y = chakraEl "gen"   : parseBefore' y
        parseAfter "n" y = chakraEl "nin"   : parseBefore' y
        parseAfter "t" y = chakraEl "tai"   : parseBefore' y
        parseAfter "r" y = chakraEl "rand"  : parseBefore' y
        parseAfter x y   = H.em_ [H.text x] : parseBefore' y

derive instance _70_ :: G.Generic Copy _
instance _71_ :: G.Decode Copy where
    decode = G.decodeObj
derive instance _72_ :: G.Newtype Copy _

derive instance _80_ :: G.Generic Copying _
instance _81_ :: G.Decode Copying where
    decode = G.decodeObj

derive instance _150_ :: G.Generic Requirement _
instance _151_ :: G.Decode Requirement where
    decode = G.decodeObj
derive instance _152_ :: Eq Requirement

derive instance _160_ :: G.Generic Skill _
instance _161_ :: G.Decode Skill where
    decode = G.decodeObj
derive instance _163_ :: G.Newtype Skill _

derive instance _170_ :: G.Generic Target _
instance _171_ :: G.Decode Target where
    decode = G.decodeObj
derive instance _172_ :: Eq Target
