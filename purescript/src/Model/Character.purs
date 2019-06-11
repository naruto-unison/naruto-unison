module Model.Character
  ( Character(..)
  , Category(..)
  , get
  , mergeSkills
  , icon, name
  ) where

import Prelude
import Data.Array ((!!), zipWith)
import Data.Function.Memoize (memoize)
import Data.Maybe (fromMaybe)
import Data.String as String
import Generic as G
import Halogen.HTML as H
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties (IProp)

import Model (Ninja(..))
import Model.Skill as Skill
import Model.Skill (Skill)
import Util as Util

data Category
    = Original
    | Shippuden
    | Reanimated

newtype Character = Character { name     :: String
                              , bio      :: String
                              , skills   :: Array (Array Skill)
                              , category :: Category
                              }

mergeSkills :: Character -> Ninja -> Character
mergeSkills (Character c) (Ninja n) =
    Character c { skills = zipWith f c.skills n.skills }
  where
    f cSkills skill = f' <$> cSkills
      where f' cSkill | Skill.match cSkill skill = skill
                      | otherwise                = cSkill

unknown :: Character
unknown = Character { name:     "unknown"
                    , bio:      ""
                    , skills:   []
                    , category: Original
                    }

get :: Array Character -> Int -> Character
get xs slot = fromMaybe unknown $ xs !! slot

name :: ∀ a b. Character -> Array (HTML a b)
name (Character c) = case c.category of
    Original   -> [H.text c.name]
    Shippuden  -> [H.text c.name, _minor "𝕊"]
    Reanimated -> [H.text c.name, _minor "ℝ"]
  where
    _minor x = H.span [P.class_ $ ClassName "minor"] $ [H.text x]

icon :: ∀ a b. Character -> (String -> IProp (src :: String | b) a)
icon = memoize go <<< show
  where
    go n x  = P.src $
              "/img/ninja/" <> Util.shorten n <> "/" <> shorter x <> ".jpg"
    shorter = Util.shorten <<<
              String.takeWhile (_ /= String.codePointFromChar '(')

instance _showCharacter_ :: Show Character where
    show (Character c) = case c.category of
        Original   -> c.name
        Shippuden  -> c.name <> " (S)"
        Reanimated -> c.name <> " (R)"
instance _eqCharacter_ :: Eq Character where
    eq (Character x) (Character y) = x.name == y.name
                                     && x.category == y.category
instance _ordCharacter_ :: Ord Character where
    compare = comparing show

derive instance _60_ :: G.Generic Character _
instance _61_ :: G.Decode Character where
    decode = G.decodeObj
derive instance _62_ :: G.Newtype Character _

derive instance _270_ :: G.Generic Category _
instance _271_ :: G.Decode Category where
    decode = G.decodeEnum
derive instance _272_ :: Eq Category
