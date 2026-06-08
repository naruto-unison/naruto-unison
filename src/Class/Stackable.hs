module Class.Stackable
    ( Stackable(..)
    , (.:), (.++)
    ) where
import ClassyPrelude hiding (deleteBy)

import Data.List (deleteBy)

import           Class.Classed (Classed)
import qualified Class.Classed as Classed
import           Game.Model.Class (Class(..))
import           Game.Model.Internal (Destructible(Destructible), Skill(Skill), Status(Status))
import qualified Game.Model.Internal
import qualified Game.Model.Internal.Destructible as Destructible
import qualified Game.Model.Internal.Status as Status
import           Util ((∈), (∉))

class Classed a => Stackable a where
    getAmount :: a -> Int
    setAmount :: Int -> a -> a
    stackable :: a -> a -> Bool
    unstack   :: a -> a -> Bool

instance Stackable Destructible where
    getAmount Destructible{amount} = amount
    setAmount amount x = x { Destructible.amount = amount }
    stackable = (==) `on` project
      where
        project Destructible
            { user
            , skill = Skill{classes, name, owner}
            , dur
            , effects
            } = (user, owner, dur, classes, name, effects)
    unstack = (==) `on` project
      where
        project Destructible
            { user
            , skill = Skill{name, owner}
            } = (user, name, owner)

instance Stackable Status where
    getAmount Status{amount} = amount
    setAmount amount x = x { Status.amount = amount }
    stackable a@Status{bombs = []} b@Status{bombs = []} = project a == project b
      where
        project Status
            { name
            , user
            , skill = Skill{name = skillName, owner}
            , effects
            , classes
            , maxDur
            , dur
            } = (user, owner, dur, maxDur, classes, name, skillName, effects)
    stackable _ _ = False
    unstack = (==) `on` project
      where
        project Status
            { name
            , user
            , skill = Skill{name = skillName, owner}
            } = (user, owner, name, skillName)

isNonStack :: ∀ a. Classed a => a -> Bool
isNonStack x = Nonstacking ∈ Classed.classes x && Hidden ∉ Classed.classes x

addNonStacking :: ∀ a. Stackable a => a -> [a] -> [a]
addNonStacking x xs = x : filter f xs
  where
    f y = not $ isNonStack y && unstack x y

addStacking :: ∀ a. Stackable a => a -> [a] -> [a]
addStacking x xs = case find (stackable x) xs of
    Nothing -> x :< xs
    Just y  -> setAmount (getAmount y + getAmount x) x
               :< deleteBy stackable x xs

(.:) :: ∀ a. Stackable a => a -> [a] -> [a]
x .: xs
  | isNonStack x = addNonStacking x xs
  | otherwise    = addStacking x xs
infixr 5 .:

(.++) :: ∀ a. Stackable a => [a] -> [a] -> [a]
xs .++ ys = foldr (.:) ys xs
infixr 5 .++
