-- | Actions that characters can use to manipulate 'Chakra'.
module Game.Action.Chakra
  ( absorb
  , deplete, deplete1
  , gain
  ) where

import ClassyPrelude

import Data.Enum.Set (EnumSet)

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Game.Engine.Chakra as Chakra
import           Game.Model.Chakras (Chakra(..), Chakras)
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import qualified Game.Model.Game as Game

-- ** CHAKRA
-- | Adds a finite amount of @Chakra@ to the 'Game.chakra' of the target's team.
-- 'Rand's are replaced by other @Chakra@ types selected by 'Chakras.random'.
gain :: ∀ m. (MonadPlay m, MonadRandom m) => Chakras -> m ()
gain chakras = P.unsilenced $ Chakra.gain chakras

-- | Removes some number of @Chakra@s from the 'Game.chakra' of the target's
-- team. 'Chakra's are selected at random by 'Chakra.remove'.
deplete :: ∀ m. (MonadPlay m, MonadRandom m) => Int -> m ()
deplete amount = P.unsilenced . void $ Chakra.remove amount

-- | Removes a single 'Chakra' from the enemy team that is one of several types.
-- 'Chakra's are chosen randomly from the available pool of 'Game.chakra', but
-- only the ones passed in the parameter.
deplete1 :: ∀ m. (MonadPlay m, MonadRandom m) => EnumSet Chakra -> m ()
deplete1 chakras = P.unsilenced . void $ Chakra.remove1 chakras

-- | Transfers some number of @Chakra@s from the 'Game.chakra' of the target's
-- team to the 'Game.chakra' of the user's team. @Chakra@s are selected at
-- random by 'Chakra.remove'.
absorb :: ∀ m. (MonadPlay m, MonadRandom m) => Int -> m ()
absorb amount = P.unsilenced do
    Context{user} <- P.context
    chakras <- Chakra.remove amount
    P.alter $ Game.addChakra user chakras
