-- | Actions that characters can use to manipulate 'Chakra'.
module Action.Chakra
  ( absorb
  , deplete
  , gain
  , kabuto
  ) where

import ClassyPrelude

import qualified Class.Play as P
import           Class.Play (MonadPlay)
import qualified Class.Random as R
import           Class.Random (MonadRandom)
import qualified Model.Chakra as Chakra
import           Model.Chakra (Chakra(..))
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Engine.Chakras as Chakras

-- ** CHAKRA

-- | Adds 'Chakra's to the 'Game.chakra' of the target's team.
-- 'Rand's are replaced by other Chakra types selected by 'Chakras.random'.
gain :: ∀ m. (MonadPlay m, MonadRandom m) => [Chakra] -> m ()
gain [] = return ()
gain chakras = P.unsilenced do
    target   <- P.target
    rand     <- replicateM (length rands) R.enum
    let total = Chakra.collect $ rand ++ nonrands
    P.alter $ Game.adjustChakra target (+ total)
  where
    (rands, nonrands) = partition (== Rand) chakras

-- | Removes some number of 'Chakra's from the 'Game.chakra' of the target's
-- team. 'Chakra's are selected at random by 'Chakras.remove'.
deplete :: ∀ m. (MonadPlay m, MonadRandom m) => Int -> m ()
deplete amount = P.unsilenced . void $ Chakras.remove amount

-- | Transfers some number of 'Chakra's from the 'Game.chakra' of the target's
-- team to the 'Game.chakra' of the user's team. 'Chakra's are selected at
-- random by 'Chakras.remove'.
absorb :: ∀ m. (MonadPlay m, MonadRandom m) => Int -> m ()
absorb amount = P.unsilenced do
    user    <- P.user
    chakras <- Chakras.remove amount
    P.alter $ Game.adjustChakra user (+ chakras)

-- | Cycles Kabuto Yakushi's chakra mode through the four types of 'Chakra'.
-- Uses 'Ninja.kabuto' internally.
kabuto :: ∀ m. MonadPlay m => m ()
kabuto = do
    skill  <- P.skill
    target <- P.target
    P.modify target $ Ninja.kabuto skill
