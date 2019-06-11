module Action.Chakra
  ( absorb
  , deplete
  , gain
  , kabuto
  ) where

import ClassyPrelude.Yesod

import qualified Class.Play as P
import           Class.Play (PlayT)
import           Class.Random (RandomT)
import qualified Model.Chakra as Chakra
import           Model.Chakra (Chakra(..))
import           Model.Effect (Effect(..))
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Engine.Chakras as Chakras

-- ** CHAKRA

gain :: ∀ m. (PlayT m, RandomT m) => [Chakra] -> m ()
gain [] = return ()
gain chakras = do
    target   <- P.target
    rand     <- replicateM (length rands) Chakras.random
    let total = Chakra.collect $ rand ++ nonrands
    P.modify $ Game.adjustChakra target (+ total)
    return ()
  where
    (rands, nonrands) = partition (== Rand) chakras

deplete :: ∀ m. (PlayT m, RandomT m) => Int -> m ()
deplete amount = unlessM (Ninja.is Enrage <$> P.nTarget) .
    void $ Chakras.remove amount

absorb :: ∀ m. (PlayT m, RandomT m) => Int -> m ()
absorb amount = unlessM (Ninja.is Enrage <$> P.nTarget) do
    user    <- P.user
    chakras <- Chakras.remove amount
    P.modify $ Game.adjustChakra user (+ chakras)

kabuto :: ∀ m. PlayT m => m ()
kabuto = do
    skill  <- P.skill
    target <- P.target
    P.modify . Game.adjust target $ Ninja.kabuto skill
