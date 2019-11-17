module Game.AI (runTurn) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..))

import           Class.Hook (MonadHook)
import qualified Class.Parity as Parity
import           Class.Play (MonadGame)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Class.Random as R
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Act (Act(Act))
import qualified Game.Model.Act as Act
import qualified Game.Model.Game as Game
import qualified Game.Model.Player as Player
import qualified Game.Engine as Engine
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Requirement as Requirement
import           Game.Model.Slot (Slot)

targetOptions :: [Ninja] -> Ninja -> Int -> [Act]
targetOptions ns n (Left -> i) = Act (Ninja.slot n) i . Ninja.slot <$> nTargets
  where
    nTargets = maybe [] (Requirement.targets ns n) $ Ninjas.getSkill i n

skillOptions :: [Ninja] -> Ninja -> [[Act]]
skillOptions ns n =
    filter (not . null) $ targetOptions ns n <$> [0..Ninja.numSkills n - 1]

-- | The higher this is, the more likely AI is to attack. Lower values allow it
-- to pool mana. At 0, the AI is disabled. This will certainly end up as a
-- file configuration once I figure out what I'm doing.
aggressionThreshold :: Int
aggressionThreshold = 2

-- | The higher this is, the more likely AI is to attack the vendetta target.
vendettaRatio :: Int
vendettaRatio = 5

run :: ∀ m. (MonadGame m, MonadRandom m) => Slot -> Ninja -> m (Maybe Act)
run vendetta n = runMaybeT do
    aggression <- R.random 0 aggressionThreshold
    guard $ aggression /= 0
    ninjas  <- P.ninjas
    choices <- MaybeT . R.choose $ (focusVendetta =<<) <$> skillOptions ninjas n
    MaybeT $ R.choose choices
  where
    focusVendetta act
      | Act.target act == vendetta = replicate vendettaRatio act
      | otherwise                  = singleton act

-- | Returns @Nothing@ only if all enemies are dead.
chooseVendetta :: ∀ m. (MonadGame m, MonadRandom m) => m (Maybe Slot)
chooseVendetta = do
    ninjas <- P.ninjas
    ninja  <- R.choose . filter Ninja.alive $ Parity.half Player.A ninjas
    let v   = Ninja.slot <$> ninja
    P.alter \game -> game { Game.vendetta = v }
    return v

-- | Returns @Nothing@ only if all enemies are dead.
getVendetta :: ∀ m. (MonadGame m, MonadRandom m) => m (Maybe Slot)
getVendetta = do
    vendetta <- Game.vendetta <$> P.game
    case vendetta of
        Nothing -> chooseVendetta
        Just v  -> do
            ninja <- P.ninja v
            if Ninja.alive ninja then
                return vendetta
            else
                chooseVendetta


runTurn :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => m ()
runTurn = do
    vendetta <- getVendetta
    case vendetta of
        Nothing -> Engine.runTurn [] -- All enemies are dead
        Just v  -> do
            ninjas <- P.ninjas
            acts   <- traverse (run v) . Parity.half Player.B $ fromList ninjas
            Engine.runTurn =<< R.shuffle (catMaybes acts)
