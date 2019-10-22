module Game.AI (runTurn) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Ratio (Ratio, (%), numerator, denominator)

import           Class.Hook (MonadHook)
import qualified Class.Parity as Parity
import           Class.Play (MonadGame)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Class.Random as R
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Act (Act(Act))
import qualified Game.Model.Act as Act
import           Game.Model.Chakra (Chakras)
import qualified Game.Model.Chakra as Chakra
import qualified Game.Model.Game as Game
import qualified Game.Model.Player as Player
import qualified Game.Engine as Engine
import qualified Game.Engine.Chakras as Chakras
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Requirement as Requirement
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Util ((—))

afford :: Chakras -> Skill -> Bool
afford chaks Skill{cost} =
    not (Chakra.lack diff)
    && Chakra.total diff { Chakra.rand = 0 } >= Chakra.rand cost
  where
    diff = chaks - cost

targetOptions :: [Ninja] -> Ninja -> Chakras -> Int -> [Act]
targetOptions ns n chaks (Left -> i)
  | afford chaks skill = Act (Ninja.slot n) i . Ninja.slot <$> nTargets
  | otherwise          = []
  where
    skill    = Ninjas.getSkill i n
    nTargets = Requirement.targets ns n skill

skillOptions :: [Ninja] -> Ninja -> Chakras -> [[Act]]
skillOptions ns n chaks =
    filter (not . null) $ targetOptions ns n chaks <$> [0..Ninja.skillSize - 1]

-- | The higher this is, the more likely AI is to attack. Lower values allow it
-- to pool mana. At 0, the AI is disabled. This will certainly end up as a
-- file configuration once I figure out what I'm doing.
aggressionThreshold :: Int
aggressionThreshold = 4

-- | The higher this is, the more likely AI is to attack the vendetta target.
vendettaRatio :: Ratio Int
vendettaRatio = 9 % 2

run :: ∀ m. (MonadGame m, MonadRandom m) => Slot -> Ninja -> m (Maybe Act)
run vendetta n = runMaybeT do
    aggression <- R.random 0 aggressionThreshold
    guard $ aggression /= 0
    ninjas     <- P.ninjas
    (_, chaks) <- Game.chakra <$> P.game
    let options = (focusVendetta =<<) <$> skillOptions ninjas n chaks
    choices    <- MaybeT $ R.choose options
    choice     <- MaybeT $ R.choose choices
    let skill   = Ninjas.getSkill (Act.skill choice) n
        cost    = Skill.cost skill
        rand    = Chakra.rand cost
        unrand  = cost { Chakra.rand = 0 }
    P.alter $ Game.adjustChakra Player.B (— unrand)
    void $ Chakras.removeFrom Player.B rand
    return choice
  where
    focusVendetta act
      | Act.target act == vendetta = replicate (numerator vendettaRatio) act
      | otherwise                  = replicate (denominator vendettaRatio) act

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
