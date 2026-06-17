module Game.AI (runTurn) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..), hoistMaybe)

import           Class.Hook (MonadHook)
import qualified Class.Parity as Parity
import           Class.Play (MonadGame)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Class.Random as R
import           Game.Model.Chakras (Chakra(..), Chakras)
import qualified Game.Model.Chakras as Chakras
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Game (Game(Game))
import qualified Game.Model.Game as Game
import qualified Game.Model.Player as Player
import qualified Game.Engine as Engine
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as N
import qualified Game.Model.Requirement as Requirement
import qualified Game.Model.Skill
import           Game.Model.Slot (Slot)
import           Util ((!?))

targetOptions :: [Ninja] -> Ninja -> Int -> [Context]
targetOptions ns n i =
    [ makeOption skill target | skill  <- maybeToList $ n.skills !? i,
                                target <- Requirement.targets ns n skill ]
  where
    makeOption skill target = Context
        { new       = True
        , user      = n.slot
        , skill
        , target    = target.slot
        , continues = False
        }

skillOptions :: [Ninja] -> Ninja -> [[Context]]
skillOptions ns n = filter (not . null)
    $ targetOptions ns n <$> [0..length n.skills - 1]

-- | The higher this is, the more likely AI is to attack. Lower values allow it
-- to pool mana. At 0, the AI is disabled. This will certainly end up as a
-- file configuration once I figure out what I'm doing.
aggressionThreshold :: Int
aggressionThreshold = 2

-- | The higher this is, the more likely AI is to attack the vendetta target.
vendettaRatio :: Int
vendettaRatio = 5

run :: ∀ m. (MonadGame m, MonadRandom m) => Slot -> Ninja -> m (Maybe Context)
run vendetta n = runMaybeT do
    aggression <- R.range (0, aggressionThreshold)
    guard $ aggression /= 0
    ninjas <- toList <$> P.ninjas
    Just choices <- R.choose $ (>>= focusVendetta) <$> skillOptions ninjas n
    MaybeT $ R.choose choices
  where
    focusVendetta act
      | act.target == vendetta = replicate vendettaRatio act
      | otherwise              = singleton act

-- | Returns @Nothing@ only if all enemies are dead.
chooseVendetta :: ∀ m. (MonadGame m, MonadRandom m) => m (Maybe Slot)
chooseVendetta = do
    ninjas <- P.ninjas
    ninja  <- R.choose . filter N.alive $ Parity.half Player.A ninjas
    let v   = N.slot <$> ninja
    P.alterGame $ Game.setVendetta v
    return v

-- | Returns @Nothing@ only if all enemies are dead.
getVendetta :: ∀ m. (MonadGame m, MonadRandom m) => m (Maybe Slot)
getVendetta = do
    Game{vendetta} <- P.game
    fromMaybe <$> chooseVendetta <*> runMaybeT do
        v     <- hoistMaybe vendetta
        ninja <- lift $ P.ninja v
        guard $ N.alive ninja
        return vendetta


runTurn :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => m ()
runTurn = do
    vendetta <- getVendetta
    case vendetta of
        Nothing -> Engine.runTurn [] -- All enemies are dead
        Just v  -> do
            ninjas   <- P.ninjas
            acts     <- mapM (run v) $ Parity.half Player.B ninjas
            contexts <- R.shuffle $ catMaybes acts
            Game{chakra = (_, chakras)} <- P.game
            let (contexts', chakras') = buyActs (toList contexts) chakras
            P.alterGame $ Game.setChakra Player.B chakras'
            Engine.runTurn contexts'

buyActs :: [Context] -> Chakras -> ([Context], Chakras)
buyActs [] gameChakras = ([], gameChakras)
buyActs (act:acts) gameChakras =
    case spendWithExchange act.skill.cost gameChakras of
        Just gameChakras' -> first (act :) $ buyActs acts gameChakras'
        Nothing           -> ([], gameChakras)

spendWithExchange :: Chakras -> Chakras -> Maybe Chakras
spendWithExchange cost gameChakras = do
    let (rands, nonrands) = partition (== Rand) cost
    afterNonrand <- Chakras.checkedSpend nonrands gameChakras
    let exchangeForRandoms = fromList $ take (length rands)
            $ Chakras.toDescendingList afterNonrand
    guard $ length exchangeForRandoms == length rands
    return $ Chakras.spend exchangeForRandoms afterNonrand
