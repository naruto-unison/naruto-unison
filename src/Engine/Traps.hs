-- 'Trap.Trap' processing.
module Engine.Traps
  ( track
    -- Performing 'Trap.Trap's
  , runTurn
    -- Collecting 'Trap.Trap's
  , get, getOf, getTracked, getPer
  , broken
  ) where

import ClassyPrelude hiding ((\\))

import qualified Data.List as List
import           Data.List ((\\))

import           Core.Util ((∈), (∉))
import qualified Class.Parity as Parity
import qualified Class.Play as P
import           Class.Play (MonadGame, SavedPlay)
import           Class.Random (MonadRandom)
import qualified Model.Character as Character
import qualified Model.Context as Context
import qualified Model.Defense as Defense
import qualified Model.Game as Game
import           Model.Game (Game)
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import           Model.Player (Player)
import           Model.Slot (Slot)
import qualified Model.Trap as Trap
import           Model.Trap (Trap, Trigger(..))

savedPlay :: Slot -> Trap -> SavedPlay
savedPlay user trap
  | Trap.direction trap == Trap.From = first withTarget play
  | otherwise                        = play
  where
      withTarget ctx = ctx { Context.target = user }
      play           = Trap.effect trap $ Trap.tracker trap

getOf :: (MonoFoldable o, Element o ~ Trigger) => Slot -> o -> Ninja
      -> Seq SavedPlay
getOf user triggers n = savedPlay user <$>
                        filter (match . Trap.trigger) (Ninja.traps n)
  where
    match trigger = trigger ∈ Ninja.triggers n && trigger ∈ triggers

get :: Slot -> Ninja -> Seq SavedPlay
get user n = savedPlay user <$>
                      filter ((∈ Ninja.triggers n) . Trap.trigger)
                      (Ninja.traps n)

-- | Adds a value to 'Trap.tracker' of 'Ninja.traps' with a certain 'Trigger'.
track :: Trigger -> Int -> Ninja -> Ninja
track trigger amount n = n { Ninja.traps = tracked <$> Ninja.traps n }
  where
    tracked trap
      | Trap.trigger trap == trigger =
          trap { Trap.tracker = amount + Trap.tracker trap }
      | otherwise = trap

-- | 'OnBreak' effects of 'Ninja.defense' removed during a turn.
brokenOne :: Ninja -- ^ Old.
          -> Ninja -- ^ New.
          -> Ninja
brokenOne n n' =
    n' { Ninja.traps    = filter ((∉ triggers) . Trap.trigger) $ Ninja.traps n'
       , Ninja.triggers = foldl' (flip insertSet) (Ninja.triggers n') triggers
       }
  where
    triggers = OnBreak <$> List.nub (Defense.name <$> Ninja.defense n)
                        \\ List.nub (Defense.name <$> Ninja.defense n')

broken :: Game -> Game -> Game
broken game game' =
    game' { Game.ninjas = Game.zipNinjasWith brokenOne game game' }

-- | Conditionally returns 'Trap.Trap's that accept a numeric value.
getPer :: Bool -- ^ If 'False', returns 'mempty' instead.
       -> Trigger -- ^ Filter.
       -> Int -- ^ Value to pass to 'Trap.effect'.
       -> Ninja -- 'Ninja.traps' owner.
       -> Seq SavedPlay
getPer False _  _   _ = mempty
getPer True  tr amt n = [Trap.effect trap amt | trap <- Ninja.traps n
                                              , Trap.trigger trap == tr]

-- | Conditionally returns 'Character.hooks'.
getHooks :: Bool -- ^ If 'False', returns 'mempty' instead.
         -> Trigger -- ^ Filter.
         -> Int -- ^ Value to pass to 'Character.hooks' effects.
         -> Ninja -- ^ 'Character.hooks' owner.
         -> Seq (Game -> Game)
getHooks False _  _   _ = mempty
getHooks True  tr amt n = [Game.adjust (Ninja.slot n) $ f amt
                              | (p, f) <- Character.hooks $ Ninja.character n
                              , tr == p]

-- | Tallies 'PerHealed' and 'PerDamaged' hooks.
getTurnHooks :: Player -- ^ Player during the current turn.
             -> Ninja -- ^ Old.
             -> Ninja -- ^ New.
             -> Seq (Game -> Game)
getTurnHooks player n n'
  | hp < 0 && Parity.allied player user       = getHooks True PerHealed (-hp) n'
  | hp > 0 && not (Parity.allied player user) = getHooks True PerDamaged hp n'
  | otherwise                                 = mempty
  where
    user = Ninja.slot n'
    hp   = Ninja.health n - Ninja.health n'

-- | Conditionally returns 'Trap.tracker' 'Trap.Trap's.
getTracked :: Bool -- ^ If 'False', returns 'mempty' instead.
           -> Trigger -- ^ Filter.
           -> Ninja -- ^ 'Ninja.traps' owner.
           -> Seq SavedPlay
getTracked False _ _ = mempty
getTracked True tr n =
    [Trap.effect trap $ Trap.tracker trap | trap <- Ninja.traps n
                                          , Trap.trigger trap == tr
                                          , Trap.dur trap <= 2
                                          , Trap.tracker trap > 0]

-- | Tallies 'PerHealed' and 'PerDamaged' traps.
getTurnPer :: Player -- ^ Player during the current turn.
           -> Ninja -- ^ Old.
           -> Ninja -- ^ New.
           -> Seq SavedPlay
getTurnPer player n n'
  | hp < 0 && Parity.allied player user       = getPer True PerHealed (-hp) n'
  | hp > 0 && not (Parity.allied player user) = getPer True PerDamaged hp n'
  | otherwise                                 = mempty
  where
    user = Ninja.slot n'
    hp   = Ninja.health n - Ninja.health n'

-- | Returns 'OnNoAction' 'Trap.Trap's.
getTurnNot :: Player -- ^ Player during the current turn.
           -> Ninja -- ^ 'Ninja.flags' owner.
           -> Seq SavedPlay
getTurnNot player n
  | Parity.allied player user = getOf user [OnNoAction] n
  | otherwise                 = mempty
  where
    user = Ninja.slot n

-- | Applies per-turn traps at the end of a turn.
getTurn :: Game -- ^ Old.
        -> Game -- ^ New.
        -> Seq SavedPlay
getTurn game game' = concat $
    Game.zipNinjasWith (getTurnPer player) game game'
    ++ (getTurnNot player <$> Game.ninjas game')
  where
    player = Game.playing game

-- | Processes and runs all 'Trap.Trap's at the end of a turn.
runTurn :: ∀ m. (MonadGame m, MonadRandom m) => Game -> m ()
runTurn game = do
    player <- P.player
    P.modify $ processPer player game
    traps <- getTurn game <$> P.game
    traverse_ P.launch traps

-- | Processes per-turn 'Trap.Trap's at the end of a turn.
processPer :: Player -> Game -> Game -> Game
processPer player game game' =
    foldl' (flip ($)) game' . concat $
    Game.zipNinjasWith (getTurnHooks player) game game'
