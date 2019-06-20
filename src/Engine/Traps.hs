-- 'Trap' processing.
module Engine.Traps
  ( track
    -- Performing 'Trap's
  , runTurn
    -- Collecting 'Trap's
  , get, getClassed, getTo, getTracked, getPer
  , broken
  ) where

import ClassyPrelude.Yesod hiding ((\\), get)
import qualified Data.List as List
import           Data.List ((\\))

import           Core.Util ((∈), (∉))
import qualified Class.Parity as Parity
import qualified Class.Play as P
import           Class.Play (MonadGame, SavedPlay)
import           Class.Random (MonadRandom)
import qualified Model.Character as Character
import           Model.Class (Class(..))
import qualified Model.Context as Context
import qualified Model.Defense as Defense
import qualified Model.Game as Game
import           Model.Game (Game)
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja, Flag(..))
import           Model.Player (Player)
import           Model.Slot (Slot)
import qualified Model.Trap as Trap
import           Model.Trap (Trigger(..))

get :: Slot -> Bool -> Trigger -> Ninja -> Seq SavedPlay
get _    False _       = const mempty
get user True  trigger = (savedPlay <$>) .
                         filter ((trigger ==) . Trap.trigger) . Ninja.traps
  where
    withTarget ctx = ctx { Context.target = user }
    savedPlay trap
      | Trap.direction trap == Trap.From = first withTarget play
      | otherwise                        = play
      where
        play = Trap.effect trap $ Trap.tracker trap

getClassed :: Seq Class -> Slot -> Bool -> (Class -> Trigger) -> Ninja
           -> Seq SavedPlay
getClassed _       _    False _       _ = mempty
getClassed classes user True  trigger n = do
    cla <- classes
    get user True (trigger cla) n

getTo :: Seq Ninja -> Trigger -> Ninja -> Seq SavedPlay
getTo xs trigger n
  | null xs   = mempty
  | otherwise = do
      trap <- Ninja.traps n
      guard $ Trap.trigger trap == trigger
      case Trap.direction trap of
          Trap.From -> do
              x <- xs
              let retarget ctx = ctx { Context.target = Ninja.slot x}
              return . first retarget . Trap.effect trap $ Trap.tracker trap
          _ ->
              return . Trap.effect trap $ Trap.tracker trap


track :: Trigger -> Int -> Ninja -> Ninja
track trigger amount n = n { Ninja.traps = tracked <$> Ninja.traps n }
  where
    tracked trap
      | Trap.trigger trap == trigger =
          trap { Trap.tracker = amount + Trap.tracker trap }
      | otherwise = trap

-- | 'OnBreak' effects of
broken :: Ninja -> Ninja -> (Ninja, Seq SavedPlay)
broken n n' =
    ( n' { Ninja.traps = filter ((∉ broke) . Trap.trigger) traps }
    , [Trap.effect trap 0 | trap <- traps, Trap.trigger trap ∈ broke]
    )
  where
    traps = Ninja.traps n'
    broke = OnBreak <$> List.nub (Defense.name <$> Ninja.defense n)
                     \\ List.nub (Defense.name <$> Ninja.defense n')

getPer :: Bool -> Trigger -> Int -> Ninja -> Seq SavedPlay
getPer False _  _   _ = mempty
getPer True  tr amt n = [Trap.effect trap amt | trap <- Ninja.traps n
                                              , Trap.trigger trap == tr]

getHooks :: Bool -> Trigger -> Int -> Ninja -> Seq (Game -> Game)
getHooks False _  _   _ = mempty
getHooks True  tr amt n = [Game.adjust (Ninja.slot n) $ f amt
                              | (p, f) <- Character.hooks $ Ninja.character n
                              , tr == p]

getTurnHooks :: Player -> Ninja -> Ninja -> Seq (Game -> Game)
getTurnHooks player n n'
  | hp < 0 && Parity.allied player user       = getHooks True PerHealed (-hp) n'
  | hp > 0 && not (Parity.allied player user) = getHooks True PerDamaged hp n'
  | otherwise                                 = mempty
  where
    user = Ninja.slot n'
    hp   = Ninja.health n - Ninja.health n'

getTracked :: Bool -> Trigger -> Ninja -> Seq SavedPlay
getTracked False _ _ = mempty
getTracked True tr n =
    [Trap.effect trap $ Trap.tracker trap | trap <- Ninja.traps n
                                          , Trap.trigger trap == tr
                                          , Trap.dur trap <= 2
                                          , Trap.tracker trap > 0]

getTurnPer :: Player -> Ninja -> Ninja -> Seq SavedPlay
getTurnPer player n n'
  | hp < 0 && Parity.allied player user       = getPer True PerHealed (-hp) n'
  | hp > 0 && not (Parity.allied player user) = getPer True PerDamaged hp n'
  | otherwise                                 = mempty
  where
    user = Ninja.slot n'
    hp   = Ninja.health n - Ninja.health n'

getTurnNot :: Player -> Ninja -> Seq SavedPlay
getTurnNot player n
  | Parity.allied player user = get user (Acted ∉ flags)  OnNoAction n
                             ++ get user (Harmed ∉ flags) OnNoHarm   n
  | otherwise = mempty
  where
    user     = Ninja.slot n
    flags    = Ninja.flags n

getTurn :: Game -> Game -> Seq SavedPlay
getTurn game game' = concat $
    Game.zipNinjasWith (getTurnPer player) game game'
    ++ (getTurnNot player <$> Game.ninjas game')
  where
    player = Game.playing game

trackDamaged :: Ninja -> Ninja -> Ninja
trackDamaged n n' = track TrackDamaged (Ninja.healthLost n n') n'

runTurn :: ∀ m. (MonadGame m, MonadRandom m) => Game -> m ()
runTurn game = do
    player <- P.player
    P.modify $ processPer player game
    traps <- getTurn game <$> P.game
    traverse_ P.launch traps

processPer :: Player -> Game -> Game -> Game
processPer player game game' =
    foldl' (flip ($))
    game' { Game.ninjas = Game.zipNinjasWith trackDamaged game game' } .
    concat $ Game.zipNinjasWith (getTurnHooks player) game game'
