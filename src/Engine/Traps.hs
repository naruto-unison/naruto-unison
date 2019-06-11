module Engine.Traps
  ( get
  , broken
  , direct
  --, direct
  , entrap
  , getClass, getFrom, getTo, getTracked, getPer
  --, get, getClass, getFrom, getPer, getTo, getTracked
  , runPer, runTurn
  ) where

import ClassyPrelude.Yesod hiding ((\\), get)
import qualified Data.List as List
import           Data.List ((\\))

import           Core.Util ((∈), (∉))
import qualified Class.Parity as Parity
import qualified Class.Play as P
import           Class.Play (GameT, Play)
import           Class.Random (RandomT)
import qualified Model.Character as Character
import           Model.Class (Class(..))
import qualified Model.Context as Context
import           Model.Context (Context)
import qualified Model.Defense as Defense
import qualified Model.Game as Game
import           Model.Game (Game)
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja, Flag(..))
import           Model.Player (Player)
import           Model.Slot (Slot)
import qualified Model.Trap as Trap
import           Model.Trap (Trigger(..))

broken :: Ninja -> Ninja -> (Ninja, Seq (Context, Play ()))
broken n n' =
    ( n' { Ninja.traps = filter ((∉ broke) . Trap.trigger) traps }
    , [Trap.effect trap 0 | trap <- traps, Trap.trigger trap ∈ broke]
    )
  where
    traps = Ninja.traps n'
    broke = OnBreak <$> List.nub (Defense.name <$> Ninja.defense n)
                     \\ List.nub (Defense.name <$> Ninja.defense n')

direct :: ∀ m. (GameT m, RandomT m) => Slot -> Seq (Context, Play()) -> m ()
direct user = traverse_ $ P.launch . first changeUser
  where
    changeUser ctx = ctx { Context.user = user }

get :: Bool -> Trigger -> Ninja -> Seq (Context, Play ())
get False _ _ = mempty
get True tr n = [Trap.effect trap 0 | trap <- Ninja.traps n
                                    , Trap.trigger trap == tr]

getTo :: Bool -> Trigger -> Ninja -> Seq (Context, Play ())
getTo False _ _ = mempty
getTo True tr n = [ Trap.effect trap 0 | trap <- Ninja.traps n
                                       , Trap.trigger trap == tr
                                       , Trap.direction trap /= Trap.From]

getFrom :: Trigger -> Ninja -> Ninja -> Seq (Context, Play ())
getFrom tr n from =
    [ first retarget
    $ Trap.effect trap 0 | trap <- Ninja.traps n
                         , Trap.trigger trap == tr
                         , Trap.direction trap == Trap.From ]
  where
    retarget ctx = ctx { Context.target = Ninja.slot from }


getClass :: ∀ f. (Foldable f, Functor f) => Bool -> (Class -> Trigger)
         -> f Class -> Ninja -> Seq (Context, Play ())
getClass False _  _       _ = mempty
getClass True  tr classes n = asum $ classTrigger <$> classes
    where
      classTrigger :: Class -> Seq (Context, Play ())
      classTrigger cla = get True (tr cla) n

getPer :: Bool -> Trigger -> Int -> Ninja -> Seq (Context, Play ())
getPer False _  _   _ = mempty
getPer True  tr amt n = [Trap.effect trap amt | trap <- Ninja.traps n
                                              , Trap.trigger trap == tr]

getHooks :: Bool -> Trigger -> Int -> Ninja -> Seq (Game -> Game)
getHooks False _  _   _ = mempty
getHooks True  tr amt n = [Game.adjust (Ninja.slot n) $ f amt
                              | (p, f) <- Character.hooks $ Ninja.character n
                              , tr == p]

getTurnPer :: Player -> Ninja -> Ninja -> Seq (Context, Play ())
getTurnPer player n n'
  | hp < 0 && Parity.allied player user       = getPer True PerHealed (-hp) n'
  | hp > 0 && not (Parity.allied player user) = getPer True PerDamaged hp n'
  | otherwise                                 = mempty
  where
    user = Ninja.slot n'
    hp   = Ninja.health n - Ninja.health n'

getTurnHooks :: Player -> Ninja -> Ninja -> Seq (Game -> Game)
getTurnHooks player n n'
  | hp < 0 && Parity.allied player user       = getHooks True PerHealed (-hp) n'
  | hp > 0 && not (Parity.allied player user) = getHooks True PerDamaged hp n'
  | otherwise                                 = mempty
  where
    user = Ninja.slot n'
    hp   = Ninja.health n - Ninja.health n'

getTracked :: Bool -> Trigger -> Ninja -> Seq (Context, Play ())
getTracked False _ _ = mempty
getTracked True tr n =
    [Trap.effect trap $ Trap.tracker trap | trap <- Ninja.traps n
                                          , Trap.trigger trap == tr
                                          , Trap.dur trap <= 2
                                          , Trap.tracker trap > 0]

getTurn :: Player -> Ninja -> Seq (Context, Play ())
getTurn player n
  | Parity.allied player user = getTo (Acted ∉ flags) OnNoAction n
                                ++ getTo (Harmed ∉ flags) OnNoHarm n
  | otherwise = mempty
  where
    user     = Ninja.slot n
    flags    = Ninja.flags n

track :: Ninja -> Ninja -> Ninja
track n n' = n' { Ninja.traps = updatePer <$> Ninja.traps n' }
  where
    updatePer trap = case Trap.trigger trap of
        TrackDamaged ->
            trap { Trap.tracker = Ninja.healthLost n n' + Trap.tracker trap }
        _ -> trap

runPer :: ∀ m. GameT m => Game -> m ()
runPer game = do
    player <- P.player
    P.modify $ processPer player game

processPer :: Player -> Game -> Game -> Game
processPer player game game' = foldl' (flip ($))
                               game' { Game.ninjas = ninjas'
                                     , Game.traps  = traps ++ Game.traps game'
                                     }
                               hooks
  where
    ninjas' = zipWith track (Game.ninjas game) (Game.ninjas game')
    traps   = concat $ zipWith (getTurnPer player) (Game.ninjas game) ninjas'
    hooks   = concat $ zipWith (getTurnHooks player) (Game.ninjas game) ninjas'

runTurn :: Player -> Game -> Game
runTurn player game = game
    { Game.traps = (Game.ninjas game >>= getTurn player) ++ Game.traps game }

entrap :: Slot -> Seq (Context, Play ()) -> Game -> Game
entrap source trapped game = game
    { Game.traps = Game.traps game ++ (first resrc <$> trapped) }
  where
    resrc ctx = ctx { Context.source = source }
