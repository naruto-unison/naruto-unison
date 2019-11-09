{-# LANGUAGE DeriveAnyClass #-}

module Handler.Play.Turn
  ( Turn(..)
  , new
  ) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import qualified Class.Parity as Parity
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Chakra (Chakras)
import qualified Game.Model.Channel as Channel
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Effect(..))
import           Game.Model.Game (Game(..))
import qualified Game.Model.Game as Game
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import qualified Game.Model.Requirement as Requirement
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import qualified Game.Model.Status as Status
import qualified Game.Model.Trap as Trap
import           OrphanInstances.Ninja ()
import           Util ((!!), (∈), (∉))

-- | Intermediate type for marshaling to JSON.
-- Includes censorship of 'Invisible' 'Status.Status'es, enemy cooldowns, etc.
data Turn = Turn { chakra   :: Chakras
                 , playing  :: Player
                 , victor   :: [Player]
                 , inactive :: (Int, Int)
                 , ninjas   :: [Ninja]
                 , targets  :: [[[Slot]]]
                 } deriving (Generic, ToJSON)

--  | Encodes game state into a form suitable for sending to the client.
new :: Player -> [Ninja] -> Game -> Turn
new player ninjas game = Turn { chakra  = Parity.getOf player $ Game.chakra game
                              , playing = Game.playing game
                              , victor  = Game.victor game
                              , inactive
                              , ninjas  = censored
                              , targets = targets <$> censored
                              }
  where
    censored = censor (Game.vendetta game) player ninjas <$> ninjas
    inactive = case player of
        Player.A -> Game.inactive game
        Player.B -> swap $ Game.inactive game
    targets n
      | Parity.allied player n = (Ninja.slot <$>) .
                                 Requirement.targets censored n <$>
                                 Ninjas.skills n
      | otherwise              = replicate Ninja.skillSize []

censor :: (Maybe Slot) -> Player -> [Ninja] -> Ninja -> Ninja
censor vendetta player ninjas n
  | Parity.allied player n = n'
  | n `is` Reveal          = n'
  | isJust vendetta        = n'
      { Ninja.channels = filter filt $ Ninja.channels n }
  | otherwise              = n'
      { Ninja.cooldowns  = mempty
      , Ninja.charges    = mempty
      , Ninja.channels   = filter filt $ Ninja.channels n
      , Ninja.alternates = 0 <$ Ninja.alternates n
      }
  where
    filt chan = not $ Invisible ∈ Skill.classes (Channel.skill chan)
    n'   = n { Ninja.statuses  = mapMaybe mst $ Ninja.statuses n
             , Ninja.lastSkill = Nothing
             , Ninja.traps     = [trap | trap <- Ninja.traps n
                                       , Parity.allied player (Trap.user trap)
                                         || Invisible ∉ Trap.classes trap
                                         || revealed (Trap.user trap)]
             }
    revealed slot = ninjas !! Slot.toInt slot `is` Reveal
    mst st
      | Parity.allied player $ Status.user st = Just st
      | Invisible ∈ Status.classes st
        && not (revealed $ Status.user st) = Nothing
      | otherwise = case Status.effects st of
          []       -> Just st
          [Reveal] -> Nothing
          _        -> Just st
                          { Status.effects = Reveal `delete` Status.effects st }
