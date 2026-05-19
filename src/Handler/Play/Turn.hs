module Handler.Play.Turn
  ( Turn(..)
  , new
  ) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import qualified Class.Parity as Parity
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Chakras (Chakras)
import           Game.Model.Channel (Channel(Channel))
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Effect(..))
import           Game.Model.Game (Game(Game))
import qualified Game.Model.Game
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as N
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import qualified Game.Model.Requirement as Requirement
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Game.Model.Status (Status(Status))
import qualified Game.Model.Status as Status
import           Game.Model.Trap (Trap(Trap))
import qualified Game.Model.Trap
import           OrphanInstances.Ninja ()
import           Util ((!!), (∈), (∉))

-- | Intermediate type for marshaling to JSON.
-- Includes censorship of 'Invisible' 'Status.Status'es, enemy cooldowns, etc.
data Turn = Turn
    { chakra   :: Chakras
    , playing  :: Player
    , victor   :: [Player]
    , inactive :: (Int, Int)
    , ninjas   :: [Ninja]
    , targets  :: [[[Slot]]]
    } deriving (Generic)

instance ToJSON Turn

--  | Encodes game state into a form suitable for sending to the client.
new :: Player -> [Ninja] -> Game -> Turn
new player ninjas Game{chakra, inactive, playing, vendetta, victor} = Turn
    { chakra  = Parity.getOf player chakra
    , playing
    , victor
    , inactive = swapInactive player inactive
    , ninjas   = censored
    , targets  = targets <$> censored
    }
  where
    censored  = censor vendetta player ninjas <$> ninjas
    swapInactive Player.A = id
    swapInactive Player.B = swap
    targets n
      | Parity.allied player n = (N.slot <$>)
                               . Requirement.targets censored n
                               <$> Ninjas.skills n
      | otherwise              = replicate (N.numSkills n) []

censor :: (Maybe Slot) -> Player -> [Ninja] -> Ninja -> Ninja
censor vendetta player ninjas n
  | Parity.allied player n = n'
  | n `is` Reveal          = n'
  | isJust vendetta        = n'
      { N.channels = filter filt $ N.channels n' }
  | otherwise              = n'
      { N.channels   = filter filt $ N.channels n'
      , N.charges    = mempty
      , N.cooldowns  = mempty
      , N.alternates = 0 <$ N.alternates n'
      }
  where
    filt (Channel Skill{classes} _ _) = not $ Invisible ∈ classes
    n' = n { N.statuses  = mapMaybe mst $ N.statuses n
           , N.lastSkill = Nothing
           , N.traps     = [trap | trap@Trap{classes, user} <- N.traps n
                                     , Parity.allied player user
                                       || Invisible ∉ classes
                                       || revealed user]
           }
    revealed slot = ninjas !! Slot.toInt slot `is` Reveal
    mst st@Status{classes, effects, user}
      | Parity.allied player user                  = Just st
      | Invisible ∈ classes && not (revealed user) = Nothing
      | otherwise = case effects of
          [Reveal] -> Nothing
          []       -> Just st
          _        -> Just st { Status.effects = delete Reveal effects }
