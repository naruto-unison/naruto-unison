module Handler.Play.Turn
  ( Turn(..)
  , new
  ) where

import ClassyPrelude

import Data.Aeson (ToJSON)
import Data.Enum.Set (EnumSet)

import           Class.Classed (Classed(..))
import qualified Class.Parity as Parity
import           Game.Model.Chakras (Chakras)
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Effect(..))
import           Game.Model.Game (Game(Game))
import qualified Game.Model.Game
import           Game.Model.ID (HasID)
import qualified Game.Model.ID as ID
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Player (Player)
import qualified Game.Model.Requirement as Requirement
import           Game.Model.Slot (Slot, SlotSet)
import           Handler.Play.Snapshot (Snapshot)
import qualified Handler.Play.Snapshot as Snapshot
import           Util ((∈), (∉))

-- | Intermediate type for marshaling to JSON.
-- Includes censorship of 'Invisible' 'Status.Status'es, enemy cooldowns, etc.
data Turn = Turn
    { chakra    :: Chakras
    , playing   :: Player
    , victor    :: EnumSet Player
    , inactive  :: (Int, Int)
    , ninjas    :: [Ninja]
    , snapshots :: [Snapshot]
    , targets   :: [[[Slot]]]
    } deriving (Generic)

instance ToJSON Turn

--  | Encodes game state into a form suitable for sending to the client.
new :: Player -> [Ninja] -> [Snapshot] -> Game -> Turn
new player ninjas snapshots Game{chakra, inactive, playing, victor} = Turn
    { chakra  = Parity.getOf player chakra
    , playing
    , victor
    , inactive = Parity.swap player inactive
    , ninjas   = censored
    , snapshots = censorSnapshot player <$> snapshots
    , targets   = targets <$> censored
    }
  where
    censored = censorNinjas player ninjas
    skillTargets n skill = N.slot <$> Requirement.targets censored n skill
    targets n@Ninja{skills}
      | Parity.allied player n = skillTargets n <$> toList skills
      | otherwise              = replicate (length skills) []

censorSnapshot :: Player -> Snapshot -> Snapshot
censorSnapshot player snapshot =
    snapshot { Snapshot.ninjas = censorNinjas player snapshot.ninjas }

censorNinjas :: Player -> [Ninja] -> [Ninja]
censorNinjas player ninjas = censorNinja revealed <$> ninjas
  where
    reveal n = Parity.allied player n || n `is` Reveal
    revealed = setFromList $ N.slot <$> filter reveal ninjas

censorNinja :: SlotSet -> Ninja -> Ninja
censorNinja revealed n =
    n { N.channels  = censorChannels n.channels
      , N.cooldowns = censorAll      n.cooldowns
      , N.charges   = censorAll      n.charges
      , N.statuses  = filter hide    n.statuses
      , N.traps     = filter hide    n.traps
      }
  where
    reveal user = user ∈ revealed
    revealN = reveal n.slot
    censorChannels
      | revealN   = id
      | otherwise = filter $ (Invisible ∉) . getClasses
    censorAll
      | revealN   = id
      | otherwise = const mempty
    hide :: ∀ a. (Classed a, HasID a) => a -> Bool
    hide x = Hidden ∈ classes || (Invisible ∈ classes && not (reveal user))
      where
        classes = getClasses x
        user    = (ID.from x).user
