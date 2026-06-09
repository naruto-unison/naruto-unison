module Handler.Play.Turn
  ( Turn(..)
  , new
  ) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import           Class.Classed (Classed)
import qualified Class.Classed as Classed
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
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
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
new player ninjas Game{chakra, inactive, playing, victor} = Turn
    { chakra  = Parity.getOf player chakra
    , playing
    , victor
    , inactive = Parity.swap player inactive
    , ninjas   = censored
    , targets  = targets <$> censored
    }
  where
    reveal n = Parity.allied player n || n `is` Reveal
    revealed = fromList $ reveal <$> ninjas
    censored = censor revealed <$> ninjas
    skillTargets n skill = N.slot <$> Requirement.targets censored n skill
    targets n@Ninja{skills}
      | Parity.allied player n = skillTargets n <$> toList skills
      | otherwise              = replicate (length skills) []

censor :: UVector Bool -> Ninja -> Ninja
censor revealed n@Ninja{slot} =
    n { N.channels  = censorChannels $ N.channels n
      , N.cooldowns = censorAll      $ N.cooldowns n
      , N.charges   = censorAll      $ N.charges n
      , N.statuses  = filter hide    $ N.statuses n
      , N.traps     = filter hide    $ N.traps n
      }
  where
    reveal user = revealed !! Slot.toInt user
    censorChannels
      | reveal slot = id
      | otherwise   = filter $ (Invisible ∉) . Classed.classes
    censorAll
      | reveal slot = id
      | otherwise   = const mempty
    hide :: ∀ a. (Classed a, HasID a) => a -> Bool
    hide x = Hidden ∈ classes || (Invisible ∈ classes && not (reveal user))
      where
        classes = Classed.classes x
        user    = ID.user $ ID.from x
