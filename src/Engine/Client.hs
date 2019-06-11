module Engine.Client
  ( censor
  ) where

import ClassyPrelude.Yesod
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))

import           Core.Util ((∈), (∉))
import qualified Class.Parity as Parity
import qualified Model.Channel as Channel
import           Model.Class (Class(..))
import           Model.Effect (Effect(..))
import qualified Model.Game as Game
import           Model.Game (Game)
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Player as Player
import           Model.Player (Player)
import qualified Model.Skill as Skill
import qualified Model.Status as Status
import qualified Model.Trap as Trap
import qualified Model.Variant as Variant

censorNinja :: Game -> Player -> Ninja -> Ninja
censorNinja game player n
  | Parity.allied player n = n'
  | Ninja.is Reveal n      = n'
  | otherwise              = n'
      { Ninja.cooldowns = mempty
      , Ninja.charges   = mempty
      , Ninja.variants  = replicate 4 (Variant.none :| [])
      , Ninja.channels  = filter filt $ Ninja.channels n
      , Ninja.lastSkill = Nothing
      }
  where
    filt = (not . (Invisible ∈)) . Skill.classes . Channel.skill
    n'   = n { Ninja.statuses = mapMaybe mst $ Ninja.statuses n
             , Ninja.traps    = [ trap | trap <- Ninja.traps n
                                , Parity.allied player (Trap.source trap)
                                  || Invisible ∉ Trap.classes trap
                                  || revealed (Trap.source trap) ]
             }
    revealed slot = Ninja.is Reveal $ Game.ninja slot game
    mst st
      | Parity.allied player $ Status.source st = Just st
      | Invisible ∈ Status.classes st
        && not (revealed $ Status.user st) = Nothing
      | otherwise = case Status.effects st of
          []       -> Just st
          [Reveal] -> Nothing
          _        -> Just st
              { Status.effects = List.delete Reveal $ Status.effects st }

censorPlayer :: Player -> Game -> Game
censorPlayer player game = Game.alter (censorNinja game player <$>) game

censor :: Player -> Game -> Game
censor player = censorPlayer player . Game.setChakra (Player.opponent player) 0
