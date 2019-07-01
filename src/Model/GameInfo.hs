module Model.GameInfo
  ( GameInfo(..)
  , censor
  ) where

import ClassyPrelude

import Data.Aeson ((.=), ToJSON(..), Value, object)
--import Data.List (intersect)
import Data.List.NonEmpty (NonEmpty(..))

import           Core.Util ((∈), (∉), intersects)
import qualified Class.Parity as Parity
import           Core.Model (Key, User)
import qualified Model.Channel as Channel
import           Model.Class (Class(..))
import           Model.Effect (Effect(..))
import qualified Model.Game as Game
import           Model.Game (Game(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Requirement as Requirement
import           Model.Requirement (Requirement(..))
import qualified Model.Skill as Skill
import           Model.Skill (Skill, Target(..))
import qualified Model.Player as Player
import           Model.Player (Player)
import qualified Model.Slot as Slot
import           Model.Slot (Slot)
import qualified Model.Status as Status
import qualified Model.Trap as Trap
import qualified Model.Variant as Variant
import qualified Engine.Adjust as Adjust
import qualified Engine.Cooldown as Cooldown

data GameInfo = GameInfo { vsWho  :: Key User
                         , vsUser :: User
                         , game   :: Game
                         , player :: Player
                         }

instance ToJSON GameInfo where
    toJSON GameInfo{..} = object
        [ "opponent"   .= vsUser
        , "game"       .= censor player game
        , "player"     .= player
        , "characters" .= characters
        ]
      where
        characters = Ninja.character <$> Game.ninjas game

censor :: Player -> Game -> Value
censor player = gameToJSON . censorPlayer player .
                Game.setChakra (Player.opponent player) 0

censorPlayer :: Player -> Game -> Game
censorPlayer player game = Game.alter (censorNinja game player) game

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
                                , Parity.allied player (Trap.user trap)
                                  || Invisible ∉ Trap.classes trap
                                  || revealed (Trap.user trap) ]
             }
    revealed slot = Ninja.is Reveal $ Game.ninja slot game
    mst st
      | Parity.allied player $ Status.user st = Just st
      | Invisible ∈ Status.classes st
        && not (revealed $ Status.user st) = Nothing
      | otherwise = case Status.effects st of
          []       -> Just st
          [Reveal] -> Nothing
          _        -> Just st
              { Status.effects = Reveal `delete` Status.effects st }

gameToJSON :: Game -> Value
gameToJSON g = object
    [ "chakra"  .= Game.chakra g
    , "ninjas"  .= (ninjaToJSON <$> Game.ninjas g)
    , "playing" .= Game.playing g
    , "victor"  .= Game.victor g
    , "targets" .= targets
    ]
  where
    ns = toList $ Game.ninjas g
    targets :: [[[Slot]]]
    targets = do
        n <- ns
        return do
            skill    <- Adjust.skills n
            let targs = skillTargets skill $ Ninja.slot n
            return do
                nt   <- ns
                let t = Ninja.slot nt
                guard $ Requirement.targetable skill n nt && t ∈ targs
                return t

-- | All targets that a 'Skill' from a a specific 'Ninja' affects.
skillTargets :: Skill -> Slot -> [Slot]
skillTargets skill c = filter target Slot.all
  where
    ts = fst <$>
         Skill.start skill ++ Skill.effects skill ++ Skill.interrupt skill
    harm = [Enemy, Enemies, REnemy, XEnemies] `intersects` ts
    target t
      | Everyone ∈ ts = True
      | not $ Parity.allied c t = harm
      | [XAlly, XAllies] `intersects` ts = c /= t
      | [Ally, Allies, RAlly] `intersects` ts = True
      | c == t = not harm
      | otherwise = False

ninjaToJSON :: Ninja -> Value
ninjaToJSON n = object
    [ "slot"      .= Ninja.slot n
    , "health"    .= Ninja.health n
    , "defense"   .= Ninja.defense n
    , "barrier"   .= Ninja.barrier n
    , "statuses"  .= filter ((Hidden ∉) . Status.classes) (Ninja.statuses n)
    , "charges"   .= Ninja.charges n
    , "cooldowns" .= Cooldown.active n
    , "variants"  .= Ninja.variants n
    , "copies"    .= Ninja.copies n
    , "channels"  .= Ninja.channels n
    , "traps"     .= filter ((Hidden ∉) . Trap.classes) (Ninja.traps n)
    , "face"      .= Ninja.face n
    --, "parrying"  .= Ninja.parrying n
    , "tags"      .= Ninja.tags n
    , "lastSkill" .= Ninja.lastSkill n
    , "skills"    .= (usable <$> Adjust.skills n)
    ]
  where
    usable skill = skill { Skill.require = fulfill $ Skill.require skill }
    fulfill req@HasI{}
      | Requirement.succeed req (Ninja.slot n) n = Usable
      | otherwise                                = Unusable
    fulfill x = x
