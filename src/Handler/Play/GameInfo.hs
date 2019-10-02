module Handler.Play.GameInfo
  ( GameInfo(..)
  , gameToJSON
  ) where

import ClassyPrelude

import Data.Aeson ((.=), ToJSON(..), Value, object)

import           Util ((!!), (∈), (∉), intersects)
import           Application.Model (Key, User)
import qualified Class.Parity as Parity
import qualified Game.Model.Channel as Channel
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Game as Game
import           Game.Model.Game (Game(..))
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Requirement as Requirement
import           Game.Model.Requirement (Requirement(..))
import qualified Game.Model.Skill as Skill
import           Game.Model.Skill (Skill, Target(..))
import qualified Game.Model.Player as Player
import           Game.Model.Player (Player)
import qualified Game.Model.Slot as Slot
import           Game.Model.Slot (Slot)
import qualified Game.Model.Status as Status
import qualified Game.Model.Trap as Trap
import qualified Game.Model.Variant as Variant
import qualified Game.Engine.Cooldown as Cooldown
import qualified Game.Engine.Ninjas as Ninjas

data GameInfo = GameInfo { vsWho  :: Key User
                         , vsUser :: User
                         , game   :: Game
                         , ninjas :: [Ninja]
                         , player :: Player
                         }

instance ToJSON GameInfo where
    toJSON GameInfo{..} = object
        [ "opponent"   .= vsUser
        , "game"       .= gameJson
        , "player"     .= player
        ]
      where
        gameJson = gameToJSON player ninjas $
                   Game.setChakra (Player.opponent player) 0 game

censor :: Player -> [Ninja] -> [Value]
censor player ninjas = ninjaToJSON . censorNinja player ninjas <$> ninjas

censorNinja :: Player -> [Ninja] -> Ninja -> Ninja
censorNinja player ninjas n
  | Parity.allied player n = n'
  | n `is` Reveal          = n'
  | otherwise              = n'
      { Ninja.cooldowns = mempty
      , Ninja.charges   = mempty
      , Ninja.variants  = (Variant.none :| []) <$ Ninja.variants n
      , Ninja.channels  = filter filt $ Ninja.channels n
      , Ninja.lastSkill = Nothing
      }
  where
    filt chan = not $ Invisible ∈ Skill.classes (Channel.skill chan)
    n'   = n { Ninja.statuses = mapMaybe mst $ Ninja.statuses n
             , Ninja.traps    = [ trap | trap <- Ninja.traps n
                                , Parity.allied player (Trap.user trap)
                                  || Invisible ∉ Trap.classes trap
                                  || revealed (Trap.user trap) ]
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

gameToJSON :: Player -> [Ninja] -> Game -> Value
gameToJSON player ninjas g = object
    [ "chakra"  .= Game.chakra g
    , "playing" .= Game.playing g
    , "victor"  .= Game.victor g
    , "ninjas"  .= censor player ninjas
    , "targets" .= targets
    ]
  where
    ns = toList ninjas
    targets = do
        n <- ns
        return do
            skill    <- Ninjas.skills n
            let targs = skillTargets skill $ Ninja.slot n
            return do
                nt   <- ns
                let t = Ninja.slot nt
                guard $ Requirement.targetable skill n nt && t ∈ targs
                return t

-- | All targets that a @Skill@ from a a specific 'Ninja' affects.
skillTargets :: Skill -> Slot -> [Slot]
skillTargets skill c = filter target Slot.all
  where
    ts = Skill.targets skill
    target t
      | Everyone ∈ ts                = True
      | not $ Parity.allied c t      = ts `intersects` harmTargets
      | ts `intersects` xAllyTargets = c /= t
      | ts `intersects` allyTargets  = True
      | c == t                       = not $ ts `intersects` harmTargets
      | otherwise                    = False
    harmTargets  = setFromList [Enemy, Enemies, REnemy, XEnemies]
    xAllyTargets = setFromList [XAlly, XAllies]
    allyTargets  = setFromList [Ally, Allies, RAlly]

ninjaToJSON :: Ninja -> Value
ninjaToJSON n = object
    [ "slot"      .= Ninja.slot n
    , "character" .= Character.format (Ninja.character n)
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
    , "lastSkill" .= Ninja.lastSkill n
    , "skills"    .= (usable <$> Ninjas.skills n)
    ]
  where
    usable skill = skill { Skill.require = fulfill $ Skill.require skill }
    fulfill req@HasI{}
      | Requirement.succeed req (Ninja.slot n) n = Usable
      | otherwise                                = Unusable
    fulfill x = x
