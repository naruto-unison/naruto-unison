{-# LANGUAGE DeriveAnyClass       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide          #-}

-- | Imported by "Model.Act" and "Model.GameInfo".
module Engine.ToJSON () where

import ClassyPrelude

import           Data.Aeson ((.=), ToJSON(..), object)
import qualified Data.List as List

import           Core.OrphanInstancesForArrowT ()
import           Core.Util ((∈), (∉), intersects)
import qualified Class.Parity as Parity
import           Model.Chakra (Chakras(..))
import           Model.Class (Class(..))
import qualified Model.Effect as Effect
import           Model.Face (Face(..))
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import           Model.Player (Player(..))
import qualified Model.Requirement as Requirement
import qualified Model.Skill as Skill
import qualified Model.Slot as Slot
import           Model.Slot (Slot)
import qualified Model.Status as Status
import qualified Model.Trap as Trap
import qualified Engine.Adjust as Adjust
import qualified Engine.Cooldown as Cooldown
import Model.Internal

instance ToJSON (Play a) where
    toJSON = const $ toJSON (Nothing :: Maybe ())
instance ToJSON Trigger where
    toJSON = toJSON . show
deriving instance Generic Barrier
deriving instance ToJSON Barrier
deriving instance Generic Bomb
deriving instance ToJSON Bomb
deriving instance Generic Category
deriving instance ToJSON Category
deriving instance Generic Chakras
deriving instance ToJSON Chakras
deriving instance Generic Channel
deriving instance ToJSON Channel
deriving instance Generic Channeling
deriving instance ToJSON Channeling
deriving instance Generic ChannelTag
deriving instance ToJSON ChannelTag
deriving instance Generic Character
deriving instance ToJSON Character
deriving instance Generic Class
deriving instance ToJSON Class
deriving instance Generic Context
deriving instance ToJSON Context
deriving instance Generic Copy
deriving instance ToJSON Copy
deriving instance Generic Copying
deriving instance ToJSON Copying
deriving instance Generic Defense
deriving instance ToJSON Defense
deriving instance Generic Direction
deriving instance ToJSON Direction
deriving instance Generic Player
deriving instance Generic Face
deriving instance ToJSON Face
deriving instance ToJSON Player
deriving instance Generic Requirement
deriving instance ToJSON Requirement
deriving instance Generic Skill
deriving instance ToJSON Skill
deriving instance Generic Status
deriving instance ToJSON Status
deriving instance Generic Target
deriving instance ToJSON Target
deriving instance Generic Trap
deriving instance ToJSON Trap
deriving instance Generic Trigger
deriving instance Generic Variant
deriving instance ToJSON Variant

instance ToJSON Effect where
    toJSON x = object
      [ "desc"    .= tshow x
      , "helpful" .= Effect.helpful x
      , "sticky"  .= Effect.sticky x
      , "trap"    .= False
      ]

instance ToJSON Ninja where
    toJSON n = object
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
        , "parrying"  .= Ninja.parrying n
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

instance ToJSON Game where
    toJSON g = object
        [ "chakra"  .= Game.chakra g
        , "ninjas"  .= Game.ninjas g
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
                skill <- Adjust.skills n
                return $ (List.intersect . skillTargets skill $ Ninja.slot n)
                         [Ninja.slot nt | nt <- ns
                                        , Requirement.targetable skill n nt
                                        ]

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
