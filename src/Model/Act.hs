{-# LANGUAGE DeriveAnyClass #-}

module Model.Act
  ( Act(..)
  , fromChannel
  , illegal
  , randoms
  ) where

import ClassyPrelude

import           Data.Aeson (ToJSON)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import           Text.Read (Read(..))
import           Yesod.Core.Dispatch (PathPiece(..))

import qualified Class.Parity as Parity
import qualified Class.Random as R
import           Class.Random (MonadRandom)
import qualified Model.Channel as Channel
import           Model.Channel (Channel)
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Player as Player
import           Model.Player (Player)
import qualified Model.Slot as Slot
import           Model.Slot (Slot)
import qualified Model.Skill as Skill
import           Model.Skill (Skill)

-- | A single action of a 'Ninja'.
data Act = Act { user   :: Slot
               -- ^ User index in 'Model.Game.ninjas' (0-5)
               , skill  :: Either Int Skill
               -- ^ Skill by index in 'Character.skills' of 'Ninja.character' (0-3)
               , target :: Slot
               -- ^ Target index in 'Model.Game.ninjas' (0-5)
               } deriving (Generic, ToJSON)
instance Eq Act where
    x == y = user x == user y && target x == target y && skill x `eq` skill y
      where
        eq (Left x')  (Left y')  = x' == y'
        eq (Right x') (Right y') = Skill.name x' == Skill.name y'
        eq _         _           = False
instance Show Act where
    showsPrec i = showsPrec i . fromAct
instance Read Act where
    readPrec = toAct <$> readPrec

random :: ∀ m. MonadRandom m => Slot -> m Act
random user =
    Act user <$> (Left <$> R.random 0 (Ninja.skillSize - 1)) <*> Slot.random

randoms :: ∀ m. MonadRandom m => m [Act]
randoms = traverse random $ Slot.allies Player.B

-- A 'Player' attempts to control a 'Ninja' not on their team.
illegal :: Player -> Act -> Bool
illegal p a = not . Parity.allied p $ user a

fromChannel :: Ninja -> Channel -> Act
fromChannel n chan = Act { user   = Ninja.slot n
                         , skill  = Right $ Channel.skill chan
                         , target = Channel.target chan
                         }

instance PathPiece Act where
    toPathPiece Act{user, skill, target} =
        intercalate "," [ tshow user
                        , either tshow Skill.name skill
                        , tshow target
                        ]
    fromPathPiece raw   = case pieces of
        [c, s, t] -> case makeAct c s t of
                        Right act -> Just act
                        Left  _   -> Nothing
        _         -> Nothing
      where
        pieces        = Text.splitOn "," raw
        makeAct c s t = [Act{..} | (user,_)    <- Slot.read c
                                 , (skill, _)  <- first Left <$> Read.decimal s
                                 , (target, _) <- Slot.read t
                                 ]

data Act' = Act' { user'   :: Slot
                 , skill'  :: Int
                 , target' :: Slot
                 } deriving (Show, Read)
fromAct :: Act -> Act'
fromAct (Act u (Left s) t)  = Act' u s t
fromAct (Act u (Right _) t) = Act' u (-1) t
toAct :: Act' -> Act
toAct (Act' u s t) = Act u (Left s) t
