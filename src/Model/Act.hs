{-# LANGUAGE DeriveAnyClass #-}
module Model.Act
  ( Act(..)
  , fromChannel
  , illegal
  ) where

import ClassyPrelude

import           Data.Aeson (ToJSON)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import           Text.Read (Read(..))
import           Yesod.Core.Dispatch (PathPiece(..))

import qualified Model.Channel as Channel
import           Model.Channel (Channel)
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
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
               } deriving (Eq, Generic, ToJSON)
instance Show Act where
    show = show . fromAct
instance Read Act where
    readPrec = toAct <$> readPrec

-- A 'Player' attempts to control a 'Ninja' not on their team.
illegal :: Player -> Act -> Bool
illegal p = (fromEnum p /=) . (`rem` 2) . Slot.toInt . user

fromChannel :: Ninja -> Channel -> Act
fromChannel n chan = Act { user   = Ninja.slot n
                         , skill  = Right $ Channel.skill chan
                         , target = Channel.target chan
                         }

instance PathPiece Act where
    toPathPiece Act{..} = intercalate "," [ tshow user
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
