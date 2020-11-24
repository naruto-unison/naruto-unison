{-# LANGUAGE DeriveAnyClass #-}

module Handler.Play.Act
  ( Act(..)
  , toContext
  ) where

import ClassyPrelude

import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.Aeson (ToJSON)
import qualified Data.Attoparsec.Text as Parser
import           Yesod.Core.Dispatch (PathPiece(..))

import           Class.Play (MonadGame)
import qualified Class.Play as P
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Util (hushedParse)

-- | A single action of a 'Ninja'.
data Act = Act { user   :: Slot
               -- ^ User index in 'Model.Game.ninjas' (0-5)
               , skill  :: Int
               -- ^ Skill by index in 'Character.skills' of 'Ninja.character' (0-3)
               , target :: Slot
               -- ^ Target index in 'Model.Game.ninjas' (0-5)
               } deriving (Eq, Show, Read, Generic, ToJSON)

instance PathPiece Act where
    toPathPiece Act{user, skill, target} =
        intercalate "," [ tshow user
                        , tshow skill
                        , tshow target
                        ]
    fromPathPiece =
        hushedParse $ Act
            <$> Slot.parse
            <*> (Parser.char ',' >> Parser.decimal)
            <*> (Parser.char ',' >> Slot.parse)
            <* Parser.endOfInput

toContext :: ∀ m. MonadGame m => Act -> ExceptT LByteString m Context
toContext Act { user, skill, target } = do
    nUser <- P.ninja user
    case Ninjas.getSkill skill nUser of
        Nothing -> throwE "Invalid skill"
        Just sk -> return Context { new = True
                                  , user
                                  , skill = sk
                                  , target
                                  , continues = False
                                  }
