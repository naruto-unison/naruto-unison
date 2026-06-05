module Handler.Play.Act
  ( Act(..)
  , toContext
  ) where

import ClassyPrelude

import           Control.Monad.Error.Class (MonadError)
import           Data.Aeson (ToJSON)
import           Yesod.Core.Dispatch (PathPiece(..))

import           Class.Parse (Parse(..))
import qualified Class.Parse as Parse
import           Class.Play (MonadGame)
import qualified Class.Play as P
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import           Game.Model.Slot (Slot)
import           Util (tryFromJust, rightToMaybe)

-- | A single action of a 'Ninja'.
data Act = Act
    { user   :: Slot
    -- ^ User index in 'Model.Game.ninjas' (0-5)
    , skill  :: Int
    -- ^ Skill by index in 'Character.skills' of 'Ninja.character' (0-3)
    , target :: Slot
    -- ^ Target index in 'Model.Game.ninjas' (0-5)
    } deriving (Eq, Show, Read, Generic)

instance ToJSON Act

instance Parse Act where
    parser = Act
        <$> Parse.parser @Slot
        <*> (Parse.char ',' >> Parse.parser @Int)
        <*> (Parse.char ',' >> Parse.parser @Slot)

instance PathPiece Act where
    toPathPiece (Act user skill target) = intercalate ","
        [ tshow user, tshow skill, tshow target ]
    fromPathPiece piece = rightToMaybe $ Parse.parseOnly piece

toContext :: ∀ m. (MonadGame m, MonadError Text m) => Act -> m Context
toContext (Act user skill target) = do
    nUser <- P.ninja user
    sk    <- tryFromJust "Invalid skill" $ Ninjas.getSkill skill nUser
    return $ createContext sk
  where
    createContext sk = Context
        { new       = True
        , user
        , skill     = sk
        , target
        , continues = False
        }
