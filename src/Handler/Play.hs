-- | Handles API routes and WebSockets related to gameplay.
module Handler.Play
    ( getPracticeActR, getPracticeQueueR, getPracticeWaitR
    , gameSocket
    ) where

import ClassyPrelude
import Yesod

import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.Cache as Cache
import qualified System.Random.MWC as Random
import qualified Yesod.Auth as Auth

import qualified Application.App as App
import           Application.Model (EntityField(..), User(..))
import qualified Application.Model as Model
import qualified Class.Play as P
import qualified Game.AI as AI
import qualified Game.Characters as Characters
import           Game.Model.Chakras (Chakras(Chakras))
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as N
import qualified Game.Model.Player as Player
import qualified Game.Model.Slot as Slot
import           Handler.Play.Act (Act)
import           Handler.Play.Game (Enact(..), enact, gameSocket)
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Handler.Play.GameInfo
import           Handler.Play.Wrapper (Wrapper(Wrapper))
import qualified Handler.Play.Wrapper as Wrapper
import qualified Mission
import           Util ((∉), leftToMaybe, fromMaybeM)

-- | Joins the practice-match queue with a given team. Requires authentication.
getPracticeQueueR :: [Text] -> App.Handler Value
getPracticeQueueR [a1, b1, c1, a2, b2, c2]
  | hasDuplicates a1 b1 c1 || hasDuplicates a2 b2 c2 =
    invalidArgs ["Duplicate characters"]
  | otherwise = do
    ninjas <- case mapM Characters.lookup [c1, b1, a1, a2, b2, c2] of
        Just chars -> return $ zipWith N.new Slot.all chars
        Nothing    -> invalidArgs ["Character(s) not found"]

    who      <- Auth.requireAuthId
    unlocked <- Mission.unlocked
    when (any (∉ unlocked) [a1, b1, c1]) $ invalidArgs ["Character(s) locked"]

    runDB $ update who [ UserTeam     =. Just [a1, b1, c1]
                       , UserPractice =. [a2, b2, c2]
                       ]

    liftIO Random.createSystemRandom >>= runReaderT do
        rand     <- ask
        game     <- runReaderT Game.newWithChakras rand
        practice <- getsYesod App.practice

        liftIO do
            -- TODO: Move to a recurring timer?
            Cache.purgeExpired practice
            Cache.insert practice who . Wrapper mempty game $ fromList ninjas

        returnJson GameInfo { vsWho  = who
                            , vsUser = bot
                            , player = Player.A
                            , war    = Nothing
                            , game
                            , ninjas
                            }
  where
    hasDuplicates a b c = a == b || a == c || b == c
    bot = (Model.newUser "Bot" Nothing $ ModifiedJulianDay 0)
          { userName     = "Bot"
          , userAvatar   = "/img/icon/bot.jpg"
          , userVerified = True
          }

getPracticeQueueR _ = invalidArgs ["Wrong number of characters"]

-- | Wrapper for 'getPracticeActR' with no actions.
getPracticeWaitR :: Chakras -> Chakras -> App.Handler Value
getPracticeWaitR actChakra xChakra = getPracticeActR actChakra xChakra []

-- | Handles a turn for a practice game. Requires authentication.
-- Practice games are not time-limited and use GET requests instead of sockets.
getPracticeActR :: Chakras -> Chakras -> [Act] -> App.Handler Value
getPracticeActR spend exchange actions = do
    who      <- Auth.requireAuthId
    practice <- getsYesod App.practice
    game     <- fromMaybeM notFound $ liftIO $ Cache.lookup practice who
    rand     <- liftIO Random.createSystemRandom
    wrapper  <- Wrapper.thaw game

    flip runReaderT rand $ flip runReaderT wrapper do
        res <- runExceptT $ enact Enact{spend, exchange, actions}
        forM_ (leftToMaybe res) \errorMsg -> invalidArgs [errorMsg]
        game'A <- Wrapper.freeze
        P.alter \g -> g { Game.chakra  = (fst $ Game.chakra g, baseChakra)
                        , Game.playing = Player.B
                        }
        AI.runTurn
        game'B <- Wrapper.freeze
        liftIO
            if Game.inProgress $ Wrapper.game game'B then
                Cache.insert practice who game'B
            else
                Cache.delete practice who
        returnJson $ Wrapper.toTurn Player.A <$> [game'A, game'B]
  where
    baseChakra = Chakras 100 100 100 100 100
