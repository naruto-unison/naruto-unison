-- | Handles API routes and WebSockets related to gameplay.
module Handler.Play
    ( getPracticeActR, getPracticeQueueR, getPracticeWaitR
    , gameSocket
    ) where

import ClassyPrelude
import Yesod

import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.Cache as Cache
import           System.Random.MWC (createSystemRandom)
import qualified Yesod.Auth as Auth

import qualified Application.App as App
import           Application.Model (EntityField(..))
import qualified Application.Model.Unlocked as Unlocked
import qualified Application.Model.User as User
import qualified Game.AI as AI
import qualified Game.Characters as Characters
import           Game.Model.Chakras (Chakras)
import qualified Game.Model.Character as Character
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as N
import qualified Game.Model.Player as Player
import qualified Game.Model.Slot as Slot
import           Handler.Play.Act (Act)
import           Handler.Play.Game (Enact(..), enact, gameSocket)
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Handler.Play.GameInfo
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
        Just chars -> return $ fromList $ zipWith N.new Slot.all chars
        Nothing    -> invalidArgs ["Character(s) not found"]

    who      <- Auth.requireAuthId
    unlocked <- Mission.unlocked
    when (any (∉ unlocked) [a1, b1, c1])
        $ invalidArgs ["Character(s) locked"]

    runDB $ update who [ UserTeam     =. Just [a1, b1, c1]
                       , UserPractice =. [a2, b2, c2]
                       ]

    let playerReanimated = getDna [a1, b1, c1] unlocked
        botReanimated    = getDna [a2, b2, c2]
                         $ Character.ident <$> Characters.list

    practice <- getsYesod App.practice
    game <- liftIO do
        game <- createSystemRandom >>= runReaderT Game.newWithChakras
        let game' = game { Game.dna = (playerReanimated, botReanimated) }
        -- TODO: Move to a recurring timer?
        Cache.purgeExpired practice
        Cache.insert practice who $ Wrapper.new game' ninjas
        return game'


    returnJson GameInfo { vsUser = Entity who bot
                        , player = Player.A
                        , war    = Nothing
                        , game
                        , ninjas
                        , snapshots = mempty
                        }
  where
    hasDuplicates a b c = a == b || a == c || b == c
    getDna team unlocked = fromList
                         . filter (\x -> Unlocked.reanimated x && x ∉ team)
                         $ toList unlocked
    bot = (User.new "Bot" Nothing $ ModifiedJulianDay 0)
          { User.name     = "Bot"
          , User.avatar   = "/img/icon/bot.jpg"
          , User.verified = True
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
    rand     <- liftIO createSystemRandom
    wrapper  <- Wrapper.thaw game

    flip runReaderT rand $ flip runReaderT wrapper do
        res <- runExceptT $ enact Enact{spend, exchange, actions}
        forM_ (leftToMaybe res) \errorMsg -> invalidArgs [errorMsg]
        game'A <- Wrapper.freeze =<< ask
        AI.runTurn
        game'B <- Wrapper.freeze =<< ask
        liftIO
            if Game.inProgress $ Wrapper.game game'B then
                Cache.insert practice who game'B
            else
                Cache.delete practice who
        returnJson $ Wrapper.toTurn Player.A <$> [game'A, game'B]
