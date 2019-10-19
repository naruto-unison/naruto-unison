{-# LANGUAGE DeriveAnyClass #-}
-- | Handles API routes and WebSockets related to gameplay.
module Handler.Play
    ( gameSocket
    , getPracticeActR, getPracticeQueueR, getPracticeWaitR
    , Message(..)
    ) where

import ClassyPrelude
import Yesod

import           Control.Monad.Loops (untilJust)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.Cache as Cache
import qualified Data.Text as Text
import qualified System.Random.MWC as Random
import           UnliftIO.Concurrent (forkIO, threadDelay)
import qualified Yesod.Auth as Auth
import           Yesod.WebSockets (webSockets)

import           Application.App (App, Handler)
import qualified Application.App as App
import           Application.Model (EntityField(..), User(..))
import qualified Application.Model as Model
import qualified Application.Settings as Settings
import           Class.Hook (MonadHook)
import qualified Class.Parity as Parity
import           Class.Play (MonadGame)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Class.Random as R
import           Class.Sockets (MonadSockets)
import qualified Class.Sockets as Sockets
import qualified Game.Characters as Characters
import qualified Game.Engine as Engine
import           Game.Model.Act (Act)
import qualified Game.Model.Act as Act
import           Game.Model.Chakra (Chakras)
import qualified Game.Model.Chakra as Chakra
import           Game.Model.Character (Character)
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import qualified Game.Model.Slot as Slot
import           Handler.Client.Reward (Reward)
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Handler.Play.GameInfo as GameInfo
import           Handler.Play.Match (Outcome(..))
import qualified Handler.Play.Match as Match
import qualified Handler.Play.Queue as Queue
import qualified Handler.Play.Rating as Rating
import           Handler.Play.Turn (Turn)
import           Handler.Play.Wrapper (Wrapper(Wrapper))
import qualified Handler.Play.Wrapper as Wrapper
import qualified Mission
import           Util ((∉), duplic, liftST, whileM)

-- | A message sent through the websocket to the client.
-- This definition is xported so that @elm-bridge@ sends it over to the client.
data Message
    = Fail Queue.Failure
    | Info GameInfo
    | Ping
    | Play Turn
    | Rewards [Reward]
    deriving (Generic, ToJSON)

-- | If the difference in skill rating between two players exceeds this
-- threshold, they will not be matched together.
ratingThreshold :: Double
ratingThreshold = 1/0 -- i.e. infinity

bot :: User
bot = (Model.newUser "Bot" Nothing $ ModifiedJulianDay 0)
    { userName       = "Bot"
    , userAvatar     = "/img/icon/bot.jpg"
    , userVerified   = True
    }

sendClient :: ∀ m. MonadSockets m => Message -> m ()
sendClient = Sockets.sendJson

-- * HANDLERS

-- | Joins the practice-match queue with a given team. Requires authentication.
getPracticeQueueR :: [Text] -> Handler Value
getPracticeQueueR [a1, b1, c1, a2, b2, c2] =
    case zipWith Ninja.new Slot.all
         <$> traverse Characters.lookup [c1, b1, a1, a2, b2, c2] of
    Nothing -> invalidArgs ["Unknown character(s)"]
    Just ninjas -> do
        who      <- Auth.requireAuthId
        unlocked <- Mission.unlocked
        if any (∉ unlocked) [a1, b1, c1] then
            invalidArgs ["Locked character(s)"]
        else do
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
                    Cache.insert practice who . Wrapper mempty game $
                        fromList ninjas
                returnJson GameInfo { vsWho  = who
                                    , vsUser = bot
                                    , player = Player.A
                                    , game
                                    , ninjas
                                    }
getPracticeQueueR _ = invalidArgs ["Wrong number of characters"]
 --zipWith Ninja.new Slot.all
-- | Wrapper for 'getPracticeActR' with no actions.
getPracticeWaitR :: Chakras -> Chakras -> Handler Value
getPracticeWaitR actChakra xChakra = getPracticeActR actChakra xChakra []

-- | Handles a turn for a practice game. Requires authentication.
-- Practice games are not time-limited and use GET requests instead of sockets.
getPracticeActR :: Chakras -> Chakras -> [Act] -> Handler Value
getPracticeActR spend exchange acts = do
    who      <- Auth.requireAuthId
    practice <- getsYesod App.practice
    mGame    <- liftIO $ Cache.lookup practice who
    case mGame of
        Nothing   -> notFound
        Just game -> do
          random  <- liftIO Random.createSystemRandom
          wrapper <- liftST $ Wrapper.thaw game
          runReaderT (runReaderT (enactPractice who practice) wrapper) random
  where
    enactPractice who practice = do
        res <- enact $ Enact{spend, exchange, acts}
        case res of
          Left errorMsg -> invalidArgs [tshow errorMsg]
          Right ()      -> do
              game'A <- Wrapper.freeze
              P.alter \g -> g
                  { Game.chakra  = (fst $ Game.chakra g, 100)
                  , Game.playing = Player.B
                  }
              Engine.runTurn =<< Act.randoms
              game'B <- Wrapper.freeze
              liftIO if null . Game.victor $ Wrapper.game game'B then
                  Cache.insert practice who game'B
              else
                  Cache.delete practice who
              lift . returnJson $
                  Wrapper.toTurn Player.A <$> [game'A, game'B]

data Team = Team Queue.Section [Character]

parseTeam :: [Text] -> Maybe Team
parseTeam ("quick"  :team) = Team Queue.Quick   <$> formTeam team
parseTeam ("private":team) = Team Queue.Private <$> formTeam team
parseTeam _                = Nothing

formTeam :: [Text] -> Maybe [Character]
formTeam team@[_,_,_]
  | duplic team = Nothing
  | otherwise   = traverse Characters.lookup team
formTeam _ = Nothing

data Enact = Enact { spend    :: Chakras
                   , exchange :: Chakras
                   , acts     :: [Act]
                   }

formEnact :: [Text] -> Maybe Enact
formEnact (_:_: _:_:_: _:_) = Nothing -- No more than 3 actions!
formEnact (spend':exchange':acts') = Enact
                                     <$> fromPathPiece spend'
                                     <*> fromPathPiece exchange'
                                     <*> traverse fromPathPiece acts'
formEnact _ = Nothing -- willywonka.gif

pingSocket :: ∀ m. MonadSockets m => ExceptT Queue.Failure m ()
pingSocket = do
    sendClient Ping
    pong <- Sockets.receive
    when (pong == "cancel") $ throwE Queue.Canceled

makeGame :: ∀ m. (MonadRandom m, MonadIO m)
         => TChan Queue.Message
         -> Key User -> User -> [Character]
         -> Key User -> User -> [Character]
         -> m (MVar Wrapper, GameInfo)
makeGame queueWrite who user team vsWho vsUser vsTeam = do
    player <- R.player
    game   <- Game.newWithChakras
    let ninjas = fromList $ zipWith Ninja.new Slot.all case player of
            Player.A -> team ++ vsTeam
            Player.B -> vsTeam ++ team
    mvar <- newEmptyMVar
    liftIO $ atomically do
        writeTChan queueWrite $
            Queue.Respond vsWho mvar GameInfo { vsWho  = who
                                              , vsUser = user
                                              , player = Player.opponent player
                                              , game
                                              , ninjas
                                              }
        return (mvar, GameInfo { vsWho, vsUser, player, game, ninjas })

queue :: ∀ m.
        (MonadRandom m, MonadSockets m, MonadHandler m, App ~ HandlerSite m)
      => Queue.Section -> [Character]
      -> ExceptT Queue.Failure m (MVar Wrapper, GameInfo)
queue Queue.Quick team = do
    (who, user) <- Auth.requireAuthPair
    let rating = userRating user
    queueWrite  <- getsYesod App.queue
    allowVsSelf <- getsYesod $ Settings.allowVsSelf . App.settings
    userMVar    <- newEmptyMVar
    -- TODO use userMVar in some kind of periodic timeout check based on its
    -- UTCTime contents? or else just a semaphore
    queueRead <- liftIO $ atomically do
        writeTChan queueWrite $ Queue.Announce who user team userMVar
        dupTChan queueWrite
    untilJust do
      msg <- liftIO . atomically $ readTChan queueRead
      pingSocket
      case msg of
        Queue.Respond mWho mvar info
          | mWho == who -> return $ Just (mvar, info)
        Queue.Announce vsWho vsUser vsTeam vsMVar
          | abs (rating - userRating vsUser) > ratingThreshold ->
              return Nothing
          | vsWho == who && not allowVsSelf -> throwE Queue.AlreadyQueued
          | otherwise -> do
              time    <- liftIO getCurrentTime
              matched <- tryPutMVar vsMVar time
              if matched then
                  Just <$> makeGame queueWrite who user team vsWho vsUser vsTeam
              else
                  return Nothing
        _ -> return Nothing

queue Queue.Private team = do
    (who, user)         <- Auth.requireAuthPair
    allowVsSelf         <- getsYesod $ Settings.allowVsSelf . App.settings
    Entity vsWho vsUser <- do
        vsName <- Sockets.receive
        mVs    <- liftHandler . runDB $ selectFirst [UserName ==. vsName] []
        case mVs of
            Just (Entity vsWho _)
              | vsWho == who && not allowVsSelf -> throwE Queue.NotFound
            Just vs -> return vs
            Nothing -> throwE Queue.NotFound

    queueWrite <- getsYesod App.queue
    queueRead  <- liftIO $ atomically do
        writeTChan queueWrite $ Queue.Request who vsWho team
        dupTChan queueWrite
    untilJust do
      msg <- liftIO . atomically $ readTChan queueRead
      pingSocket
      case msg of
        Queue.Respond mWho mvar info
          | mWho == who && GameInfo.vsWho info == vsWho ->
              return $ Just (mvar, info)
        Queue.Request vsWho' requestWho vsTeam
          | vsWho' == vsWho && requestWho == who ->
              Just <$> makeGame queueWrite who user team vsWho vsUser vsTeam
        _ -> return Nothing

handleFailures :: ∀ m a. MonadSockets m => Either Queue.Failure a -> m (Maybe a)
handleFailures (Right val) = return $ Just val
handleFailures (Left msg)  = Nothing <$ sendClient (Fail msg)

-- | Sends messages through 'TChan's in 'App.App'. Requires authentication.
gameSocket :: Handler ()
gameSocket = webSockets do
    who        <- Auth.requireAuthId
    turnLength <- getsYesod $ Settings.turnLength . App.settings
    unlocked   <- liftHandler Mission.unlocked
    liftIO Random.createSystemRandom >>= runReaderT do
        (section, team, mvar, info) <- untilJust $
            handleFailures =<< runExceptT do
                teamNames <- Text.split (=='/') <$> Sockets.receive
                Team section team <- case parseTeam teamNames of
                    Nothing   -> throwE Queue.InvalidTeam
                    Just vals -> return vals

                let teamTail = tailEx teamNames

                when (any (∉ unlocked) teamTail) $ throwE Queue.Locked

                liftHandler . runDB $ update who [UserTeam =. Just teamTail]
                (mvar, info) <- queue section team
                return (section, teamTail, mvar, info)

        sendClient $ Info info
        let player = GameInfo.player info
        game <- liftST (Wrapper.fromInfo info) >>= runReaderT do
            when (player == Player.A) $ tryEnact player turnLength mvar
            whileM do
                wrapper <- takeMVar mvar
                if null . Game.victor $ Wrapper.game wrapper then do
                    sendClient . Play $ Wrapper.toTurn player wrapper
                    newWrapper <- ask
                    liftST $ Wrapper.replace wrapper newWrapper
                    tryEnact player turnLength mvar
                    game <- P.game
                    if null $ Game.victor game then
                        return True
                    else do
                        let match = Match.fromGame game player who $
                                    GameInfo.vsWho info
                        liftHandler . runDB . void . forkIO $
                            mapM_ Rating.update =<< Match.load match
                        return False
                else do
                    liftST . Wrapper.replace wrapper =<< ask
                    return False
            -- Because the STWrapper is confined to its ReaderT, it may safely
            -- be deconstructed as the final step.
            liftST . Wrapper.unsafeFreeze =<< ask
        sendClient . Play $ Wrapper.toTurn player game
        when (section == Queue.Quick) do -- eventually, || Queue.Ladder
            let outcome = Match.outcome (Wrapper.game game) player
            dnaReward <- liftHandler $ Mission.awardDNA Queue.Quick outcome
            sendClient $ Rewards dnaReward
            liftHandler do
                case outcome of
                    Victory -> Mission.processWin team
                    _       -> Mission.processDefeat team
                Mission.processUnpicked team
                traverse_ Mission.progress $ Wrapper.progress game

-- | Wraps @enact@ with error handling.
tryEnact :: ∀ m. ( MonadGame m
                 , MonadHook m
                 , MonadRandom m
                 , MonadSockets m
                 , MonadUnliftIO m
                 )
         => Player -> Int -> MVar Wrapper -> m ()
tryEnact player turnLength mvar = do
    -- This is necessary because interrupting Sockets.receive closes the socket
    -- connection, which means that a naive timeout will break the connection.
    -- Even if the turn is over and its output will be ignored, Sockets.receive
    -- must not be canceled.
    lock <- newEmptyMVar

    forkIO do
        threadDelay turnLength -- No message before the end of the turn.
        void $ tryPutMVar lock Nothing

    forkIO do
        message <- Sockets.receive -- Message received from client.
        void . tryPutMVar lock $ Just message

    enactMessage <- (Text.split ('/' ==) <$>) <$> readMVar lock

    case enactMessage of
        Just ["forfeit"] -> do
            P.forfeit player
            conclude
        Just (formEnact -> Just formedEnact) -> do
            res <- enact formedEnact
            case res of
                Left errorMsg -> Sockets.send errorMsg
                Right ()      -> conclude
        _ -> do
            Engine.runTurn []
            conclude
  where
    conclude = do
        wrapper <- Wrapper.freeze
        sendClient . Play $ Wrapper.toTurn player wrapper
        putMVar mvar wrapper

-- | Processes a user's actions and passes them to 'Engine.run'.
enact :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
      => Enact -> m (Either LByteString ())
enact Enact{spend, exchange, acts} = runExceptT do
    player     <- P.player
    gameChakra <- Parity.getOf player . Game.chakra <$> P.game
    let chakra  = gameChakra + exchange - spend
    when (length acts > Slot.teamSize)         $ throwE "Too many actions"
    when (duplic $ Act.user <$> acts)          $ throwE "Duplicate actors"
    when (randTotal < 0 || Chakra.lack chakra) $ throwE "Insufficient chakra"
    when (any (Act.illegal player) acts)       $ throwE "Character out of range"
    P.alter . Game.setChakra player $ chakra { Chakra.rand = randTotal }
    Engine.runTurn acts
  where
    randTotal = Chakra.total spend - 5 * Chakra.total exchange
