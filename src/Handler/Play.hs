{-# LANGUAGE DeriveAnyClass #-}

-- | Handles API routes and WebSockets related to gameplay.
module Handler.Play
    ( getPracticeActR, getPracticeQueueR, getPracticeWaitR
    , Message(..)
    , gameSocket
    ) where

import ClassyPrelude
import Yesod

import           Control.Monad.Logger
import           Control.Monad.Loops (untilJust, whileM)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.Aeson (ToJSON, toEncoding)
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Cache as Cache
import qualified Data.Text as Text
import           Network.WebSockets (ConnectionException(..))
import qualified System.Random.MWC as Random
import           UnliftIO.Concurrent (forkIO, threadDelay)
import qualified Yesod.Auth as Auth
import           Yesod.WebSockets (webSockets)

import           Application.App (App, Handler, liftDB)
import qualified Application.App as App
import           Application.Model (EntityField(..), User(..))
import qualified Application.Model as Model
import           Application.Settings (Settings)
import qualified Application.Settings as Settings
import           Class.Hook (MonadHook)
import qualified Class.Parity as Parity
import           Class.Play (MonadGame)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Class.Random as R
import           Class.Sockets (MonadSockets)
import qualified Class.Sockets as Sockets
import qualified Game.AI as AI
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
import           Handler.Client.Reward (Reward(Reward))
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Handler.Play.GameInfo as GameInfo
import           Handler.Play.Match (Outcome(..))
import qualified Handler.Play.Match as Match
import qualified Handler.Play.Queue as Queue
import qualified Handler.Play.Rating as Rating
import           Handler.Play.Turn (Turn)
import qualified Handler.Play.War as War
import           Handler.Play.Wrapper (Wrapper(Wrapper))
import qualified Handler.Play.Wrapper as Wrapper
import qualified Mission
import           Util ((∉), duplic, liftST)

-- | If the difference in skill rating between two players exceeds this
-- threshold, they will not be matched together.
ratingThreshold :: Double
ratingThreshold = 1/0 -- i.e. infinity

-- | A message sent through the websocket to the client.
-- This definition is exported so that @elm-bridge@ sends it over to the client.
data Message
    = Fail Queue.Failure
    | Info GameInfo
    | Ping
    | Play Turn
    | Rewards [Reward]
    deriving (Generic, ToJSON)

sendClient :: ∀ m. MonadSockets m => Message -> m ()
sendClient x = Sockets.send . Encoding.encodingToLazyByteString $ toEncoding x

-- * INPUT PARSING

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

-- * HANDLERS

-- | Joins the practice-match queue with a given team. Requires authentication.
getPracticeQueueR :: [Text] -> Handler Value
getPracticeQueueR [a1, b1, c1, a2, b2, c2] = do
    when (duplic [a1, b1, c1] || duplic [a2, b2, c2]) $
        invalidArgs ["Duplicate characters"]

    ninjas   <- case traverse Characters.lookup [c1, b1, a1, a2, b2, c2] of
        Nothing    -> invalidArgs ["Character(s) not found"]
        Just chars -> return $ zipWith Ninja.new Slot.all chars

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
            Cache.insert practice who . Wrapper mempty game $
                fromList ninjas

        returnJson GameInfo { vsWho  = who
                            , vsUser = bot
                            , player = Player.A
                            , war    = Nothing
                            , game
                            , ninjas
                            }
  where
    bot = (Model.newUser "Bot" Nothing $ ModifiedJulianDay 0)
          { userName     = "Bot"
          , userAvatar   = "/img/icon/bot.jpg"
          , userVerified = True
          }

getPracticeQueueR _ = invalidArgs ["Wrong number of characters"]

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
    game     <- maybe notFound return mGame
    rand     <- liftIO Random.createSystemRandom
    wrapper  <- liftST $ Wrapper.thaw game

    flip runReaderT rand $ flip runReaderT wrapper do
        res  <- enact $ Enact{spend, exchange, acts}
        case res of
          Left errorMsg -> invalidArgs [tshow errorMsg]
          Right ()      -> do
              game'A <- Wrapper.freeze
              P.alter \g -> g { Game.chakra  = (fst $ Game.chakra g, 100)
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

data Team = Team Queue.Section [Character]

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
    liftIO do
        let ninjas = fromList $ zipWith Ninja.new Slot.all case player of
                Player.A -> team ++ vsTeam
                Player.B -> vsTeam ++ team
        war  <- War.match team vsTeam <$> War.today
        mvar <- newEmptyMVar
        atomically . writeTChan queueWrite $
            Queue.Respond vsWho mvar GameInfo { vsWho  = who
                                              , vsUser = user
                                              , player = Player.opponent player
                                              , war    = War.opponent <$> war
                                              , game
                                              , ninjas
                                              }
        return (mvar, GameInfo { vsWho, vsUser, player, game, ninjas, war })

queue :: ∀ m. ( MonadHandler m, App ~ HandlerSite m
              , MonadRandom m
              , MonadSockets m
              ) => Queue.Section -> [Character]
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
        mVs    <- liftDB $ selectFirst [UserName ==. vsName] []
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
gameSocket :: ∀ m. ( MonadHandler m, App ~ HandlerSite m
                   , MonadUnliftIO m
                   , MonadRandom m
                   ) => m ()
gameSocket = webSockets do
    who      <- Auth.requireAuthId
    settings <- getsYesod App.settings
    unlocked <- liftHandler Mission.unlocked

    (section, team, (mvar, info)) <- untilJust $
        handleFailures =<< runExceptT do
            teamNames <- Text.split (=='/') <$> Sockets.receive
            Team section team <- case parseTeam teamNames of
                Nothing   -> throwE Queue.InvalidTeam
                Just vals -> return vals

            let teamTail = tailEx teamNames
            when (any (∉ unlocked) teamTail) $ throwE Queue.Locked
            liftDB $ update who [UserTeam =. Just teamTail]

            queued <- queue section team
            return (section, teamTail, queued)

    sendClient $ Info info
    let player = GameInfo.player info

    game <- liftST (Wrapper.fromInfo info) >>= runReaderT do
        when (player == Player.A) $ tryEnact settings player mvar

        whileM (Game.inProgress <$> P.game) do
            wrapper <- takeMVar mvar

            if Game.inProgress $ Wrapper.game wrapper then do
                sendClient . Play $ Wrapper.toTurn player wrapper
                liftST . Wrapper.replace wrapper =<< ask
                tryEnact settings player mvar
                game <- P.game

                unless (Game.inProgress game) . liftDB . void $ forkIO do
                    match <- Match.load . Match.fromGame game player who $
                              GameInfo.vsWho info
                    mapM_ Rating.update match
            else
                liftST . Wrapper.replace wrapper =<< ask

        -- Because the STWrapper is confined to its ReaderT, it may safely
        -- be deconstructed as the final step.
        liftST . Wrapper.unsafeFreeze =<< ask

    sendClient . Play $ Wrapper.toTurn player game

    when (section == Queue.Quick) do -- eventually, || Queue.Ladder
        let outcome = Match.outcome (Wrapper.game game) player
        if outcome == Defeat && Game.forfeit (Wrapper.game game) then
            sendClient $ Rewards [Reward "Forfeit" 0]
        else do
            dnaReward <- liftHandler .
                Mission.awardDNA Queue.Quick outcome $ GameInfo.war info
            sendClient $ Rewards dnaReward

        liftHandler do
            case outcome of
                Victory -> Mission.processWin team
                _       -> Mission.processDefeat team
            Mission.processUnpicked team
            traverse_ Mission.progress $ Wrapper.progress game

data ClientResponse
    = Received [Text]
    | TimedOut
    | SocketException ConnectionException
    deriving (Eq, Show)

-- | Wraps @enact@ with error handling.
tryEnact :: ∀ m. ( MonadGame m
                 , MonadHook m
                 , MonadRandom m
                 , MonadSockets m
                 , MonadUnliftIO m
                 , MonadLogger m
                 ) => Settings -> Player -> MVar Wrapper -> m ()
tryEnact settings player mvar = do
    -- This is necessary because interrupting Sockets.receive closes the socket
    -- connection, which means that a naive timeout will break the connection.
    -- Even if the turn is over and its output will be ignored, Sockets.receive
    -- must not be canceled.
    lock <- newEmptyMVar

    forkIO do
        threadDelay $ Settings.turnLength settings
        void $ tryPutMVar lock TimedOut

    forkIO do
        tryMessage <- try Sockets.receive
        void $ tryPutMVar lock case tryMessage of
            Right message -> Received $ Text.split (== '/') message
            Left err      -> SocketException err

    enactMessage <- readMVar lock

    case enactMessage of
        Received ["forfeit"] ->
            Engine.forfeit player

        Received (formEnact -> Just formedEnact) -> do
            Engine.resetInactive player
            res <- enact formedEnact
            case res of
                Right ()      -> return ()
                Left errorMsg -> do
                    logErrorN $ "Client error: "
                                ++ toStrict (decodeUtf8 errorMsg)
                    Sockets.send errorMsg

        Received malformed ->
            logErrorN $ "Malformed client input: " ++ intercalate "/" malformed

        TimedOut ->
            Engine.skipTurn (Settings.forfeitAfterSkips settings) player

        SocketException ConnectionClosed -> do
            logErrorN "Socket closed"
            Engine.forfeit player

        SocketException (CloseRequest code why) -> do
            logErrorN $ "Socket closed: " ++ tshow code ++ " "
                        ++ toStrict (decodeUtf8 why)
            Engine.forfeit player

        SocketException (ParseException malformed) ->
            logErrorN $ "Malformed client input: " ++ pack malformed

        SocketException (UnicodeException malformed) ->
            logErrorN $ "Malformed client input: " ++ pack malformed

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
    mapM_ throwE $ illegal player chakra
    P.alter . Game.setChakra player $ chakra { Chakra.rand = randTotal }
    Engine.runTurn acts
  where
    randTotal = Chakra.total spend - 5 * Chakra.total exchange
    illegal player chakra
      | length acts > Slot.teamSize         = Just "Too many actions"
      | duplic $ Act.user <$> acts          = Just "Duplicate actors"
      | randTotal < 0 || Chakra.lack chakra = Just "Insufficient chakra"
      | any (Act.illegal player) acts       = Just "Character out of range"
      | otherwise                           = Nothing
