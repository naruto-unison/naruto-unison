-- | Handles API routes and WebSockets related to gameplay.
module Handler.Play
    ( gameSocket
    , getPracticeActR, getPracticeQueueR, getPracticeWaitR
    ) where

import ClassyPrelude
import Yesod

import           Control.Monad.Loops (untilJust)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Control.Monad.ST as ST
import           Control.Monad.ST (RealWorld, ST)
import qualified Data.Cache as Cache
import qualified Data.Text as Text
import qualified System.Random.MWC as Random
import           UnliftIO.Concurrent (forkIO, threadDelay)
import qualified Yesod.Auth as Auth
import           Yesod.WebSockets (webSockets)

import           Util (duplic)
import qualified Application.App as App
import           Application.App (App, Handler)
import           Application.Model (EntityField(..), User(..))
import qualified Application.Settings as Settings
import qualified Handler.Play.Queue as Queue
import qualified Handler.Play.Rating as Rating
import qualified Handler.Play.Wrapper as Wrapper
import           Handler.Play.Wrapper (Wrapper(Wrapper))
import qualified Class.Parity as Parity
import qualified Class.Play as P
import           Class.Play (MonadGame)
import qualified Class.Random as R
import           Class.Random (MonadRandom)
import qualified Class.Sockets as Sockets
import           Class.Sockets (MonadSockets)
import qualified Game.Model.Act as Act
import           Game.Model.Act (Act)
import qualified Game.Model.Chakra as Chakra
import           Game.Model.Chakra (Chakras)
import           Game.Model.Character (Character)
import qualified Game.Model.Game as Game
import qualified Handler.Play.GameInfo as GameInfo
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Player as Player
import           Game.Model.Player (Player)
import qualified Game.Model.Slot as Slot
import qualified Game.Engine as Engine
import qualified Game.Characters as Characters

-- | If the difference in skill rating between two players exceeds this
-- threshold, they will not be matched together.
ratingThreshold :: Double
ratingThreshold = 1/0 -- i.e. infinity

bot :: User
bot = (App.newUser "Bot" Nothing $ ModifiedJulianDay 0)
    { userName       = "Bot"
    , userAvatar     = "/img/icon/bot.jpg"
    , userVerified   = True
    }

-- * HANDLERS

-- | Joins the practice-match queue with a given team. Requires authentication.
getPracticeQueueR :: [Text] -> Handler Value
getPracticeQueueR [a1, b1, c1, a2, b2, c2] =
    case fromList . zipWith Ninja.new Slot.all
         <$> traverse Characters.lookupName [c1, b1, a1, a2, b2, c2] of
    Nothing -> invalidArgs ["Unknown character(s)"]
    Just ninjas -> do
        who <- Auth.requireAuthId
        runDB $ update who [ UserTeam    =. Just [a1, b1, c1]
                          , UserPractice =. [a2, b2, c2]
                          ]
        liftIO Random.createSystemRandom >>= runReaderT do
            rand     <- ask
            game     <- runReaderT Game.newWithChakras rand
            practice <- getsYesod App.practice
            liftIO do
                Cache.purgeExpired practice -- TODO: Move to a recurring timer?
                Cache.insert practice who $ Wrapper game ninjas
            returnJson GameInfo { vsWho  = who
                                , vsUser = bot
                                , player = Player.A
                                , game   = game
                                , ninjas = ninjas
                                }
getPracticeQueueR _ = invalidArgs ["Wrong number of characters"]
 --zipWith Ninja.new Slot.all
-- | Wrapper for 'getPracticeActR' with no actions.
getPracticeWaitR :: Chakras -> Chakras -> Handler Value
getPracticeWaitR actChakra xChakra = getPracticeActR actChakra xChakra []

liftST :: ∀ m a. MonadIO m => ST RealWorld a -> m a
liftST = liftIO . ST.stToIO

-- | Handles a turn for a practice game. Requires authentication.
-- Practice games are not time-limited and use GET requests instead of sockets.
getPracticeActR :: Chakras -> Chakras -> [Act] -> Handler Value
getPracticeActR actChakra exchangeChakra actions = do
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
        res <- enact actChakra exchangeChakra actions
        case res of
          Left errorMsg -> invalidArgs [tshow errorMsg]
          Right ()      -> do
              game'A   <- Wrapper.freeze
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
                  Wrapper.toJSON Player.A <$> [game'A, game'B]

formTeam :: [Text] -> Maybe [Character]
formTeam team@[_,_,_]
  | duplic team = Nothing
  | otherwise   = traverse Characters.lookupName team
formTeam _ = Nothing

parseTeam :: [Text] -> Maybe (Queue.Section,  [Character])
parseTeam ("quick"  :team) = (Queue.Quick, )   <$> formTeam team
parseTeam ("private":team) = (Queue.Private, ) <$> formTeam team
parseTeam _                = Nothing

formEnact :: [Text] -> Maybe (Chakras, Chakras, [Act])
formEnact (_:_: _:_:_: _:_) = Nothing -- No more than 3 actions!
formEnact (actChakra:exchangeChakra:acts) = do
    actChakra'      <- fromPathPiece actChakra
    exchangeChakra' <- fromPathPiece exchangeChakra
    acts'           <- traverse fromPathPiece acts
    return (actChakra', exchangeChakra', acts')
formEnact _ = Nothing -- willywonka.gif

pingSocket :: ∀ m. MonadSockets m => ExceptT Queue.Failure m ()
pingSocket = do
    Sockets.send "ping"
    pong <- Sockets.receive
    when (pong == "cancel") $ throwE Queue.Canceled

makeGame :: ∀ m. (MonadRandom m, MonadIO m)
         => TChan Queue.Message
         -> Key User -> User -> [Character]
         -> Key User -> User -> [Character]
         -> m (MVar Wrapper, GameInfo)
makeGame queueWrite who user team vsWho vsUser vsTeam = do
    randPlayer <- R.player
    game       <- Game.newWithChakras
    let ninjas = zipWith Ninja.new Slot.all case randPlayer of
            Player.A -> team ++ vsTeam
            Player.B -> vsTeam ++ team
    mvar <- newEmptyMVar
    liftIO $ atomically do
        writeTChan queueWrite $
            Queue.Respond vsWho mvar
            GameInfo
                { GameInfo.vsWho  = who
                , GameInfo.vsUser = user
                , GameInfo.player = Player.opponent randPlayer
                , GameInfo.game   = game
                , GameInfo.ninjas = fromList ninjas
                }
        let info = GameInfo
                { GameInfo.vsWho = vsWho
                , GameInfo.vsUser = vsUser
                , GameInfo.player = randPlayer
                , GameInfo.game   = game
                , GameInfo.ninjas = fromList ninjas
                }
        return (mvar, info)

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
              | vsWho == who && not allowVsSelf -> throwE Queue.OpponentNotFound
            Just vs -> return vs
            Nothing -> throwE Queue.OpponentNotFound

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
handleFailures (Left msg)  = Nothing <$ Sockets.sendJson msg

-- | Sends messages through 'TChan's in 'App.App'. Requires authentication.
gameSocket :: Handler ()
gameSocket = webSockets do
    who        <- Auth.requireAuthId
    turnLength <- getsYesod $ Settings.turnLength . App.settings
    liftIO Random.createSystemRandom >>= runReaderT do
        (mvar, info) <- untilJust $ handleFailures =<< runExceptT do
            teamNames <- Text.split (=='/') <$> Sockets.receive
            (section, team) <- case parseTeam teamNames of
                Nothing   -> throwE Queue.InvalidTeam
                Just vals -> return vals

            liftHandler . runDB $
                update who [UserTeam =. Just (tailEx teamNames)]
            queue section team

        Sockets.sendJson info
        let player = GameInfo.player info
        liftST (Wrapper.fromInfo info) >>= runReaderT do
            when (player == Player.A) $ tryEnact turnLength player mvar
            gameEnd <- untilJust do
                wrapper <- takeMVar mvar
                if null . Game.victor $ Wrapper.game wrapper then do
                    Sockets.sendJson $ Wrapper.toJSON player wrapper
                    newWrapper <- ask
                    liftST $ Wrapper.replace wrapper newWrapper
                    tryEnact turnLength player mvar
                    game <- P.game
                    if null $ Game.victor game then
                        return Nothing
                    else do
                        liftHandler . runDB . void . forkIO .
                            Rating.update game player who $ GameInfo.vsWho info
                        Just <$> Wrapper.freeze
                else
                    return $ Just wrapper
            Sockets.sendJson $ Wrapper.toJSON player gameEnd

-- | Wraps @enact@ with error handling.
tryEnact :: ∀ m. (MonadGame m, MonadRandom m, MonadSockets m, MonadUnliftIO m)
         => Int -> Player -> MVar Wrapper -> m ()
tryEnact turnLength player mvar = do
    -- This is necessary because canceling Sockets.receive closes the socket
    -- connection, which means that a naive timeout will break the connection.
    lock <- newEmptyMVar
    forkIO do
        threadDelay turnLength
        void $ tryPutMVar lock Nothing

    forkIO do
        message <- Sockets.receive
        void . tryPutMVar lock $ Just message

    enactMessage <- (Text.split ('/' ==) <$>) <$> readMVar lock

    case enactMessage of
        Just ["forfeit"] -> do
            P.forfeit player
            conclude
        Just (formEnact -> Just (actChakra, exchangeChakra, actions)) -> do
            res <- enact actChakra exchangeChakra actions
            case res of
                Left errorMsg -> Sockets.send errorMsg
                Right ()      -> conclude
        _ -> do
            Engine.runTurn []
            conclude
  where
    conclude = do
        wrapper <- Wrapper.freeze
        Sockets.sendJson $ Wrapper.toJSON player wrapper
        putMVar mvar wrapper

-- | Processes a user's actions and passes them to 'Engine.run'.
enact :: ∀ m. (MonadGame m, MonadRandom m)
      => Chakras -> Chakras -> [Act] -> m (Either LByteString ())
enact actChakra exchangeChakra actions = do
    player     <- P.player
    gameChakra <- Parity.getOf player . Game.chakra <$> P.game
    let chakra  = gameChakra + exchangeChakra - actChakra
    if | not . null $ drop Slot.teamSize actions -> err "Too many actions"
       | duplic $ Act.user <$> actions           -> err "Duplicate actors"
       | randTotal < 0 || Chakra.lack chakra     -> err "Insufficient chakra"
       | any (Act.illegal player) actions        -> err "Character out of range"
       | otherwise                               -> do
            P.alter . Game.setChakra player $ chakra { Chakra.rand = randTotal }
            Right <$> Engine.runTurn actions
  where
    randTotal = Chakra.total actChakra - 5 * Chakra.total exchangeChakra
    err       = return . Left
