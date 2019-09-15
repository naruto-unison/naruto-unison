-- | Handles API routes and WebSockets related to gameplay.
module Handler.Play
    ( gameSocket
    , getPracticeActR, getPracticeQueueR, getPracticeWaitR
    ) where

import ClassyPrelude hiding (Handler)
import Yesod

import           Control.Monad.Loops (untilJust)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Control.Monad.ST as ST
import           Control.Monad.ST (RealWorld, ST)
import qualified Data.Cache as Cache
import qualified Data.Text as Text
import qualified System.Random.MWC as Random
import qualified UnliftIO.Timeout as Timeout
import qualified Yesod.Auth as Auth

import qualified Core.App as App
import           Core.App (App, Handler)
import           Core.Fields (Privilege(..))
import qualified Core.Queue as Queue
import           Core.Model (EntityField(..), User(..))
import qualified Core.Wrapper as Wrapper
import           Core.Wrapper (Wrapper(Wrapper))
import           Core.Util (duplic)
import qualified Class.Parity as Parity
import qualified Class.Play as P
import           Class.Play (MonadGame)
import qualified Class.Random as R
import           Class.Random (MonadRandom)
import qualified Class.Sockets as Sockets
import           Class.Sockets (MonadSockets, SocketsT)
import qualified Model.Act as Act
import           Model.Act (Act)
import qualified Model.Chakra as Chakra
import           Model.Chakra (Chakras)
import           Model.Character (Character)
import qualified Model.Game as Game
import qualified Model.GameInfo as GameInfo
import           Model.GameInfo (GameInfo(GameInfo))
import qualified Model.Ninja as Ninja
import qualified Model.Player as Player
import           Model.Player (Player)
import qualified Model.Slot as Slot
import qualified Engine.Turn as Turn
import qualified Characters

bot :: User
bot = User
    { userIdent      = ""
    , userPassword   = Nothing
    , userName       = "Bot"
    , userAvatar     = "/img/icon/bot.jpg"
    , userVerkey     = Nothing
    , userVerified   = True
    , userPrivilege  = Normal
    , userBackground = Nothing
    , userXp         = 0
    , userWins       = 0
    , userLosses     = 0
    , userStreak     = 0
    , userClan       = Nothing
    , userTeam       = Nothing
    , userPractice   = []
    , userMuted      = False
    , userCondense   = False
    }

-- * HANDLERS

-- | Joins the practice-match queue with a given team. Requires authentication.
getPracticeQueueR :: [Text] -> Handler Value
getPracticeQueueR [a1, b1, c1, a2, b2, c2] =
    case fromList . zipWith Ninja.new Slot.all <$>
         traverse Characters.lookupName [c1, b1, a1, a2, b2, c2] of
    Nothing -> invalidArgs ["Unknown character(s)"]
    Just ninjas -> do
        who <- Auth.requireAuthId
        runDB $ update who [ UserTeam    =. Just [a1, b1, c1]
                          , UserPractice =. [a2, b2, c2]
                          ]
        liftIO Random.createSystemRandom >>= runReaderT do
            game <- runReaderT Game.newWithChakras =<< ask
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

-- | Handles a turn for a practice game. Practice games are not limited by time
-- and use GET requests instead of WebSockets.
getPracticeActR :: Chakras -> Chakras -> [Act] -> Handler Value
getPracticeActR actChakra exchangeChakra actions = do
    who      <- Auth.requireAuthId -- !FAILS!
    practice <- getsYesod App.practice
    mGame    <- liftIO $ Cache.lookup practice who -- !FAILS
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
          Left errorMsg -> invalidArgs [tshow errorMsg] -- !FAILS!
          Right ()      -> do
              game'A   <- Wrapper.freeze
              P.alter \g -> g
                  { Game.chakra  = (fst $ Game.chakra g, 100)
                  , Game.playing = Player.B
                  }
              Turn.run =<< Act.randoms
              game'B <- Wrapper.freeze
              liftIO if (null . Game.victor $ Wrapper.game game'B) then
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
formEnact (_:_: _:_:_:_:_) = Nothing -- No more than 3 actions!
formEnact (actChakra:exchangeChakra:acts) = do
    actChakra'      <- fromPathPiece actChakra
    exchangeChakra' <- fromPathPiece exchangeChakra
    acts'           <- traverse fromPathPiece acts
    return (actChakra', exchangeChakra', acts')
formEnact _ = Nothing -- willywonka.gif

queue :: ∀ m.
        (MonadRandom m, MonadSockets m, MonadHandler m, App ~ HandlerSite m)
      => Queue.Section -> [Character]
      -> ExceptT Queue.Failure m (GameInfo, TBQueue Wrapper, TBQueue Wrapper)
queue Queue.Quick team = do
    (who, user) <- Auth.requireAuthPair
    randPlayer  <- R.player
    queueWrite  <- getsYesod App.queue
    userMVar <- newEmptyMVar
    -- TODO use userMVar in some kind of periodic timeout check based on its
    -- UTCTime contents? or else just a semaphore
    queueRead <- (liftIO . atomically) do
        writeTChan queueWrite $ Queue.Announce who user team userMVar
        dupTChan queueWrite
    untilJust do
      msg <- liftIO . atomically $ readTChan queueRead
      Sockets.send "ping"
      pong <- Sockets.receive
      when (pong == "cancel") $ throwE Queue.Canceled
      case msg of
        Queue.Respond mWho writer reader info
          | mWho == who -> return $ Just (info, writer, reader)
        Queue.Announce vsWho vsUser vsTeam vsMVar -> do
            time <- liftIO getCurrentTime
            matched <- tryPutMVar vsMVar time
            if not matched then
                return Nothing
            else do
                game <- Game.newWithChakras
                let ninjas = zipWith Ninja.new Slot.all case randPlayer of
                        Player.A -> team ++ vsTeam
                        Player.B -> vsTeam ++ team
                liftIO $ atomically do
                    writer <- newTBQueue 8
                    reader <- newTBQueue 8
                    writeTChan queueWrite $
                        Queue.Respond vsWho reader writer
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
                    return $ Just (info, writer, reader)
        _ -> return Nothing

queue Queue.Private team = do
    (who, user)         <- Auth.requireAuthPair
    Entity vsWho vsUser <- do
        vsName <- Sockets.receive
        mVs    <- liftHandler . runDB $ selectFirst [UserName ==. vsName] []
        case mVs of
            Just vs -> return vs
            Nothing -> throwE Queue.OpponentNotFound

    queueWrite <- getsYesod App.queue
    randPlayer <- R.player
    queueRead  <- (liftIO . atomically) do
        writeTChan queueWrite $ Queue.Request who vsWho team
        dupTChan queueWrite
    untilJust do
      msg <- liftIO . atomically $ readTChan queueRead
      Sockets.send "ping"
      pong <- Sockets.receive
      when (pong == "cancel") $ throwE Queue.Canceled
      case msg of
        Queue.Respond mWho writer reader info
          | mWho == who && GameInfo.vsWho info == vsWho ->
              return $ Just (info, writer, reader)
        Queue.Request vsWho' requestWho vsTeam
          | vsWho' == vsWho && requestWho == who -> do
              game <- Game.newWithChakras
              let ninjas = zipWith Ninja.new Slot.all case randPlayer of
                      Player.A -> team ++ vsTeam
                      Player.B -> vsTeam ++ team
              liftIO $ atomically do
                  writer <- newTBQueue 8
                  reader <- newTBQueue 8
                  writeTChan queueWrite $
                      Queue.Respond vsWho reader writer
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
                  return $ Just (info, writer, reader)
        _ -> return Nothing

handleFailures :: ∀ m a. MonadSockets m => Either Queue.Failure a -> m (Maybe a)
handleFailures (Right val)              = return $ Just val
handleFailures (Left Queue.Canceled)    = return Nothing
handleFailures (Left Queue.InvalidTeam) = do
    Sockets.send "Invalid team"
    return Nothing
handleFailures (Left Queue.OpponentNotFound) = do
    Sockets.send "User not found"
    return Nothing

-- | Sends messages through 'TChan's in 'App.App'.
gameSocket :: SocketsT Handler ()
gameSocket = do
    who <- Auth.requireAuthId
    liftIO Random.createSystemRandom >>= runReaderT do
        (info, writer, reader) <- untilJust $ handleFailures =<< runExceptT do
            teamNames <- Text.split (=='/') <$> Sockets.receive
            (section, team) <- case parseTeam teamNames of
                Nothing   -> throwE Queue.InvalidTeam
                Just vals -> return vals

            liftHandler . runDB $ update who [UserTeam =. Just teamNames]
            queue section team

        lift $ Sockets.sendJson info
        let player = GameInfo.player info
        liftST (Wrapper.fromInfo info) >>= runReaderT do
            when (player == Player.A) $ tryEnact player writer
            completedGame <- untilJust do
                wrapper <- liftIO . atomically $ readTBQueue reader
                Sockets.clear
                if null . Game.victor $ Wrapper.game wrapper then do
                    Sockets.sendJson $ Wrapper.toJSON player wrapper
                    liftST . Wrapper.replace wrapper =<< ask
                    tryEnact player writer
                    return Nothing
                else do
                    return $ Just wrapper
            Sockets.sendJson $ Wrapper.toJSON player completedGame

-- | Wraps @enact@ with error handling.
tryEnact :: ∀ m. (MonadGame m, MonadRandom m, MonadSockets m, MonadUnliftIO m)
         => Player -> TBQueue Wrapper -> m ()
tryEnact player writer = do
    enactMessage <- Timeout.timeout 60000000 $ -- 1 minute
                    Text.split ('/' ==) <$> Sockets.receive

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
            Turn.run []
            conclude
  where
    conclude = do
        wrapper <- Wrapper.freeze
        Sockets.sendJson $ Wrapper.toJSON player wrapper
        atomically $ writeTBQueue writer wrapper

-- | Processes a user's actions and passes them to 'Turn.run'.
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
       | otherwise                               -> Right <$> do
            P.alter . Game.setChakra player $ chakra { Chakra.rand = randTotal }
            Turn.run actions
  where
    randTotal = Chakra.total actChakra - 5 * Chakra.total exchangeChakra
    err       = return . Left
