-- | Handles API routes and WebSockets related to gameplay.
module Handler.Play.Game
    ( Enact(..), enact
    , gameSocket
    ) where

import ClassyPrelude
import Database.Persist

import           Control.Monad (fail)
import           Control.Monad.Error.Class (MonadError(..), modifyError)
import           Control.Monad.Logger (MonadLogger, logErrorN)
import           Control.Monad.Loops (untilJust, whileM)
import           Control.Monad.Trans.Except (runExceptT, except)
import           UnliftIO.Concurrent (forkIO, threadDelay)
import qualified Yesod.Auth as Auth
import           Yesod.Core (getsYesod, liftHandler)

import           Application.App (liftDB)
import qualified Application.App as App
import           Application.Model (EntityField(..))
import           Application.Settings (Settings(Settings))
import qualified Application.Settings as Settings
import           Class.Hook (MonadHook)
import qualified Class.Parity as Parity
import           Class.Parse (Parse(..), Parser, Parsed)
import qualified Class.Parse as Parse
import           Class.Play (MonadGame)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Game.Engine as Engine
import           Game.Model.Chakras (Chakras)
import qualified Game.Model.Chakras as Chakras
import           Game.Model.Character (Character)
import qualified Game.Model.Character as Character
import qualified Game.Model.Context as Context
import           Game.Model.Game (Game(Game))
import qualified Game.Model.Game as Game
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import qualified Game.Model.Skill as Skill
import qualified Game.Model.Slot as Slot
import qualified Handler.Client.Message as Message
import           Handler.Client.Reward (Reward(Reward))
import qualified Handler.Client.Socket as Socket
import           Handler.Play.Act (Act)
import qualified Handler.Play.Act as Act
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Handler.Play.GameInfo
import           Handler.Play.Match (Outcome(..))
import qualified Handler.Play.Match as Match
import qualified Handler.Play.Rating as Rating
import           Handler.Play.Wrapper (Wrapper)
import qualified Handler.Play.Wrapper as Wrapper
import qualified Handler.Queue as Queue
import           Handler.Queue.Message (Response(Response))
import qualified Mission
import           Util ((∈), (∉), leftToMaybe, tryFromJust)

-- * INPUT PARSING

separator :: Char
separator = '/'

separate :: ∀ i. Parsed i => Parser i Char
separate = Parse.char separator

data Team = Team Queue.Section [Character]

instance Parse Team where
    parser = Team <$> parseSection
                  <*> (separate >> parseCharacters)
      where
        parseSection = Parse.string "private" $> Queue.Private
                    <|> Parse.string "quick" $> Queue.Quick

        parseCharacters = do
            chars <- Parse.sepBy (Parse.parser @Character) separate
            case chars of
                [_, _, _] -> return chars
                _         -> fail "Must have 3 team members"


data Enact = Enact
    { spend    :: Chakras
    , exchange :: Chakras
    , actions  :: [Act]
    } deriving (Eq, Show)

instance Parse Enact where
    parser = Enact
        <$> Parse.parser @Chakras
        <*> (separate >> Parse.parser @Chakras)
        <*> (separate >> parseActs <|> Parse.endOfInput $> [])
      where
        parseActs = do
            acts <- Parse.sepBy (Parse.parser @Act) separate
            case acts of
                (_:_:_:_:_) -> fail "No more than 3 actions"
                _           -> return acts

data ClientMessage
    = Forfeit
    | EnactMsg Enact
    deriving (Eq, Show)

instance Parse ClientMessage where
    parser = (Parse.string "forfeit" $> Forfeit)
        <|> EnactMsg
        <$> Parse.parser @Enact

-- * HANDLERS

handleFailures :: ∀ m a. MonadIO m
               => Socket.Connection -> Either Message.Failure a -> m (Maybe a)
handleFailures socket (Left msg)  = Socket.sendJSONData socket (Message.Fail msg)
                                    $> Nothing
handleFailures _      (Right val) = return $ Just val

-- | Sends messages through 'MVar's in 'App.App'. Requires authentication.
gameSocket :: ∀ m. ( App.MonadHandler m
                   , MonadUnliftIO m
                   , MonadRandom m
                   , PrimMonad m
                   ) => m ()
gameSocket = Socket.withSocket \socket -> do
    who      <- Auth.requireAuthId
    settings <- getsYesod App.settings
    unlocked <- liftHandler Mission.unlocked

    (section, team, Response mvar info@GameInfo{player, war, vsWho}) <-
        untilJust $ handleFailures socket =<< runExceptT do
            message <- Socket.receiveData socket {-! BLOCKS !-}
            Team section team <- modifyError Message.InvalidTeam . except
                               $ Parse.parseOnly @Team message

            let teamNames = Character.ident <$> team
                locked    = filter (∉ unlocked) teamNames
            when (not $ null locked)
                . throwError $ Message.Locked locked
            liftDB $ update who [ UserTeam =. Just teamNames ]

            queued <- Queue.queue socket section team {-! BLOCKS !-}
            return (section, teamNames, queued)

    liftIO $ Socket.sendJSONData socket $ Message.Info info

    game <- Wrapper.runGame info do
        when (player == Player.A)
            $ tryEnact socket settings player mvar {-! BLOCKS !-}

        void $ whileM (Game.inProgress <$> P.game) do
            wrapper <- takeMVar mvar {-! BLOCKS !-}

            if Game.inProgress $ Wrapper.game wrapper then do
                liftIO . Socket.sendJSONData socket
                       . Message.Play $ Wrapper.toTurn player wrapper
                Wrapper.replace wrapper =<< ask
                tryEnact socket settings player mvar {-! BLOCKS !-}
                game <- P.game

                unless (Game.inProgress game) . liftDB . void $ forkIO do
                    match <- Match.load $ Match.fromGame game player who vsWho
                    mapM_ Rating.updatePostMatch match
            else
                Wrapper.replace wrapper =<< ask

    liftIO . Socket.sendJSONData socket
           . Message.Play $ Wrapper.toTurn player game

    when (section == Queue.Quick) do -- eventually, || Queue.Ladder
        let outcome = Match.outcome (Wrapper.game game) player
        if outcome == Defeat && Game.forfeit (Wrapper.game game) then
            liftIO . Socket.sendJSONData socket
                   $ Message.Rewards [Reward "Forfeit" 0]
        else do
            dnaReward <- liftHandler $ Mission.awardDNA Queue.Quick outcome war
            liftIO . Socket.sendJSONData socket $ Message.Rewards dnaReward

        liftHandler do
            case outcome of
                Victory -> Mission.processWin team
                _       -> Mission.processDefeat team
            Mission.processUnpicked team
            mapM_ (void . Mission.progress) $ Wrapper.progress game

  `finally`
      Queue.leave

data ClientResponse
    = Received ClientMessage
    | Malformed ByteString
    | TimedOut
    | SocketException Socket.ConnectionException
    deriving (Eq, Show)

decodeMessage :: Either Socket.ConnectionException LByteString -> ClientResponse
decodeMessage (Left err) = SocketException err
decodeMessage (Right bytes) = case Parse.parseOnly @ClientMessage message of
    Left _       -> Malformed message
    Right parsed -> Received parsed
  where
    message = toStrict bytes

-- | Wraps @enact@ with error handling.
tryEnact :: ∀ m. ( MonadGame m
                 , MonadHook m
                 , MonadRandom m
                 , MonadUnliftIO m
                 , MonadLogger m
                 )
         => Socket.Connection -> Settings -> Player -> MVar Wrapper -> m ()
tryEnact socket Settings{forfeitAfterSkips, turnLength} player mvar = do
    -- This is necessary because interrupting Sockets.receive closes the socket
    -- connection, which means that a naive timeout will break the connection.
    -- Even if the turn is over and its output will be ignored, Sockets.receive
    -- must not be canceled.

    enactMessage <- liftIO do
        lock <- newEmptyMVar

        forkIO do
            threadDelay turnLength {-! BLOCKS !-}
            void $ tryPutMVar lock TimedOut

        forkIO do
            tryMessage <- try $ Socket.receiveData socket {-! BLOCKS !-}
            void $ tryPutMVar lock $ decodeMessage tryMessage

        readMVar lock {-! BLOCKS !-}

    case enactMessage of
        Received Forfeit ->
            Engine.forfeit player

        Received (EnactMsg enactMsg) -> do
            Engine.resetInactive player
            res <- runExceptT $ enact enactMsg
            forM_ (leftToMaybe res) \errorMsg -> do
                logErrorN $ "Client error: " ++ errorMsg
                Socket.sendTextData socket . fromStrict $ encodeUtf8 errorMsg

        Malformed malformed ->
            logErrorN $ "Malformed client input: " ++ decodeUtf8 malformed

        TimedOut ->
            Engine.skipTurn forfeitAfterSkips player

        SocketException Socket.ConnectionClosed -> do
            logErrorN "Socket closed"
            Engine.forfeit player

        SocketException (Socket.CloseRequest code why) -> do
            logErrorN $ "Socket closed: " ++ tshow code ++ " "
                        ++ toStrict (decodeUtf8 why)
            Engine.forfeit player

        SocketException (Socket.ParseException malformed) ->
            logErrorN $ "Malformed client input: " ++ pack malformed

        SocketException (Socket.UnicodeException malformed) ->
            logErrorN $ "Malformed client input: " ++ pack malformed

    wrapper <- Wrapper.freeze
    Socket.sendJSONData socket . Message.Play $ Wrapper.toTurn player wrapper
    putMVar mvar wrapper -- this should never block

-- | Processes a user's actions and passes them to 'Engine.run'.
enact :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m, MonadError Text m)
      => Enact -> m ()
enact Enact{spend, exchange, actions}
  | randTotal < 0 = throwError "Insufficient chakra"
  | otherwise     = do
    contexts <- mapM Act.toContext actions
    Game{chakra, playing = player} <- P.game
    validate player contexts

    newChakra <- tryFromJust "Insufficient chakra"
               $ getRemainingChakra contexts $ Parity.getOf player chakra
    P.alter $ Game.setChakra player newChakra
    Engine.runTurn contexts
  where
    randTotal = length spend - 5 * length exchange

    getRemainingChakra contexts chakra = do
        exchanged <- Chakras.checkedSpend spend $ chakra ++ exchange
        Chakras.checkedSpend actCosts $ exchanged { Chakras.rand = randTotal }
      where
        actCosts = concatMap (Skill.cost . Context.skill) contexts

    validate player contexts
      | length contexts > Slot.teamSize       = throwError "Too many actions"
      | nonUnique $ Context.user <$> contexts = throwError "Duplicate actors"
      | any (Context.illegal player) contexts = throwError "Character out of range"
      | otherwise                             = return ()

    nonUnique = go (mempty :: IntSet)
      where
        go set ((Slot.toInt -> x):xs) = x ∈ set || go (insertSet x set) xs
        go _   []                     = False
