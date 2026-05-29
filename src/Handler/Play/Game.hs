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
import qualified Data.Attoparsec.Text as Parse
import           Data.Attoparsec.Text (Parser)
import           Network.WebSockets (ConnectionException(..))
import           UnliftIO.Concurrent (forkIO, threadDelay)
import qualified Yesod.Auth as Auth
import           Yesod.Core (getsYesod, liftHandler)
import           Yesod.WebSockets (webSockets)

import           Application.App (liftDB)
import qualified Application.App as App
import           Application.Model (EntityField(..))
import           Application.Settings (Settings)
import qualified Application.Settings as Settings
import           Class.Hook (MonadHook)
import qualified Class.Parity as Parity
import           Class.Play (MonadGame)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import           Class.Sockets (MonadSockets)
import qualified Class.Sockets as Sockets
import qualified Game.Characters as Characters
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
import qualified Handler.Client.Message as Client
import           Handler.Client.Reward (Reward(Reward))
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

separate :: Parser Char
separate = Parse.char separator

data Team = Team Queue.Section [Character]

parseTeam :: Parser Team
parseTeam = Team <$> parseSection <*> parseCharacters
  where
    parseSection = Parse.string "private" $> Queue.Private
                 <|> Parse.string "quick" $> Queue.Quick

    parseCharacters = Parse.count 3 do
        separate
        text <- Parse.takeWhile (/= separator) <|> Parse.takeText
        case Characters.lookup text of
            Just c  -> return c
            Nothing -> fail $ unpack (text ++ " is not a character")


data Enact = Enact
    { spend    :: Chakras
    , exchange :: Chakras
    , actions  :: [Act]
    } deriving (Eq, Show)

parseActs :: Parser [Act]
parseActs = do
    separate
    acts <- Parse.sepBy Act.parse separate
    case acts of
        (_:_:_:_:_) -> fail "No more than 3 actions"
        _           -> return acts

parseEnact :: Parser Enact
parseEnact = Enact
    <$> Chakras.parse
    <*> (separate >> Chakras.parse)
    <*> (parseActs <|> Parse.endOfInput $> [])

data ClientMessage
    = Forfeit
    | EnactMsg Enact
    deriving (Eq, Show)

parseMessage :: Parser ClientMessage
parseMessage = (Parse.string "forfeit" >> return Forfeit)
    <|> EnactMsg
    <$> parseEnact

-- * HANDLERS

handleFailures :: ∀ m a. MonadSockets m => Either Client.Failure a -> m (Maybe a)
handleFailures (Left msg)  = Client.send (Client.Fail msg) $> Nothing
handleFailures (Right val) = return $ Just val

-- | Sends messages through 'MVar's in 'App.App'. Requires authentication.
gameSocket :: ∀ m. ( App.MonadHandler m
                   , MonadUnliftIO m
                   , MonadRandom m
                   , PrimMonad m
                   ) => m ()
gameSocket = webSockets do
    who      <- Auth.requireAuthId
    settings <- getsYesod App.settings
    unlocked <- liftHandler Mission.unlocked

    (section, team, Response mvar info@GameInfo{player, war, vsWho}) <-
        untilJust $ handleFailures =<< runExceptT do
            message <- Sockets.receive {-! BLOCKS !-}
            Team section team <- modifyError Client.InvalidTeam
                               $ except $ Parse.parseOnly parseTeam message

            let teamNames = Character.ident <$> team
                locked    = filter (∉ unlocked) teamNames
            when (not $ null locked)
                . throwError $ Client.Locked locked
            liftDB $ update who [ UserTeam =. Just teamNames ]

            queued <- Queue.queue section team {-! BLOCKS !-}
            return (section, teamNames, queued)

    Client.send $ Client.Info info

    game <- Wrapper.runGame info do
        when (player == Player.A)
            $ tryEnact settings player mvar {-! BLOCKS !-}

        void $ whileM (Game.inProgress <$> P.game) do
            wrapper <- takeMVar mvar {-! BLOCKS !-}

            if Game.inProgress $ Wrapper.game wrapper then do
                Client.send . Client.Play $ Wrapper.toTurn player wrapper
                Wrapper.replace wrapper =<< ask
                tryEnact settings player mvar {-! BLOCKS !-}
                game <- P.game

                unless (Game.inProgress game) . liftDB . void $ forkIO do
                    match <- Match.load $ Match.fromGame game player who vsWho
                    mapM_ Rating.updatePostMatch match
            else
                Wrapper.replace wrapper =<< ask

    Client.send . Client.Play $ Wrapper.toTurn player game

    when (section == Queue.Quick) do -- eventually, || Queue.Ladder
        let outcome = Match.outcome (Wrapper.game game) player
        if outcome == Defeat && Game.forfeit (Wrapper.game game) then
            Client.send $ Client.Rewards [Reward "Forfeit" 0]
        else do
            dnaReward <- liftHandler $ Mission.awardDNA Queue.Quick outcome war
            Client.send $ Client.Rewards dnaReward

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
    | Malformed Text
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

    liftIO $ forkIO do
        threadDelay $ Settings.turnLength settings {-! BLOCKS !-}
        void $ tryPutMVar lock TimedOut

    forkIO do
        tryMessage <- try Sockets.receive {-! BLOCKS !-}
        void $ tryPutMVar lock case tryMessage of
            Left err      -> SocketException err
            Right message -> case Parse.parseOnly parseMessage message of
                                Left _       -> Malformed message
                                Right parsed -> Received parsed

    enactMessage <- readMVar lock {-! BLOCKS !-}

    case enactMessage of
        Received Forfeit ->
            Engine.forfeit player

        Received (EnactMsg enactMsg) -> do
            Engine.resetInactive player
            res <- runExceptT $ enact enactMsg
            forM_ (leftToMaybe res) \errorMsg -> do
                logErrorN $ "Client error: " ++ errorMsg
                Sockets.send $ fromStrict $ encodeUtf8 errorMsg

        Malformed malformed ->
            logErrorN $ "Malformed client input: " ++ malformed

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
    Client.send . Client.Play $ Wrapper.toTurn player wrapper
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
