{-# LANGUAGE DeriveAnyClass #-}

-- | Handles API routes and WebSockets related to gameplay.
module Handler.Play
    ( getPracticeActR, getPracticeQueueR, getPracticeWaitR
    , gameSocket
    ) where

import ClassyPrelude
import Yesod

import           Control.Monad (fail)
import           Control.Monad.Logger
import           Control.Monad.Loops (untilJust, whileM)
import           Control.Monad.Trans.Except (runExceptT, throwE)
import qualified Data.Attoparsec.Text as Parse
import           Data.Attoparsec.Text (Parser)
import qualified Data.Cache as Cache
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
import           Class.Sockets (MonadSockets)
import qualified Class.Sockets as Sockets
import qualified Game.AI as AI
import qualified Game.Characters as Characters
import qualified Game.Engine as Engine
import           Game.Model.Chakra (Chakras)
import qualified Game.Model.Chakra as Chakra
import qualified Game.Model.Character as Character
import           Game.Model.Character (Character)
import qualified Game.Model.Context as Context
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import qualified Game.Model.Skill as Skill
import qualified Game.Model.Slot as Slot
import qualified Handler.Client.Message as Client
import           Handler.Client.Reward (Reward(Reward))
import           Handler.Play.Act (Act)
import qualified Handler.Play.Act as Act
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Handler.Play.GameInfo as GameInfo
import           Handler.Play.Match (Outcome(..))
import qualified Handler.Play.Match as Match
import qualified Handler.Play.Rating as Rating
import           Handler.Play.Wrapper (Wrapper(Wrapper))
import qualified Handler.Play.Wrapper as Wrapper
import qualified Handler.Queue as Queue
import           Handler.Queue.Message (Response(Response))
import qualified Mission
import           Util ((∉), duplic, liftST)

-- * INPUT PARSING

separator :: Char
separator = '/'

separate :: Parser Char
separate = Parse.char separator

data Team = Team Queue.Section [Character]

parseTeam :: Parser Team
parseTeam = Team
    <$> parseSection
    <*> parseCharacters
  where
    parseSection =
        Parse.string "private" $> Queue.Private
        <|> Parse.string "quick" $> Queue.Quick

    parseCharacters =
        Parse.count 3 $
            separate
            >> Parse.takeWhile (/= separator) <|> Parse.takeText
            >>= parseCharacter

    parseCharacter text =
        case Characters.lookup text of
            Just c -> return c
            Nothing -> fail $ show (text ++ " is not a character")


data Enact = Enact { spend    :: Chakras
                   , exchange :: Chakras
                   , actions  :: [Act]
                   }
  deriving (Eq, Show)

parseActs :: Parser [Act]
parseActs = separate >> Parse.sepBy Act.parse separate >>= guardLength
  where
    guardLength (_:_:_:_:_) = fail "No more than 3 actions"
    guardLength xs          = return xs

parseEnact :: Parser Enact
parseEnact = Enact
    <$> Chakra.parse
    <*> (separate >> Chakra.parse)
    <*> (parseActs <|> (Parse.endOfInput >> return []))

data ClientMessage
    = Forfeit
    | EnactMsg Enact
    deriving (Eq, Show)

parseMessage :: Parser ClientMessage
parseMessage =
    (Parse.string "forfeit" >> return Forfeit)
    <|> EnactMsg <$> parseEnact

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
getPracticeActR spend exchange actions = do
    who      <- Auth.requireAuthId
    practice <- getsYesod App.practice
    mGame    <- liftIO $ Cache.lookup practice who
    game     <- maybe notFound return mGame
    rand     <- liftIO Random.createSystemRandom
    wrapper  <- liftST $ Wrapper.thaw game

    flip runReaderT rand $ flip runReaderT wrapper do
        res  <- enact Enact{spend, exchange, actions}
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

handleFailures :: ∀ m a. MonadSockets m => Either Client.Failure a -> m (Maybe a)
handleFailures (Right val) = return $ Just val
handleFailures (Left msg)  = Nothing <$ Client.send (Client.Fail msg)

-- | Sends messages through 'TChan's in 'App.App'. Requires authentication.
gameSocket :: ∀ m. ( MonadHandler m, App ~ HandlerSite m
                   , MonadUnliftIO m
                   , MonadRandom m
                   ) => m ()
gameSocket = webSockets do
    who      <- Auth.requireAuthId
    settings <- getsYesod App.settings
    unlocked <- liftHandler Mission.unlocked

    (section, team, Response mvar info) <- untilJust $
        handleFailures =<< runExceptT do
            Team section team <- either (throwE . Client.InvalidTeam)
                                 return . Parse.parseOnly parseTeam =<<
                                 Sockets.receive

            let teamNames = Character.ident <$> team
                locked = filter (∉ unlocked) teamNames
            when (not $ null locked) . throwE $ Client.Locked locked
            liftDB $ update who [UserTeam =. Just teamNames]

            queued <- Queue.queue section team
            return (section, teamNames, queued)

    Client.send $ Client.Info info
    let player = GameInfo.player info

    game <- liftST (Wrapper.fromInfo info) >>= runReaderT do
        when (player == Player.A) $ tryEnact settings player mvar

        whileM (Game.inProgress <$> P.game) do
            wrapper <- takeMVar mvar

            if Game.inProgress $ Wrapper.game wrapper then do
                Client.send . Client.Play $ Wrapper.toTurn player wrapper
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

    Client.send . Client.Play $ Wrapper.toTurn player game

    when (section == Queue.Quick) do -- eventually, || Queue.Ladder
        let outcome = Match.outcome (Wrapper.game game) player
        if outcome == Defeat && Game.forfeit (Wrapper.game game) then
            Client.send $ Client.Rewards [Reward "Forfeit" 0]
        else do
            dnaReward <- liftHandler .
                Mission.awardDNA Queue.Quick outcome $ GameInfo.war info
            Client.send $ Client.Rewards dnaReward

        liftHandler do
            case outcome of
                Victory -> Mission.processWin team
                _       -> Mission.processDefeat team
            Mission.processUnpicked team
            traverse_ Mission.progress $ Wrapper.progress game

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
        threadDelay $ Settings.turnLength settings
        void $ tryPutMVar lock TimedOut

    forkIO do
        tryMessage <- try Sockets.receive
        void $ tryPutMVar lock case tryMessage of
            Left err      -> SocketException err
            Right message -> either (const $ Malformed message) Received $
                             Parse.parseOnly parseMessage message

    enactMessage <- readMVar lock

    case enactMessage of
        Received Forfeit ->
            Engine.forfeit player

        Received (EnactMsg enactMsg) -> do
            Engine.resetInactive player
            res <- enact enactMsg
            case res of
                Right ()      -> return ()
                Left errorMsg -> do
                    logErrorN $ "Client error: "
                                ++ toStrict (decodeUtf8 errorMsg)
                    Sockets.send errorMsg

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
    putMVar mvar wrapper

-- | Processes a user's actions and passes them to 'Engine.run'.
enact :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
      => Enact -> m (Either LByteString ())
enact Enact{spend, exchange, actions} = runExceptT do
    contexts   <- traverse Act.toContext actions
    player     <- P.player
    gameChakra <- Parity.getOf player . Game.chakra <$> P.game
    let actCosts  = sum $ Skill.cost . Context.skill <$> contexts
        adjChakra = gameChakra + exchange - spend
        chakra    = adjChakra { Chakra.rand = randTotal } - actCosts

    mapM_ throwE $ illegal player chakra contexts
    P.alter $ Game.setChakra player chakra
    Engine.runTurn contexts
  where
    randTotal = Chakra.total spend - 5 * Chakra.total exchange
    illegal player chakra contexts
      | length contexts > Slot.teamSize       = Just "Too many actions"
      | duplic $ Context.user <$> contexts    = Just "Duplicate actors"
      | randTotal < 0 || Chakra.lack chakra   = Just "Insufficient chakra"
      | any (Context.illegal player) contexts = Just "Character out of range"
      | otherwise                             = Nothing
