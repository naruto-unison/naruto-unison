-- | Handles API routes and WebSockets related to gameplay.
module Handler.Play
    ( gameSocket
    , getPracticeActR, getPracticeQueueR, getPracticeWaitR
    ) where

import ClassyPrelude hiding (Handler)
import Yesod

import           Control.Monad.Loops (untilJust)
import qualified Data.Cache as Cache
import           Data.HashMap.Strict ((!))
import           Data.List (transpose)
import qualified Data.Text as Text
import qualified Database.Persist.Postgresql as Sql
import qualified System.Random.MWC as Random
import qualified UnliftIO.Timeout as Timeout
import qualified Yesod.Auth as Auth

import qualified Core.App as App
import           Core.App (Handler)
import           Core.Fields (Privilege(..))
import qualified Core.Message as Message
import           Core.Model (EntityField(..), User(..))
import qualified Core.Wrapper as Wrapper
import           Core.Wrapper (Wrapper(Wrapper))
import           Core.Util (duplic)
import qualified Class.Play as P
import           Class.Play (MonadGame)
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

-- | @'concat' . 'transpose'@
vs :: ∀ a. [a] -> [a] -> [a]
x `vs` y = concat $ transpose [x, y]

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
getPracticeQueueR characters@[a1, b1, c1, a2, b2, c2]
  | any (not . (`member` Characters.map)) characters =
        invalidArgs ["Unknown character(s)"]
  | otherwise = do
      (who, _) <- Auth.requireAuthPair
      runDB $ update who [ UserTeam     =. Just [a1, b1, c1]
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
  where
    ninjas  = fromList . zipWith Ninja.new Slot.all $ map (Characters.map !)
              [c1, a2, b1, b2, a1, c2]
getPracticeQueueR _ = invalidArgs ["Wrong number of characters"]
 --zipWith Ninja.new Slot.all
-- | Wrapper for 'getPracticeActR' with no actions.
getPracticeWaitR :: Chakras -> Chakras -> Handler Value
getPracticeWaitR actChakra xChakra = getPracticeActR actChakra xChakra []

-- | Handles a turn for a practice game. Practice games are not limited by time
-- and use GET requests instead of WebSockets.
getPracticeActR :: Chakras -> Chakras -> [Act] -> Handler Value
getPracticeActR actChakra exchangeChakra actions = do
    (who, _) <- Auth.requireAuthPair -- !FAILS!
    practice <- getsYesod App.practice
    mGame    <- liftIO $ Cache.lookup practice who -- !FAILS
    case mGame of
        Nothing   -> notFound
        Just game -> do
          random  <- liftIO Random.createSystemRandom
          wrapper <- Wrapper.thawIO game
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
formTeam team@[a, b, c]
  | duplic team = Nothing
  | otherwise   = [[a', b', c'] | a' <- lookup a Characters.map
                                , b' <- lookup b Characters.map
                                , c' <- lookup c Characters.map
                                ]
formTeam _ = Nothing

formEnact :: [Text] -> Maybe (Chakras, Chakras, [Act])
formEnact (_:_: _:_:_:_:_) = Nothing -- No more than 3 actions!
formEnact (actChakra:exchangeChakra:acts) = do
    actChakra'      <- fromPathPiece actChakra
    exchangeChakra' <- fromPathPiece exchangeChakra
    acts'           <- traverse fromPathPiece acts
    return (actChakra', exchangeChakra', acts')
formEnact _ = Nothing -- willywonka.gif

-- | Sends messages through 'TChan's in 'App.App'.
gameSocket :: SocketsT Handler ()
gameSocket = do
    app         <- lift getYesod
    (who, user) <- Auth.requireAuthPair
    teamNames   <- Text.split (=='/') <$> Sockets.receive

    case formTeam teamNames of
      Nothing   -> Sockets.send "Invalid team"
      Just team -> do
        flip Sql.runSqlPool (App.connPool app) $
            update who [UserTeam =. Just (reverse teamNames)]
        liftIO Random.createSystemRandom >>= runReaderT do
              randPlayer <- (Player.from :: Bool -> Player)
                            <$> (ask >>= liftIO . Random.uniform)
              let writeQueueChan = App.queue app
              readQueueChan <- (liftIO . atomically) do
                  writeTChan writeQueueChan $ Message.Announce who user team
                  dupTChan writeQueueChan
              (info, writer, reader) <- untilJust do
                msg <- liftIO . atomically $ readTChan readQueueChan
                case msg of
                  Message.Respond mWho writer reader info
                    | mWho == who -> return $ Just (info, writer, reader)
                  Message.Announce vsWho vsUser vsTeam -> do
                      game <- Game.newWithChakras
                      let ninjas = zipWith Ninja.new Slot.all case randPlayer of
                              Player.A -> team `vs` vsTeam
                              Player.B -> vsTeam `vs` team
                      liftIO $ atomically do
                          writer <- newTBQueue 8
                          reader <- newTBQueue 8
                          writeTChan writeQueueChan $
                              Message.Respond vsWho reader writer
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
              lift $ Sockets.sendJson info
              let player = GameInfo.player info
              Wrapper.fromInfo info >>= runReaderT do
                  when (player == Player.A) $ tryEnact player writer
                  completedGame <- untilJust do
                      msg  <- liftIO . atomically $ readTBQueue reader
                      case msg of
                          Message.Forfeit -> do
                              P.forfeit $ Player.opponent player
                              Just <$> Wrapper.freeze
                          Message.Enact wrapper -> do
                              if not . null . Game.victor $
                                 Wrapper.game wrapper then
                                  return $ Just wrapper
                              else do
                                  Sockets.clear
                                  Sockets.sendJson $
                                      Wrapper.toJSON player wrapper
                                  Wrapper.replaceIO wrapper
                                  tryEnact player writer
                                  return Nothing
                  Sockets.sendJson $ Wrapper.toJSON player completedGame

-- | Wraps @enact@ with error handling.
tryEnact :: ∀ m. (MonadGame m, MonadRandom m, MonadSockets m, MonadUnliftIO m)
         => Player -> TBQueue Message.Game -> m ()
tryEnact player writer = do
    enactMessage <- Timeout.timeout 60000000 $ -- 1 minute
                    Text.split ('/' ==) <$> Sockets.receive

    case formEnact =<< enactMessage of
        Nothing -> do
            Turn.run []
            conclude
        Just (actChakra, exchangeChakra, actions) -> do
            res <- enact actChakra exchangeChakra actions
            case res of
                Left errorMsg -> Sockets.send errorMsg
                Right ()      -> conclude
  where
    conclude = do
        wrapper <- Wrapper.freeze
        Sockets.sendJson $ Wrapper.toJSON player wrapper
        atomically . writeTBQueue writer $ Message.Enact wrapper

-- | Processes a user's actions and passes them to 'Turn.run'.
enact :: ∀ m. (MonadGame m, MonadRandom m) => Chakras -> Chakras -> [Act]
      -> m (Either LByteString ())
enact actChakra exchangeChakra actions = do
    player     <- P.player
    gameChakra <- Game.getChakra player <$> P.game
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
