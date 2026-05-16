module Handler.Queue where

import ClassyPrelude
import Yesod

import           Control.Monad.Loops (untilJust)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.HashTable as HashTable
import           Data.Time.Clock.System (SystemTime(..), getSystemTime)
import           Data.Vector (unsafeFreeze, unsafeThaw)
import qualified Data.Vector.Algorithms.Intro as Algorithms
import qualified System.Random.MWC as Random
import qualified Yesod.Auth as Auth

import           Application.App (App(App), liftDB)
import qualified Application.App as App
import           Application.Model (EntityField(..), User(..))
import           Class.Random (MonadRandom)
import qualified Class.Random as R
import           Class.Sockets (MonadSockets)
import qualified Class.Sockets as Sockets
import           Game.Model.Character (Character)
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Player as Player
import qualified Game.Model.Slot as Slot
import qualified Handler.Client.Message as Client
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Handler.Play.GameInfo as GameInfo
import qualified Handler.Queue.Message as Message
import           Handler.Queue.UserInfo (UserInfo(UserInfo))
import qualified Handler.Queue.UserInfo as UserInfo
import qualified Handler.Play.War as War
import           Handler.Play.Wrapper (Wrapper)

-- | Queue section.
data Section
    = Quick
    | Private
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

chunkPairs :: ∀ a. [a] -> [(a, a)]
chunkPairs (x:y:xs) = (x, y) : chunkPairs xs
chunkPairs _        = []

readyWith :: Int -> SystemTime -> (Key User, UserInfo) -> Bool
readyWith load (MkSystemTime time _) (_, user) = joined + delay < time
  where
    delay = truncate $ sqrt (fromIntegral load :: Float)
    UserInfo{joined = MkSystemTime joined _} = user

quickManager :: App -> IO ()
quickManager App{quick} = forever do
    ready  <- readyWith <$> HashTable.readLoad quick <*> getSystemTime
    assocs <- atomically $ HashTable.readAssocs quick
    usersM <- unsafeThaw . fromList $ filter ready assocs
    Algorithms.sortBy (compare `on` userRating . UserInfo.user . snd) usersM
    usersV <- unsafeFreeze usersM
    let users = toList usersV
    forConcurrently_ (chunkPairs users) \((whoA, infoA), (whoB, infoB)) -> do
        let userA = UserInfo.user infoA; userB = UserInfo.user infoB
            teamA = UserInfo.team infoA; teamB = UserInfo.team infoB
            chanA = UserInfo.chan infoA; chanB = UserInfo.chan infoB

        (mvar, gameA, gameB) <- Random.createSystemRandom >>= runReaderT
                                (makeGame whoA userA teamA whoB userB teamB)
        putMVar chanA $ Message.Response mvar gameA -- this will not block
        putMVar chanB $ Message.Response mvar gameB -- this will not block
        HashTable.delete quick whoA
        HashTable.delete quick whoB

leave :: ∀ m. (MonadHandler m, App ~ HandlerSite m) => m ()
leave = do
    who   <- Auth.requireAuthId
    quick <- getsYesod App.quick
    void . liftIO $ HashTable.delete quick who

queue :: ∀ m. ( MonadHandler m, App ~ HandlerSite m
              , MonadRandom m
              , MonadSockets m
              ) => Section -> [Character]
                -> ExceptT Client.Failure m Message.Response
queue Quick team = do
    (who, user) <- Auth.requireAuthPair
    quick       <- getsYesod App.quick
    liftIO do
        chan   <- newEmptyMVar
        joined <- getSystemTime
        void $ HashTable.insert quick who UserInfo { user, team, joined, chan }
        takeMVar chan {-! BLOCKS !-}

queue Private team = do
    (who, user)         <- Auth.requireAuthPair
    Entity vsWho vsUser <- do
        vsName <- Sockets.receive {-! BLOCKS !-}
        mVs    <- liftDB $ selectFirst [UserName ==. vsName] []
        case mVs of
            Just (Entity vsWho _) | vsWho == who -> throwE Client.NotFound
            Just vs -> return vs
            Nothing -> throwE Client.NotFound

    writer <- getsYesod App.private
    reader <- liftIO $ atomically do
        writeTChan writer $ Message.Request who vsWho team
        dupTChan writer

    untilJust do
      msg <- liftIO . atomically $ readTChan reader {-! BLOCKS !-}
      Client.ping {-! BLOCKS !-}
      case msg of
        Message.Respond mWho response
          | mWho == who && GameInfo.vsWho (Message.info response) == vsWho ->
              return $ Just response
        Message.Request vsWho' requestWho vsTeam
          | vsWho' == vsWho && requestWho == who -> do
              (mvar, gameA, gameB) <- makeGame who user team vsWho vsUser vsTeam
              atomically . writeTChan writer $
                  Message.Respond vsWho' $ Message.Response mvar gameB
              return . Just $ Message.Response mvar gameA
        _ -> return Nothing

makeGame :: ∀ m. (MonadRandom m, MonadIO m)
         => Key User -> User -> [Character]
         -> Key User -> User -> [Character]
         -> m (MVar Wrapper, GameInfo, GameInfo)
makeGame who user team vsWho vsUser vsTeam = do
    player <- R.player
    game   <- Game.newWithChakras
    liftIO do
        let ninjas = fromList $ zipWith Ninja.new Slot.all case player of
                Player.A -> team ++ vsTeam
                Player.B -> vsTeam ++ team
        war  <- War.match team vsTeam <$> War.today
        mvar <- newEmptyMVar
        let gameInfoA = GameInfo { vsWho, vsUser, player, game, ninjas, war }
            gameInfoB = GameInfo { vsWho  = who
                                 , vsUser = user
                                 , player = Player.opponent player
                                 , war    = War.opponent <$> war
                                 , game
                                 , ninjas
                                 }
        return (mvar, gameInfoA, gameInfoB)
