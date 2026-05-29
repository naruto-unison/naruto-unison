module Handler.Queue
    ( Section(..)
    , quickManager
    , leave
    , queue
    ) where

import ClassyPrelude
import Database.Persist

import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Loops (untilJust)
import qualified Data.HashTable as HashTable
import           Data.Time.Clock.System (SystemTime(..), getSystemTime)
import qualified System.Random.MWC as Random
import           Yesod (getsYesod)
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
import qualified Game.Model.Ninja as N
import qualified Game.Model.Player as Player
import qualified Game.Model.Slot as Slot
import qualified Handler.Client.Message as Client
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Handler.Play.GameInfo
import           Handler.Queue.Message (Response(Response))
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

getPairings :: Int -> SystemTime -> [(Key User, UserInfo)]
            -> [((Key User, UserInfo), (Key User, UserInfo))]
getPairings load (MkSystemTime time _) assocs = chunkPairs
                                              $ sortWith rate
                                              $ filter ready assocs
  where
    ready (_, UserInfo{joined = MkSystemTime joined _}) = joined + delay < time
    rate (_, (UserInfo User{userRating} _ _ _)) = userRating
    delay = truncate $ sqrt (fromIntegral load :: Float)

quickManager :: App -> IO ()
quickManager App{quick} = forever do
    pairings <- getPairings <$> HashTable.readLoad quick
                            <*> getSystemTime
                            <*> atomically (HashTable.readAssocs quick)
    rand <- Random.createSystemRandom
    mapM_ (runPair rand) pairings
  where
    runPair rand ( (whoA, UserInfo userA teamA _ chanA)
                 , (whoB, UserInfo userB teamB _ chanB)
                 ) = void do
        (mvar, gameA, gameB) <- runReaderT makeGame' rand
        putMVar chanA $ Message.Response mvar gameA -- this will not block
        putMVar chanB $ Message.Response mvar gameB -- this will not block
        HashTable.delete quick whoA
        HashTable.delete quick whoB
      where
        makeGame' = makeGame whoA userA teamA whoB userB teamB

leave :: ∀ m. App.MonadHandler m => m ()
leave = do
    who   <- Auth.requireAuthId
    quick <- getsYesod App.quick
    void . liftIO $ HashTable.delete quick who

queue :: ∀ m. ( App.MonadHandler m
              , MonadRandom m
              , MonadSockets m
              , MonadError Client.Failure m
              ) => Section -> [Character]
                -> m Message.Response
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
            Just vs@(Entity vsWho _) | vsWho /= who -> return vs
            _ -> throwError Client.NotFound

    writer <- getsYesod App.private
    reader <- liftIO $ atomically do
        writeTChan writer $ Message.Request who vsWho team
        dupTChan writer

    untilJust do
      msg <- liftIO . atomically $ readTChan reader {-! BLOCKS !-}
      Client.ping {-! BLOCKS !-}
      case msg of
        Message.Respond mWho response@Response{info = GameInfo{vsWho = vsWho'}}
          | vsWho' == vsWho && mWho == who -> return $ Just response
        Message.Request vsWho' requestWho vsTeam
          | vsWho' == vsWho && requestWho == who -> do
              (mvar, gameA, gameB) <- makeGame who user team vsWho vsUser vsTeam
              atomically . writeTChan writer
                  $ Message.Respond vsWho' $ Message.Response mvar gameB
              return . Just $ Message.Response mvar gameA
        _ -> return Nothing

makeGame :: ∀ m. (MonadRandom m, MonadIO m)
         => Key User -> User -> [Character]
         -> Key User -> User -> [Character]
         -> m (MVar Wrapper, GameInfo, GameInfo)
makeGame who user team vsWho vsUser vsTeam = makeGame' <$> R.random
                                                       <*> Game.newWithChakras
                                                       <*> liftIO War.today
                                                       <*> newEmptyMVar
  where
    makeGame' player game warPair mvar = (mvar, gameInfoA, gameInfoB)
      where
        war = War.match team vsTeam warPair
        ninjas = fromList $ zipWith N.new Slot.all case player of
                 Player.A -> team ++ vsTeam
                 Player.B -> vsTeam ++ team
        gameInfoA = GameInfo
                { vsWho
                , vsUser
                , player
                , war
                , game
                , ninjas
                }
        gameInfoB = GameInfo
            { vsWho  = who
            , vsUser = user
            , player = Player.opponent player
            , war    = War.opponent <$> war
            , game
            , ninjas
            }
