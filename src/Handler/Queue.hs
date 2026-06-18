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
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.HashTable as HashTable
import           Data.Time.Clock.System (SystemTime(..), getSystemTime)
import           System.Random.MWC (createSystemRandom)
import           Yesod (getsYesod)
import qualified Yesod.Auth as Auth

import           Application.App (App(App), liftDB)
import qualified Application.App as App
import           Application.Model (EntityField(..))
import           Application.Model.User (User(User))
import qualified Application.Model.User
import           Class.Parse (Parse)
import qualified Class.Parse as Parse
import           Class.Random (MonadRandom)
import qualified Class.Random as R
import           Game.Model.Character (Character)
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as N
import qualified Game.Model.Player as Player
import qualified Game.Model.Slot as Slot
import qualified Handler.Client.Message as Message
import qualified Handler.Client.Socket as Socket
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Handler.Play.GameInfo
import qualified Handler.Queue.Message as Message
import           Handler.Queue.UserInfo (UserInfo(UserInfo))
import qualified Handler.Queue.UserInfo as UserInfo
import qualified Handler.Play.War as War
import           Handler.Play.Wrapper (Wrapper)

-- | Queue section.
data Section
    = Quick
    | Private
    deriving (Bounded, Enum, Eq, Ord, Show)

instance Parse Section where
    parser = Parse.choice
        [ Parse.string "private" $> Private
        , Parse.string "quick"   $> Quick
        ]

chunkPairs :: ∀ a. [a] -> [(a, a)]
chunkPairs (x:y:xs) = (x, y) : chunkPairs xs
chunkPairs _        = []

getPairings :: Int -> SystemTime -> [(Key User, UserInfo)]
            -> [(UserInfo, UserInfo)]
getPairings load (MkSystemTime time _) assocs = chunkPairs
                                              $ sortWith rate
                                              . filter ready
                                              $ snd <$> assocs
  where
    ready UserInfo{joined = MkSystemTime joined _} = joined + delay < time
    rate UserInfo {user = Entity _ User{rating}} = rating
    delay = truncate $ sqrt @Float $ fromIntegral load

quickManager :: App -> IO ()
quickManager App{quick} = forever do
    pairings <- getPairings <$> HashTable.readLoad quick
                            <*> getSystemTime
                            <*> HashTable.readAssocsIO quick
    rand <- createSystemRandom
    mapM_ (runPair rand) pairings
  where
    runPair rand ( UserInfo userA teamA _ chanA
                 , UserInfo userB teamB _ chanB
                 ) = do
        (mvar, gameA, gameB) <- runReaderT makeGame' rand
        putMVar chanA $ Message.Response mvar gameA -- this will not block
        putMVar chanB $ Message.Response mvar gameB -- this will not block
        void $ HashTable.delete quick userA.entityKey
        void $ HashTable.delete quick userB.entityKey
      where
        makeGame' = makeGame userA teamA userB teamB

leave :: ∀ m. App.MonadHandler m => m ()
leave = do
    who   <- Auth.requireAuthId
    quick <- getsYesod App.quick
    void . liftIO $ HashTable.delete quick who

queue :: ∀ m. ( App.MonadHandler m
              , MonadRandom m
              , MonadError Message.Failure m
              ) => Socket.Connection -> Section -> [Character]
                -> m Message.Response
queue _ Quick team = do
    user@(Entity who _) <- Auth.requireAuth
    quick <- getsYesod App.quick
    liftIO do
        chan   <- newEmptyMVar
        joined <- getSystemTime
        void $ HashTable.insert quick who UserInfo { user, team, joined, chan }
        takeMVar chan {-! BLOCKS !-}

queue socket Private team = do
    user@(Entity who _) <- Auth.requireAuth
    vsUser@(Entity vsWho _) <- do
        vsName <- trySocket $ Socket.receiveData socket {-! BLOCKS !-}
        mVs    <- liftDB $ selectFirst [ UserName ==. toStrict (decodeUtf8 vsName) ] []
        case mVs of
            Just vs@(Entity vsWho _) | vsWho /= who -> return vs
            _ -> throwError Message.NotFound

    writer <- getsYesod App.private
    reader <- atomically do
        writeTChan writer $ Message.Request who vsWho team
        dupTChan writer

    untilJust $ runMaybeT do
        msg <- atomically $ readTChan reader {-! BLOCKS !-}
        trySocket $ Socket.sendJSONData socket Message.Ping
        pong <- trySocket $ Socket.receiveData socket {-! BLOCKS !-}
        when (pong == "cancel")
            $ throwError Message.Canceled

        guard $ users msg == (who, vsWho)

        case msg of
            Message.Respond _ response -> return response

            Message.Request vsWho' _ vsTeam -> do
              (mvar, gameA, gameB) <- makeGame user team vsUser vsTeam
              atomically . writeTChan writer
                  $ Message.Respond vsWho' $ Message.Response mvar gameB
              return $ Message.Response mvar gameA
  where
    users (Message.Respond who response) = (who, response.info.vsUser.entityKey)
    users (Message.Request vsWho who _)  = (who, vsWho)
    trySocket m = f =<< m
      where
        f (Left err)     = throwError $ Message.SocketError
                                      $ displayException err
        f (Right result) = return result

makeGame :: ∀ m. (MonadRandom m, MonadIO m)
         => Entity User -> [Character]
         -> Entity User -> [Character]
         -> m (MVar Wrapper, GameInfo, GameInfo)
makeGame user team vsUser vsTeam = do
    player <- R.random
    game   <- Game.newWithChakras
    war    <- liftIO $ War.match team vsTeam <$> War.today
    mvar   <- newEmptyMVar
    let ninjas = fromList $ zipWith N.new Slot.all case player of
            Player.A -> team ++ vsTeam
            Player.B -> vsTeam ++ team
        gameInfoA = GameInfo
            { vsUser
            , player
            , war
            , game
            , ninjas
            , snapshots = mempty
            }
        gameInfoB = GameInfo
            { vsUser = user
            , player = Player.opponent player
            , war    = War.opponent <$> war
            , game
            , ninjas
            , snapshots = mempty
            }
    return (mvar, gameInfoA, gameInfoB)
