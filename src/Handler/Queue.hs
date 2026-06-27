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
import           Yesod (getsYesod, liftHandler)
import qualified Yesod.Auth as Auth

import           Application.App (App(App), liftDB)
import qualified Application.App as App
import           Application.Model (EntityField(..))
import qualified Application.Model.Unlocked as Unlocked
import           Application.Model.User (User(User))
import qualified Application.Model.User
import           Class.Random (MonadRandom)
import qualified Class.Random as R
import           Game.Model.Character (Character)
import qualified Game.Model.Character as Character
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as N
import qualified Game.Model.Player as Player
import qualified Game.Model.Slot as Slot
import qualified Handler.Client.QueueMessage as QueueMessage
import qualified Handler.Client.Socket as Socket
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Handler.Play.GameInfo
import qualified Handler.Queue.Message as Message
import           Handler.Queue.Section (Section(..))
import           Handler.Queue.UserInfo (UserInfo(UserInfo))
import qualified Handler.Queue.UserInfo as UserInfo
import qualified Handler.Play.War as War
import           Handler.Play.Wrapper (Wrapper)
import qualified Mission
import           Util ((∉))

chunkPairs :: ∀ a. [a] -> [(a, a)]
chunkPairs (x:y:xs) = (x, y) : chunkPairs xs
chunkPairs _        = []

getPairings :: Int -> SystemTime -> [(Key User, UserInfo)]
            -> [(UserInfo, UserInfo)]
getPairings load (MkSystemTime time _) assocs =
    chunkPairs . sortWith rate . filter ready $ snd <$> assocs
  where
    ready UserInfo{joined = MkSystemTime joined _} = joined + delay < time
    rate UserInfo{user = Entity _ User{rating}} = rating
    delay = truncate $ sqrt @Float $ fromIntegral load

quickManager :: App -> IO ()
quickManager App{quick} = forever do
    pairings <- getPairings <$> HashTable.readLoad quick
                            <*> getSystemTime
                            <*> HashTable.readAssocsIO quick
    rand <- createSystemRandom
    mapM_ (runPair rand) pairings
  where
    runPair rand ( UserInfo userA teamA dnaA _ chanA
                 , UserInfo userB teamB dnaB _ chanB
                 ) = do
        (mvar, gameA, gameB) <- runReaderT makeGame' rand
        putMVar chanA $ Message.Response mvar gameA -- this will not block
        putMVar chanB $ Message.Response mvar gameB -- this will not block
        void $ HashTable.delete quick userA.entityKey
        void $ HashTable.delete quick userB.entityKey
      where
        makeGame' = makeGame userA teamA dnaA userB teamB dnaB

leave :: ∀ m. App.MonadHandler m => m ()
leave = do
    who   <- Auth.requireAuthId
    quick <- getsYesod App.quick
    void . liftIO $ HashTable.delete quick who

getDnaUnlocks :: ∀ m. App.MonadHandler m => Key User -> m (Seq Text)
getDnaUnlocks who = fromList . filter Unlocked.reanimated . toList
                <$> liftHandler (Mission.unlockedOf (Just who))

queue :: ∀ m. ( App.MonadHandler m
              , MonadRandom m
              , MonadError QueueMessage.QueueFailure m
              ) => Socket.Connection -> Section -> [Character]
                -> m Message.Response
queue _ Quick team = do
    user@(Entity who _) <- Auth.requireAuth
    quick       <- getsYesod App.quick
    dnaUnlocks  <- getDnaUnlocks who
    liftIO do
        chan   <- newEmptyMVar
        joined <- getSystemTime
        void $ HashTable.insert quick who UserInfo
            { user
            , team
            , dna = dnaUnlocks
            , joined
            , chan
            }
        takeMVar chan {-! BLOCKS !-}

queue socket Private team = do
    user@(Entity who _) <- Auth.requireAuth
    vsUser@(Entity vsWho _) <- do
        vsName <- trySocket $ Socket.receiveData socket {-! BLOCKS !-}
        mVs    <- liftDB $ selectFirst
                    [ UserName ==. toStrict (decodeUtf8 vsName) ] []
        case mVs of
            Just vs@(Entity vsWho _) | vsWho /= who -> return vs
            _ -> throwError QueueMessage.NotFound

    writer <- getsYesod App.private
    reader <- atomically do
        writeTChan writer $ Message.Request who vsWho team
        dupTChan writer

    untilJust $ runMaybeT do
        msg <- atomically $ readTChan reader {-! BLOCKS !-}
        trySocket $ Socket.sendJSONData socket QueueMessage.Ping
        pong <- trySocket $ Socket.receiveData socket {-! BLOCKS !-}
        when (pong == "cancel")
            $ throwError QueueMessage.Canceled

        guard $ users msg == (who, vsWho)

        case msg of
            Message.Respond _ response -> return response

            Message.Request vsWho' _who' vsTeam -> do
                (dna, vsDna) <- liftDB $ (,) <$> getDnaUnlocks who
                                             <*> getDnaUnlocks vsWho'
                (mvar, gameA, gameB) <- makeGame user team dna
                                        vsUser vsTeam vsDna
                atomically . writeTChan writer
                    $ Message.Respond vsWho' $ Message.Response mvar gameB
                return $ Message.Response mvar gameA
  where
    users (Message.Respond who response) = (who, response.info.vsUser.entityKey)
    users (Message.Request vsWho who _)  = (who, vsWho)
    trySocket m = f =<< m
      where
        f (Left err)     = throwError $ QueueMessage.SocketError
                                      $ displayException err
        f (Right result) = return result

makeGame :: ∀ m. (MonadRandom m, MonadIO m)
         => Entity User -> [Character] -> Seq Text
         -> Entity User -> [Character] -> Seq Text
         -> m (MVar Wrapper, GameInfo, GameInfo)
makeGame user team userDna vsUser vsTeam vsDna = do
    player <- R.random
    game   <- Game.newWithChakras
    war    <- liftIO $ War.match team vsTeam <$> War.today
    mvar   <- newEmptyMVar
    let ninjas = fromList $ zipWith N.new Slot.all case player of
            Player.A -> team ++ vsTeam
            Player.B -> vsTeam ++ team
        userDna' = filterDna team userDna
        vsDna'   = filterDna vsTeam vsDna
        game' = game { Game.dna = (userDna', vsDna') }
        gameInfoA = GameInfo
            { vsUser
            , player
            , war
            , game = game'
            , ninjas
            }
        gameInfoB = GameInfo
            { vsUser = user
            , player = Player.opponent player
            , war    = War.opponent <$> war
            , game = game'
            , ninjas
            }
    return (mvar, gameInfoA, gameInfoB)
  where
    filterDna chars = filter (∉ (Character.ident <$> chars))
