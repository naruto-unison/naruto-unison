-- | Handles API routes and WebSockets related to gameplay.
module Handler.Play
    ( gameSocket
    , getPracticeActR, getPracticeQueueR, getPracticeWaitR
    ) where

import StandardLibrary

import qualified Data.Aeson.Encoding         as Encoding
import qualified Data.HashMap.Strict         as Map
import qualified Data.Text                   as Text
import qualified STMContainers.Map           as STMMap
import qualified Database.Persist.Postgresql as SQL
import qualified System.Random               as Random
import qualified Yesod.WebSockets            as Socket

import Control.Monad.Loops (untilJust)
import Data.HashMap.Strict ((!))
import Data.Ix (inRange)
import Yesod.WebSockets (WebSocketsT)

import Calculus
import Core.Fields
import Core.Import
import Game.Structure
import Game.Functions
import Game.Game
import Game.Characters

-- * CONSTANTS

--transConcat :: ∀ a. [[a]] -> [[a]]
--transConcat = concat . transpose

-- micro = 1000000

bot :: User
bot = User { userIdent      = ""
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
           , userMuted      = False
           , userCondense   = False
           }

-- * HANDLERS

-- | Joins the practice-match queue with a given team. Requires authentication.
getPracticeQueueR :: [Text] -> Handler Value
getPracticeQueueR team
  | null (drop 2 team) || not (null (drop 3 team)) = 
        invalidArgs ["Wrong number of characters"]
  | any (not . (`Map.member` cs)) team = invalidArgs ["Unknown character(s)"]
  | otherwise = do
      app        <- getYesod
      (who, _)   <- requireAuthPair
      runDB $ update who [ UserTeam =. Just (reverse team) ]
      chakRns    <- Random.randomRs (0, 3) <$> liftIO Random.newStdGen
      let game   = updateChakra PlayerA (take teamSize chakRns) $
                   newGame ns who who
      liftIO . atomically . STMMap.insert game who $ appPractice app
      returnJson $ GameInfo who bot PlayerA 0 game
  where 
    oppTeam = ["Naruto Uzumaki", "Tenten", "Sakura Haruno"]
    ns      = map (cs !) . concat $ transpose [team, oppTeam]

formTeam :: [Text] -> Maybe [Character]
formTeam team@[a, b, c]
  | duplic team = Nothing
  | otherwise   = [[a', b', c'] | a' <- Map.lookup a cs
                                , b' <- Map.lookup b cs
                                , c' <- Map.lookup c cs
                                ]
formTeam _ = Nothing

formEnact :: [Text] -> Maybe (Chakras, Chakras, [Act])
formEnact (_:_: _:_:_:_:_) = Nothing -- No more than 3 actions!
formEnact (actChakra:exchangeChakra:acts) = do
    actChakra'      <- fromPathPiece actChakra
    exchangeChakra' <- fromPathPiece exchangeChakra
    acts'           <- formActs acts
    return (actChakra', exchangeChakra', acts')
  where 
    formActs []     = Just []
    formActs (x:xs) = do
        x'  <- fromPathPiece x
        xs' <- formActs xs
        return $ actFromPath x':xs'
formEnact _ = Nothing -- willywonka.gif

sendJson :: ∀ a. ToJSON a => a -> WebSocketsT Handler ()
sendJson = Socket.sendTextData . Encoding.encodingToLazyByteString . toEncoding

gameSocket :: WebSocketsT Handler ()
gameSocket = do
    Socket.sendTextData ("Hello" :: ByteString)
    app         <- getYesod
    (who, user) <- requireAuthPair
    teamNames   <- Text.split (=='/') <$> Socket.receiveData

    case formTeam teamNames of
      Nothing   -> Socket.sendTextData ("Invalid team" :: ByteString)
      Just team -> do
        flip SQL.runSqlPool (appConnPool app) $ 
            update who [UserTeam =. Just (reverse teamNames)]
        (chakRn, stdGen) <- Random.randomR (0,3) <$> liftIO Random.newStdGen
        let play = if fst $ Random.random stdGen then PlayerA else PlayerB
            opp  = opponent play
            writeQueueChan = appQueue app
        readQueueChan <- (liftIO . atomically) $ do
          writeTChan writeQueueChan $ Announce who user team
          dupTChan writeQueueChan
        (gameInfo, writer, reader) <- untilJust . liftIO . atomically $ do
          msg <- readTChan readQueueChan
          case msg of
            Respond mWho writer reader gameInfo 
              | mWho == who -> return $ Just (gameInfo, writer, reader)
            Announce vsWho vsUser vsTeam -> do
              let game = updateChakra PlayerA (take teamSize [chakRn]) $
                         case play of
                             PlayerA -> newGame 
                                        (concat $ transpose [team, vsTeam]) 
                                        vsWho who
                             _       -> newGame 
                                        (concat $ transpose [vsTeam, team]) 
                                        who vsWho
              writer <- newTChan
              reader <- newTChan
              writeTChan writeQueueChan . Respond vsWho reader writer .
                  GameInfo who user opp 1 $ censor opp game
              return $ Just
                ( GameInfo vsWho vsUser play 0 $ censor play game
                , writer
                , reader
                )
            _ -> return Nothing
        sendJson gameInfo
        let player = gamePar gameInfo
        when (player == PlayerA) . tryEnact writer player $ gameGame gameInfo
        completedGame <- untilJust $ do
          msg  <- liftIO . atomically $ readTChan reader
          game <- case msg of
            Enact game -> do
              sendJson $ censor player game
              return game
          if isJust $ gameVictor game then return $ Just game else do
              tryEnact writer player game
              return Nothing
        sendJson $ censor player completedGame
  where 
    tryEnact gameChan player game = do
        enactText <- Socket.receiveData
        case formEnact $ Text.split (=='/') enactText of
            Nothing -> Socket.sendTextData ("Invalid acts" :: ByteString)
            Just (actChakra, exchangeChakra, actions) -> do
                std <- liftIO Random.newStdGen 
                case enact player std game actChakra exchangeChakra actions of
                    Left errorMsg -> Socket.sendTextData errorMsg
                    Right game' -> do 
                        sendJson $ censor player game'
                        liftIO . atomically . writeTChan gameChan $ Enact game'

enact :: Player -> Random.StdGen -> Game -> Chakras -> Chakras -> [Act] 
      -> Either Text Game
enact player stdGen game actChakra exchangeChakra actions
  | not . null $ drop teamSize actions = Left "Too many actions"
  | duplic $ actC <$> actions            = Left "Duplicate actors"
  | any (not . inRange (0, 3)) allS    = Left "Action out of range"
  | randTotal < 0 || lack chakra        = Left "Insufficient chakra"
  | any (illegal player) actions       = Left "Character out of range"
  | otherwise = Right . runTurn player actions stdGen $
                setGameChakra player chakra { rand = randTotal } game
  where 
    allS      = lefts $ actS <$> actions
    randTotal = χTotal actChakra - 5 * χTotal exchangeChakra
    chakra    = getGameChakra player game + exchangeChakra - actChakra

-- | Wrapper for 'getPracticeActR' with no actions.
getPracticeWaitR :: Chakras -> Chakras -> Handler Value
getPracticeWaitR actChakra xChakra = getPracticeActR actChakra xChakra []

getPracticeActR :: Chakras -> Chakras -> [ActPath] -> Handler Value
getPracticeActR actChakra exchangeChakra actPaths = do
    app      <- getYesod
    (who, _) <- requireAuthPair -- !FAILS!
    let practiceMap = appPractice app
    mGame    <- liftIO . atomically $ STMMap.lookup who practiceMap -- !FAILS
    case mGame of 
      Nothing   -> notFound
      Just game -> do
        stdGen <- liftIO Random.newStdGen
        case enact PlayerA stdGen game actChakra exchangeChakra actions of
          Left errorMsg -> invalidArgs [errorMsg] -- !FAILS!
          Right game'A  -> do
            let game'B = runTurn PlayerB botActs stdGen $
                         setGameChakra PlayerB
                         0{ rand = 3, nin = 2, gen = 2, blood = 1 } game'A
            liftIO . atomically $ STMMap.insert game'B who practiceMap
            returnJson [censor PlayerA game'A, censor PlayerA game'B]
  where 
    actions = actFromPath <$> actPaths  
