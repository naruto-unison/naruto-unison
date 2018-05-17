-- | Handles API routes and WebSockets related to gameplay.
module Handler.Play
    ( gameSocket
    , getPracticeActR, getPracticeQueueR, getPracticeWaitR
    ) where

import Preludesque

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified STMContainers.Map   as M

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops
import Data.Aeson.Encoding
import Data.ByteString.Lazy   (ByteString)
import Data.Either
import Data.HashMap.Strict    ((!))
import Data.Ix                (inRange)
import Data.Text              (Text)
import Database.Persist.Postgresql
import System.Random
import Yesod
import Yesod.WebSockets

import Calculus
import Core.Fields
import Core.Import
import Game.Structure
import Game.Functions
import Game.Game
import Game.Characters

-- * CONSTANTS

-- micro = 1000000

bot ∷ User
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
getPracticeQueueR ∷ [Text] → Handler Value
getPracticeQueueR team
  | null (drop 2 team) ∨ not (null (drop 3 team))
      = invalidArgs ["Wrong number of characters"]
  | any (not ∘ (`HM.member` cs)) team
      = invalidArgs ["Unknown character(s)"]
  | otherwise = do
      app        ← getYesod
      (who, _)   ← requireAuthPair
      runDB $ update who [ UserTeam =. Just (reverse team) ]
      chakRns    ← randomRs (0, 3) ↤ liftIO newStdGen
      let game   = updateChakra PlayerA (take teamSize chakRns)
                 $ newGame ns who who
      liftIO ∘ atomically ∘ M.insert game who $ appPractice app
      returnJson $ GameInfo who bot PlayerA 0 game
  where oppTeam = ["Naruto Uzumaki", "Tenten", "Sakura Haruno"]
        ns      = (cs !) ↤∘ concat $ transpose [team, oppTeam]

formTeam ∷ [Text] → Maybe [Character]
formTeam team@[a, b, c]
  | duplic team = Nothing
  | otherwise   = do
      a' ← HM.lookup a cs
      b' ← HM.lookup b cs
      c' ← HM.lookup c cs
      return [a', b', c']
formTeam _ = Nothing

formEnact ∷ [Text] → Maybe (Chakras, Chakras, [Act])
formEnact (_:_: _:_:_:_:_) = Nothing -- No more than 3 actions!
formEnact (actChakra:exchangeChakra:acts) = do
    actChakra'      ← fromPathPiece actChakra
    exchangeChakra' ← fromPathPiece exchangeChakra
    acts'           ← formActs acts
    return (actChakra', exchangeChakra', acts')
  where formActs []     = Just []
        formActs (x:xs) = do
            x'  ← fromPathPiece x
            xs' ← formActs xs
            return $ actFromPath x':xs'
formEnact _ = Nothing -- willywonka.gif

sendJson ∷ ToJSON a ⇒ a → WebSocketsT Handler ()
sendJson = sendTextData ∘ encodingToLazyByteString ∘ toEncoding

gameSocket ∷ WebSocketsT Handler ()
gameSocket = do
    sendTextData ("Hello" ∷ ByteString)
    app         ← getYesod
    (who, user) ← requireAuthPair
    teamNames   ← T.split (≡'/') ↤ receiveData

    case formTeam teamNames of
      Nothing   → sendTextData ("Invalid team" ∷ ByteString)
      Just team → do
        flip runSqlPool (appConnPool app) 
          $ update who [UserTeam =. Just (reverse teamNames)]
        (chakRn, stdGen) ← randomR (0,3) ↤ liftIO newStdGen
        let play           = if fst $ random stdGen then PlayerA else PlayerB
            opp            = opponent play
            writeQueueChan = appQueue app
        readQueueChan ← (liftIO ∘ atomically) $ do
          writeTChan writeQueueChan $ Announce who user team
          dupTChan writeQueueChan
        (gameInfo, writeChan, readChan) ← untilJust ∘ liftIO ∘ atomically $ do
          msg ← readTChan readQueueChan
          case msg of
            Respond mWho writeChan readChan gameInfo | mWho ≡ who → 
                return $ Just (gameInfo, writeChan, readChan)
            Announce vsWho vsUser vsTeam → do
              let game' | play ≡ PlayerA = newGame 
                          (concat $ transpose [team, vsTeam]) vsWho who
                        | otherwise = newGame 
                          (concat $ transpose [vsTeam, team]) who vsWho
              let game  = updateChakra PlayerA (take teamSize [chakRn]) game'
              writeChan ← newTChan
              readChan ← newTChan
              writeTChan writeQueueChan ∘ Respond vsWho readChan writeChan
                ∘ GameInfo who user opp 1 $ censor opp game
              return $ Just
                ( GameInfo vsWho vsUser play 0 $ censor play game
                , writeChan
                , readChan
                )
            _ → return Nothing
        sendJson gameInfo
        let player = gamePar gameInfo
        when (player ≡ PlayerA) ∘ tryEnact writeChan player $ gameGame gameInfo
        completedGame ← untilJust $ do
          msg  ← liftIO ∘ atomically $ readTChan readChan
          game ← case msg of
            Enact game → do
              sendJson $ censor player game
              return game
          if isJust $ gameVictor game then return $ Just game else do
              tryEnact writeChan player game
              return Nothing
        sendJson $ censor player completedGame
  where tryEnact gameChan player game = do
          enactText ← receiveData
          case formEnact $ T.split (≡'/') enactText of
            Nothing → sendTextData ("Invalid acts" ∷ ByteString)
            Just (actChakra, exchangeChakra, actions) → do
              stdGen ← liftIO newStdGen 
              case enact player stdGen game actChakra exchangeChakra actions of
                Left errorMsg → sendTextData errorMsg
                Right game' → do 
                  sendJson $ censor player game'
                  liftIO ∘ atomically ∘ writeTChan gameChan $ Enact game'

enact ∷ Player → StdGen → Game → Chakras → Chakras → [Act] → Either Text Game
enact player stdGen game actChakra exchangeChakra actions
  | not ∘ null $ drop teamSize actions = Left "Too many actions"
  | duplic $ actC ↤ actions            = Left "Duplicate actors"
  | any (not ∘ inRange (0, 3)) allS    = Left "Action out of range"
  | randTotal < 0 ∨ lack chakra        = Left "Insufficient chakra"
  | any (illegal player) actions       = Left "Character out of range"
  | otherwise = Right ∘ runTurn player actions stdGen
              $ setGameChakra player chakra { rand = randTotal } game
  where allS      = lefts $ actS ↤ actions
        randTotal = χTotal actChakra - 5 * χTotal exchangeChakra
        chakra    = getGameChakra player game ⧺ exchangeChakra -~ actChakra

-- | Wrapper for 'getPracticeActR' with no actions.
getPracticeWaitR ∷ Chakras → Chakras → Handler Value
getPracticeWaitR actChakra xChakra = getPracticeActR actChakra xChakra []

getPracticeActR ∷ Chakras → Chakras → [ActPath] → Handler Value
getPracticeActR actChakra exchangeChakra actPaths = do
    app      ← getYesod
    (who, _) ← requireAuthPair -- !FAILS!
    let practiceMap = appPractice app
    mGame    ← liftIO ∘ atomically $ M.lookup who practiceMap -- !FAILS
    case mGame of 
      Nothing   → notFound
      Just game → do
        stdGen ← liftIO newStdGen
        case enact PlayerA stdGen game actChakra exchangeChakra actions of
          Left errorMsg → invalidArgs [errorMsg] -- !FAILS!
          Right game'A  → do
            let game'B = runTurn PlayerB botActs stdGen
                       $ setGameChakra PlayerB ø 
                         { rand = 3, nin = 2, gen = 2, blood = 1 } game'A
            liftIO ∘ atomically $ M.insert game'B who practiceMap
            returnJson [censor PlayerA game'A, censor PlayerA game'B]
  where actions = actFromPath ↤ actPaths  
