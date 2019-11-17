{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Interface for the Elm game client.
module Handler.Client
    ( getPlayR
    , getMissionR, ObjectiveProgress(..)
    , getMuteR
    , getReanimateR
    , getUpdateR
    ) where

import ClassyPrelude
import Yesod

import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.List (nub)
import qualified System.Random.MWC as Random
import qualified Yesod.Auth as Auth

import           Application.App (Handler)
import           Application.Model (EntityField(..), Unlocked(..), User(..))
import           Application.Settings (widgetFile)
import qualified Game.Characters as Characters
import           Game.Model.Character (Character)
import qualified Game.Model.Character as Character
import qualified Game.Model.Class as Class
import qualified Game.Model.Skill as Skill
import qualified Handler.Play as Play
import qualified Handler.Play.War as War
import qualified Mission
import           Mission.Goal (Goal)
import qualified Mission.Goal as Goal
import           Util ((<$><$>), (∈), (∉), shorten)

-- | Updates a user's profile and returns it. Requires authentication.
getUpdateR :: Text -> Bool -> Text -> Text -> Handler Value
getUpdateR updateName updateCondense updateBackground updateAvatar
  | not $ "/img/icon/" `isPrefixOf` updateAvatar =
      invalidArgs ["Invalid avatar"]
  | any (∉ legalChars) updateName =
      invalidArgs ["Name can only contain letters and numbers"]
  | otherwise = do
    accId <- Auth.requireAuthId
    user  <- runDB $ updateGet accId [ UserName      =. updateName
                                    , UserCondense   =. updateCondense
                                    , UserBackground =. updateBackground''
                                    , UserAvatar     =. updateAvatar
                                    ]
    returnJson user
  where
    legalChars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'z']
    updateBackground'  = fromMaybe "" $ tailMay updateBackground
    updateBackground''
      | null updateBackground' = Nothing
      | otherwise              = Just updateBackground'

-- | Used for displaying mission progress when viewing a character in the
-- character selection screen.
data ObjectiveProgress =
    ObjectiveProgress { character :: Maybe Text
                      , desc      :: Text
                      , goal      :: Int
                      , progress  :: Int
                      } deriving (Eq, Ord, Show, Read, Generic, ToJSON)

-- | Unpacks the output of 'Mission.userMission'.
unzipGoal :: (Goal, Int) -> ObjectiveProgress
unzipGoal (goal, progress) =
    ObjectiveProgress { character = Character.format <$> Goal.character goal
                      , desc      = Goal.desc goal
                      , goal      = Goal.reach goal
                      , progress
                      }

-- | Returns progress on a character's mission as a list of 'ObjectiveProgress'.
getMissionR :: Character -> Handler Value
getMissionR char =
    returnJson . maybe mempty (unzipGoal <$>)
    =<< Mission.userMission (Character.ident char)

-- | Updates a user's muted status and returns it. Requires authentication.
getMuteR :: Bool -> Handler Value
getMuteR mute = do
    who <- Auth.requireAuthId
    runDB $ update who [ UserMuted =. mute ]
    returnJson mute

-- | Buys a Reanimated character with DNA. Returns the new 'UserDna' balance.
getReanimateR :: Character -> Handler Value
getReanimateR char = do
    (who, user) <- Auth.requireAuthPair
    when (userDna user < price) $
        invalidArgs ["Unaffordable"]
    unlocks <- Mission.unlocked
    when (Character.ident char ∈ unlocks) $
        invalidArgs ["Character already unlocked"]
    mCharID <- runMaybeT . Mission.characterID $ Character.ident char
    case mCharID of
        Nothing -> invalidArgs ["Character not found"]
        Just charID -> runDB do
            insertUnique $ Unlocked who charID
            user' <- updateGet who [UserDna -=. price]
            returnJson $ userDna user'
  where
    price = Character.price char

-- | Renders the gameplay client.
getPlayR :: Handler Html
getPlayR = do
    muser       <- snd <$><$> Auth.maybeAuthPair
    unlocked    <- Mission.unlocked
    (red,blue)  <- liftIO War.today
    when (isJust muser) $
        liftIO Random.createSystemRandom >>= runReaderT Play.gameSocket
    let team     = maybe []
                   (mapMaybe Characters.lookup . filter (∈ unlocked)) $
                   muser >>= userTeam
        practice = maybe [] (mapMaybe Characters.lookup . userPractice)
                   muser
        bg       = fromMaybe "/img/bg/valley2.jpg" $ muser >>= userBackground
        vol :: Text
        vol
          | isMuted muser = "click muted"
          | otherwise     = "click unmuted"
    setCsrfCookie
    token <- reqToken <$> getRequest
    defaultLayout do
        setTitle "Naruto Unison"
        addStylesheetRemote "/css/embeds.css"
        $(widgetFile "include/progressbar.min")
        $(widgetFile "include/soundjs.min")
        $(widgetFile "include/normalize")
        $(widgetFile "play/elm")
        $(widgetFile "play/play")

-- | Icons from all of a character's skills.
charAvatars :: Character -> [Text]
charAvatars char = toFile <$> "icon" : skills
  where
    skills      = nub $ Skill.name <$> concatMap toList (Character.skills char)
    toFile path = "/img/ninja/" ++ Character.ident char ++ "/"
                  ++ shorten path ++ ".jpg"

-- | Icons that users can set as their avatars.
avatars :: Value
avatars = toJSON $ icons ++ concatMap charAvatars Characters.list
  where
    icons = ("/img/icon/" ++) <$>
        [ "default.jpg"
        , "gaaraofthefunk.jpg"
        , "ninjainfocards.jpg"
        , "kabugrin.jpg"
        ]
{-# NOINLINE avatars #-}

isMuted :: Maybe User -> Bool
isMuted = maybe False userMuted
