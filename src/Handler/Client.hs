{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Interface for the Elm game client.
module Handler.Client
    ( getPlayR
    , getMissionR, ObjectiveProgress(..)
    , getReanimateR
    , getUpdateR
    ) where

import ClassyPrelude
import Yesod

import           Data.Aeson.Encoding (encodingToLazyByteString)
import qualified System.Random.MWC as Random
import           Text.Blaze (Markup)
import qualified Text.Blaze as Markup
import qualified Yesod.Auth as Auth

import qualified Application.App as App
import           Application.Model (EntityField(..), Unlocked(..), User(..))
import           Application.Settings (combineScripts, widgetFile)
import qualified Application.Static as Static
import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character as Character
import           Handler.Client.Data (addDataJS)
import qualified Handler.Play as Play
import qualified Handler.Play.War as War
import qualified Mission
import           Mission.Goal (Goal(Reach))
import qualified Mission.Goal as Goal
import           Util ((∈), (∉), fromMaybeM)

-- | Updates a user's profile and returns it. Requires authentication.
getUpdateR :: Text -> Bool -> Text -> Text -> App.Handler Value
getUpdateR updateName updateCondense updateBackground updateAvatar
  | not $ "/img/icon/" `isPrefixOf` updateAvatar =
      invalidArgs ["Invalid avatar"]
  | any (∉ legalChars) updateName =
      invalidArgs ["Name can only contain letters and numbers"]
  | otherwise = do
    accId <- Auth.requireAuthId
    user  <- runDB $ updateGet accId [ UserName       =. updateName
                                     , UserCondense   =. updateCondense
                                     , UserBackground =. updateBackground''
                                     , UserAvatar     =. updateAvatar
                                     ]
    returnJson user
  where
    legalChars :: HashSet Char
    legalChars = setFromList $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'z']
    updateBackground' = fromMaybe "" $ tailMay updateBackground
    updateBackground''
      | null updateBackground' = Nothing
      | otherwise              = Just updateBackground'

-- | Used for displaying mission progress when viewing a character in the
-- character selection screen.
data ObjectiveProgress = ObjectiveProgress
    { character :: Maybe Text
    , desc      :: Text
    , goal      :: Int
    , progress  :: Int
    } deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON ObjectiveProgress

-- | Returns progress on a character's mission as a list of 'ObjectiveProgress'.
getMissionR :: Character -> App.Handler Value
getMissionR Character{ident} = do
    mission <- Mission.userMission ident
    returnJson $ case mission of
        Just m  -> unzipGoal <$> m
        Nothing -> mempty
  where
    unzipGoal (goal@Reach{desc, reach}, progress) = ObjectiveProgress
        { character = Character.format <$> Goal.character goal
        , goal      = reach
        , desc
        , progress
        }

-- | Buys a Reanimated character with DNA. Returns the new 'UserDna' balance.
getReanimateR :: Character -> App.Handler Value
getReanimateR Character{ident, price} = do
    (who, user) <- Auth.requireAuthPair
    when (userDna user < price)
        $ invalidArgs ["Unaffordable"]
    unlocks <- Mission.unlocked
    when (ident ∈ unlocks)
        $ invalidArgs ["Character already unlocked"]
    charID <- fromMaybeM (invalidArgs ["Character not found"])
            $ Mission.characterID ident
    runDB do
        insertUnique $ Unlocked who charID
        user' <- updateGet who [ UserDna -=. price ]
        returnJson $ userDna user'

-- | Renders the gameplay client.
getPlayR :: App.Handler Html
getPlayR = do
    mauth       <- Auth.maybeAuthPair
    (red,blue)  <- liftIO War.today
    let muser = snd <$> mauth
    PlayParams{bg, practice, team, unlocked} <- getPlayParams muser
    when (isJust muser)
        $ liftIO Random.createSystemRandom >>= runReaderT Play.gameSocket
    setCsrfCookie
    token <- reqToken <$> getRequest
    defaultLayout do
        setTitle "Naruto Unison"
        addStylesheet $ App.StaticR Static.css_embeds_css
        $(combineScripts 'App.StaticR [ Static.js_include_progressbar_min_js
                                      , Static.js_include_soundjs_min_js
                                      ])
        addScript $ App.StaticR Static.js_elm_js
        addDataJS
        $(widgetFile "play/play")
  where
    getPlayParams (Just user) = userPlayParams user <$> Mission.unlocked
    getPlayParams Nothing     = return guestPlayParams
    encodeJSON :: ∀ a. ToJSON a => a -> Markup
    encodeJSON obj = Markup.preEscapedLazyText . decodeUtf8 . encodingToLazyByteString
                   $ toEncoding obj

data PlayParams = PlayParams
    { bg       :: Text
    , practice :: [Text]
    , team     :: [Text]
    , unlocked :: Mission.Unlocks
    }

guestPlayParams :: PlayParams
guestPlayParams = PlayParams
    { bg       = "/img/bg/valley2.jpg"
    , practice = []
    , team     = []
    , unlocked = mempty
    }

userPlayParams :: User -> Mission.Unlocks -> PlayParams
userPlayParams User { userBackground
                    , userPractice
                    , userTeam
                    } unlocked = PlayParams
    { bg       = fromMaybe (bg guestPlayParams) userBackground
    , practice = userPractice
    , team     = maybe [] (filter (∈ unlocked)) userTeam
    , unlocked = unlocked \\ Mission.freeChars
    }
