{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

-- | Yesod foundation. Implemented in 'Application'.
module Foundation where

import Preludesque

import qualified Data.CaseInsensitive    as CI
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy.Encoding
import qualified Yesod.Core.Unsafe       as Unsafe

import Control.Monad                 (join)
import Control.Monad.Logger          (LogSource)
import Control.Concurrent.STM
import Data.Text                     (Text, toLower)
import Database.Persist.Sql          (ConnectionPool, runSqlPool)
import Network.Mail.Mime
import STMContainers.Map             (Map)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet                   (hamletFile, shamlet)
import Text.Jasmine                  (minifym)
import Text.Shakespeare.Text         (stext)
import Yesod.Auth                    
import Yesod.Auth.Dummy
import Yesod.Auth.Email
import Yesod.Core.Types              (Logger)
import Yesod.Default.Util            (addStaticContentExternal)

import Core.Fields
import Core.NoFoundation 
import Game.Structure
import Game.JSON ()

data GameInfo = GameInfo { gameVsWho     ∷ Key User
                         , gameVsUser    ∷ User
                         , gamePar       ∷ Player
                         , gameLeft      ∷ Int
                         , gameGame      ∷ Game
                         }
instance ToJSON GameInfo where
    toJSON GameInfo {..} = object
        [ "opponentAccount" .= gameVsUser
        , "par"             .= gamePar
        , "game"            .= gameGame
        , "left"            .= gameLeft
        , "userTeam"        .= team alliesP
        , "opponentTeam"    .= team enemiesP
        , "gameVsUser"      .= gameVsUser
        , "gamePar"         .= gamePar
        , "gameLeft"        .= gameLeft
        , "gameGame"        .= gameGame
        , "gameCharacters"  .= (nCharacter ↤ gameNinjas gameGame)
        ]
      where team f = (characterName ∘ nCharacter) 
                   ↤ f gamePar (gameNinjas gameGame)

newtype GameMessage = Enact Game

data QueueMessage = Announce (Key User) User [Character]
                  | Respond  (Key User) (TChan GameMessage) (TChan GameMessage) GameInfo

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    ∷ AppSettings
    , appStatic      ∷ Static
    , appConnPool    ∷ ConnectionPool
    , appHttpManager ∷ Manager
    , appLogger      ∷ Logger
    , appPractice    ∷ Map (Key User) Game
    , appQueue       ∷ TChan QueueMessage
    }

data MenuItem = MenuItem
    { menuItemLabel ∷ Text
    , menuItemRoute ∷ Route App
    , menuItemAccessCallback ∷ Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- Generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFileNoCheck "config/routes") 

-- | A convenient synonym for database access functions.
type DB a = forall (m ∷ * → *).
    (MonadIO m) ⇒ ReaderT SqlBackend m a
   
instance Yesod App where
    approot ∷ Approot App
    approot = ApprootRequest $ \app@App{..} req →
        fromMaybe (getApprootText guessApproot app req) ∘ appRoot $ appSettings

    makeSessionBackend ∷ App → IO (Maybe SessionBackend)
    makeSessionBackend _ = Just ↤ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    yesodMiddleware ∷ ToTypedContent res ⇒ Handler res → Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout ∷ Widget → Handler Html
    defaultLayout widget = do
        master ← getYesod
        mmsg ← getMessage
        --muser ← maybeAuthPair
        mcurrentRoute ← getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) ← breadcrumbs
        pc ← widgetToPageContent $ do
            $(widgetFile "include/cookie.min")
            $(widgetFile "include/jquery.min")
            $(widgetFile "include/main")
            $(widgetFile "include/normalize")
            $(widgetFile "default-layout/default-layout")
        withUrlRenderer
          $(hamletFile "templates/default-layout/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    --authRoute _ = Just $ AuthR LoginR
    --authRoute ∷ App → Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    --isAuthorized ProfileR _ = isAuthenticated
    --isAuthorized PlayR _ = isAuthenticated

    isAuthorized
        ∷ Route App  -- ^ The route the user is visiting.
        → Bool       -- ^ Whether or not this is a "write" request.
        → Handler AuthResult
    isAuthorized _ _ = return Authorized

    -- Creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master ← getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR ∘ flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ⧺ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO ∷ App → LogSource → LogLevel → IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger ∷ App → IO Logger
    makeLogger = return ∘ appLogger

instance YesodBreadcrumbs App where
  breadcrumb (AuthR _) = return ("Login", Just PlayR)
  breadcrumb ChangelogR = return ("Changelog", Just PlayR)
  breadcrumb  _ = return ("Home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    --runDB ∷ SqlPersistT Handler a → Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner ∷ Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

newUser ∷ Text → Maybe Text → User
newUser ident verkey = User { userIdent      = ident
                            , userPassword   = Nothing
                            , userVerkey     = verkey
                            , userVerified   = False
                            , userPrivilege  = Normal
                            , userName       = ident
                            , userAvatar     = "/img/icon/default.jpg"
                            , userBackground = Nothing
                            , userXp         = 0
                            , userWins       = 0
                            , userLosses     = 0
                            , userStreak     = 0
                            , userClan       = Nothing
                            , userTeam       = Nothing
                            , userMuted      = False
                            }

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = PlayR
    logoutDest _ = PlayR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate ∷ (MonadHandler m, HandlerSite m ~ App)
                 ⇒ Creds App → m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUser ident
        case x of
            Just (Entity uid _) → return $ Authenticated uid
            Nothing → Authenticated ↤ insert (newUser ident Nothing)
      where ident = credsIdent creds
    authPlugins app = authEmail
                    : extraAuthPlugins
        where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

isAuthenticated ∷ Handler AuthResult
isAuthenticated = do
    muid ← maybeAuthId
    return $ case muid of
        Nothing → Unauthorized "You must login to access this page"
        Just _ → Authorized

instance YesodAuthPersist App

-- Modify for customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler ∷ App → Handler a → IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    afterPasswordRoute _ = PlayR

    addUnverified email verkey = liftHandler $
        runDB $ insert $ newUser email (Just verkey)

    sendVerifyEmail email _ verurl =
        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[vtextPart, vhtmlPart]]
            }
      where
        vtextPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8 [stext|
Welcome to Naruto Unison! To confirm your email address, click on the link below or copy and paste it into your address bar.

\#{verurl}
|]
            , partHeaders = []
            }
        vhtmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml [shamlet|
<p>Welcome to Naruto Unison! To confirm your email address, click on the link below or copy and paste it into your address bar.
<p>
    <a href=#{verurl}>#{verurl}
|]
            , partHeaders = []
            }
    getVerifyKey = liftHandler ∘ runDB ∘ fmap (join ∘ fmap userVerkey) ∘ get
    setVerifyKey uid key = liftHandler ∘ runDB $ update uid [UserVerkey =. Just key]
    verifyAccount uid = liftHandler ∘ runDB $ do
        mu ← get uid
        case mu of 
          Nothing → return Nothing
          Just _  → do
                update uid [UserVerified =. True]
                return $ Just uid
    getPassword = liftHandler ∘ runDB ∘ fmap (join ∘ fmap userPassword) ∘ get
    setPassword uid pass = liftHandler ∘ runDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = liftHandler ∘ runDB $ do
        mu ← getBy ∘ UniqueUser $ toLower email
        return $ mu ↦ \(Entity uid u) → EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                , emailCredsEmail = toLower email
                }
    getEmail = liftHandler ∘ runDB ∘ fmap (fmap userIdent) ∘ get
