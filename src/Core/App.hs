{-# LANGUAGE NoStrictData    #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Yesod app instance. @stack new@ was used to generate boilerplate.
module Core.App
  ( App(..)
  , Form
  , Handler, Widget
  , Route(..)
  , AppPersistEntity
  , unsafeHandler
  , resourcesApp

  , newUser
  ) where

import ClassyPrelude
import Yesod

import           Control.Monad.Logger (LogSource)
import qualified Data.CaseInsensitive as CaseInsensitive
import           Data.Cache (Cache)
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import qualified Database.Persist.Sql as Sql
import           Database.Persist.Sql (ConnectionPool, SqlBackend, SqlPersistT)
import           Network.HTTP.Client.Conduit (HasHttpManager(..), Manager)
import qualified Network.Mail.Mime as Mail
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import           Text.Hamlet (hamletFile)
import qualified Text.Jasmine as Jasmine
import           Text.Shakespeare.Text (stext)
import qualified Yesod.Auth as Auth
import           Yesod.Auth (Auth, YesodAuth(..), YesodAuthPersist, AuthenticationResult(..), AuthPlugin)
import qualified Yesod.Auth.Dummy as Dummy
import qualified Yesod.Auth.Email as AuthEmail
import           Yesod.Auth.Email (YesodAuthEmail(..))
import           Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Yesod.Default.Util as YesodUtil
-- Used only when in "auth-dummy-login" setting is enabled.
import           Yesod.Static hiding (static)

import           Core.Fields (ForumBoard, Privilege(..), boardName)
import qualified Core.Queue as Queue
import           Core.Wrapper (Wrapper)
import           Core.Model (EntityField(..), Topic(..), TopicId, User(..), UserId, Unique(..))
import qualified Core.AppSettings as AppSettings
import           Core.AppSettings (AppSettings)
import           Core.Settings (widgetFile)
import           Model.Act (Act)
import           Model.Chakra (Chakras)
import qualified Model.Character as Character
import           Model.Character (Category)
import qualified Characters

-- | App environment.
data App = App
    { settings    :: AppSettings
      -- ^ Settings loaded from a local file.
    , static      :: Static
      -- ^ Server for static files.
    , connPool    :: ConnectionPool
      -- ^ Database connection.
    , httpManager :: Manager
      -- ^ Web request manager.
    , logger      :: Logger
      -- ^ See https://www.yesodweb.com/blog/2014/01/new-fast-logger
    , practice    :: Cache (Key User) Wrapper
      -- ^ Saved state of Practice Games. Games expire after one hour or as soon
      -- as they yield a victor.
      -- All other games are stored in their websocket threads.
    , queue       :: TChan Queue.Message
      -- ^ Broadcast channel for users to queue and be matched with each other.
    }

-- Generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | The set of constraints for persisted types.
type AppPersistEntity a = ( PersistEntity a
                          , PersistRecordBackend a
                            (BaseBackend (YesodPersistBackend App))
                          )

getNavLinks :: Handler [(Route App, Html)]
getNavLinks = do
    showAdmin <- isAuthenticated Moderator
    return $ admin showAdmin
      [ (HomeR,       "Home")
      , (CharactersR, "Characters")
      , (ForumsR,     "Forums")
      ]
  where
    admin Authorized xs = xs ++ [(AdminR, "Admin")]
    admin _          xs = xs

origin :: Route App -> Route App
origin ChangelogR = HomeR
origin ProfileR{} = ForumsR
origin BoardR{} = ForumsR
origin NewTopicR{} = ForumsR
origin TopicR{} = ForumsR
origin CharacterR{} = CharactersR
origin x = x

instance Yesod App where
    approot :: Approot App
    approot = ApprootRequest \app req ->
        fromMaybe (getApprootText guessApproot app req) .
        AppSettings.root $ settings app

    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120 -- Timeout in minutes of session data stored in encrypted cookies
        "config/client_session_key.aes"

    --yesodMiddleware :: ∀ res. ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master           <- getYesod
        mmsg             <- getMessage
        mcurrentRoute    <- getCurrentRoute
        (title, parents) <- breadcrumbs
        muser            <- (entityVal <$>) <$> Auth.maybeAuth
        navLinks         <- getNavLinks

        pc <- widgetToPageContent do
            setTitle . toHtml $ title ++ " - Naruto Unison"
            $(widgetFile "include/cookie.min")
            $(widgetFile "include/jquery.min")
            $(widgetFile "include/main")
            $(widgetFile "include/normalize")
            $(widgetFile "default-layout/default-layout")
        withUrlRenderer
          $(hamletFile "templates/default-layout/default-layout-wrapper.hamlet")

    authRoute :: App -> Maybe (Route App)
    authRoute _ = Just $ AuthR Auth.LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    isAuthorized AdminR _ = isAuthenticated Moderator
    -- isAuthorized PlayR _ = isAuthenticated Normal
    -- Routes not requiring authentication.
    isAuthorized _     _ = return Authorized

    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        staticDir <- getsYesod $ AppSettings.staticDir . settings
        YesodUtil.addStaticContentExternal
            Jasmine.minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        genFileName lbs = "autogen-" ++ base64md5 lbs

    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $ AppSettings.shouldLogAll (settings app)
                 || level == LevelWarn
                 || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . logger

instance YesodBreadcrumbs App where
  breadcrumb (AuthR _) = return ("Login", Just HomeR)
  breadcrumb AdminR = return ("Admin", Just HomeR)
  breadcrumb ChangelogR = return ("Changelog", Just HomeR)
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb (ProfileR name) = return ("User: " ++ name, Just HomeR)
  breadcrumb ForumsR = return ("Forums", Just HomeR)
  breadcrumb (BoardR board) = return (boardName board, Just ForumsR)
  breadcrumb (NewTopicR board) = return ("New Topic", Just $ BoardR board)
  breadcrumb (TopicR topic) = do
      Topic{topicTitle, topicBoard} <- runDB $ get404 topic
      return (topicTitle, Just $ BoardR topicBoard)
  breadcrumb (CharacterR category char) = return
      ( maybe "Character" Character.format $ Characters.lookupSite category char
      , Just CharactersR
      )
  breadcrumb (TopicR topic) = do
  breadcrumb _ = return (mempty, Nothing)

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: ∀ a. SqlPersistT Handler a -> Handler a
    runDB action = Sql.runSqlPool action =<< getsYesod connPool

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner connPool

newUser :: Text -> Maybe Text -> Day -> User
newUser ident verkey day = User
    { userIdent      = ident
    , userPassword   = Nothing
    , userVerkey     = verkey
    , userVerified   = False
    , userJoined     = day
    , userPrivilege  = Normal
    , userName       = ident
    , userAvatar     = "/img/icon/default.jpg"
    , userBackground = Nothing
    , userXp         = 0
    , userWins       = 0
    , userLosses     = 0
    , userStreak     = 0
    , userRecord     = 0
    , userClan       = Nothing
    , userTeam       = Nothing
    , userPractice   = ["Naruto Uzumaki", "Sakura Haruno", "Sasuke Uchiha"]
    , userMuted      = False
    , userCondense   = False
    , userRating     = 0.0
    , userDeviation  = 350.0 / 173.7178
    , userVolatility = 0.06
    }

instance YesodAuth App where
    type AuthId App = UserId

    loginDest :: App -> Route App
    loginDest _ = PlayR
    logoutDest :: App -> Route App
    logoutDest _ = PlayR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: ∀ m. (MonadHandler m, App ~ HandlerSite m)
                 => Auth.Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB do
        x <- getBy $ UniqueUser ident
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$>
                (insert =<<
                 newUser ident Nothing . utctDay <$> liftIO getCurrentTime)
      where
        ident = Auth.credsIdent creds
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = AuthEmail.authEmail : extraAuthPlugins
        -- Enable authDummy login if enabled.
        where
          extraAuthPlugins =
              [Dummy.authDummy | AppSettings.authDummyLogin $ settings app]

isAuthenticated :: Privilege -> Handler AuthResult
isAuthenticated level = do
    muser <- Auth.maybeAuth
    return case muser of
        Just (Entity _ user)
          | userPrivilege user >= level -> Authorized
        Just _  -> Unauthorized "You do not have access to this page"
        Nothing -> Unauthorized "You must login to access this page"

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = httpManager

unsafeHandler :: ∀ a. App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger logger

instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    afterPasswordRoute _ = PlayR

    addUnverified email verkey = do
        liftHandler . runDB . insert .
            newUser email (Just verkey) . utctDay =<< liftIO getCurrentTime

    sendVerifyEmail email _ verurl =
        liftIO $ Mail.renderSendMail
            (Mail.emptyMail $ Mail.Address Nothing "noreply")
                { Mail.mailTo      = [Mail.Address Nothing email]
                , Mail.mailHeaders = [("Subject", "Verify your email address")]
                , Mail.mailParts   = [[vtextPart, vhtmlPart]]
                }
      where
        vtextPart = Mail.Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = Mail.None
            , partDisposition = Mail.DefaultDisposition
            , partContent = Mail.PartContent $ LazyEncoding.encodeUtf8 [stext|
Welcome to Naruto Unison! To confirm your email address, click on the link below or copy and paste it into your address bar.
\#{verurl}
|]
            , partHeaders = []
            }
        vhtmlPart = Mail.Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = Mail.None
            , partDisposition = Mail.DefaultDisposition
            , partContent = Mail.PartContent $ Blaze.renderHtml [shamlet|
<p>Welcome to Naruto Unison! To confirm your email address, click on the link below or copy and paste it into your address bar.
<p>
    <a href=#{verurl}>#{verurl}
|]
            , partHeaders = []
            }
    getVerifyKey = liftHandler . runDB . fmap (join . fmap userVerkey) . get
    setVerifyKey uid key = liftHandler . runDB $ update uid [UserVerkey =. Just key]
    verifyAccount uid = liftHandler $ runDB do
        mu <- get uid
        case mu of
          Nothing -> return Nothing
          Just _  -> do
                update uid [UserVerified =. True]
                return $ Just uid
    getPassword = liftHandler . runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = liftHandler . runDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = liftHandler $ runDB do
        mu <- getBy . UniqueUser $ toLower email
        return $ mu <&> \(Entity uid u) -> AuthEmail.EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                , emailCredsEmail = toLower email
                }
    getEmail = liftHandler . runDB . fmap (fmap userIdent) . get
