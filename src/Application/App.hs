{-# LANGUAGE CPP             #-}
{-# LANGUAGE NoStrictData    #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Yesod app instance. @stack new@ was used to generate boilerplate.
module Application.App
  ( App(..)
  , Form
  , Handler, Widget
  , Route(..)
  , AppPersistEntity
  , getPrivilege
  , liftDB
  , unchanged304
  , lastModified
  , unsafeHandler
  , resourcesApp
  ) where

import ClassyPrelude
import Yesod

import           Control.Monad.Logger (LogSource)
import           Data.Bimap (Bimap)
import           Data.Cache (Cache)
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import qualified Data.Time.Format as Format
import qualified Data.Time.LocalTime as LocalTime
import qualified Database.Persist.Sql as Sql
import           Database.Persist.Sql (ConnectionPool, SqlBackend, SqlPersistT)
import           Network.HTTP.Client.Conduit (HasHttpManager(..), Manager)
import qualified Network.Mail.Mime as Mail
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import           Text.Hamlet (hamletFile)
import qualified Text.Jasmine as Jasmine
import           Text.Read (read)
import           Text.Shakespeare.Text (stext)
import           Yesod.Auth (Auth, YesodAuth(..), YesodAuthPersist, AuthenticationResult(..), AuthPlugin)
import qualified Yesod.Auth as Auth
import qualified Yesod.Auth.Dummy as Dummy
import qualified Yesod.Auth.Email as AuthEmail
import           Yesod.Auth.Email (YesodAuthEmail(..))
import           Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Yesod.Default.Util as YesodUtil
-- Used only when [auth-dummy-login](config/settings.yml) setting is enabled.
import           Yesod.Static hiding (static)

import           Application.Fields (ForumBoard, Privilege(..), boardName)
import           Application.Model (CharacterId, EntityField(..), ForumPostId, ForumTopic(..), ForumTopicId, User(..), UserId, Unique(..))
import qualified Application.Model as Model
import           Application.Settings (Settings, widgetFile)
import qualified Application.Settings as Settings
import           Game.Model.Act (Act)
import           Game.Model.Chakra (Chakras)
import           Game.Model.Character (Character)
import qualified Game.Model.Character as Character
import qualified Handler.Play.Queue as Queue
import           Handler.Play.Wrapper (Wrapper)
import           OrphanInstances.Character ()
import           Util ((<$><$>), (<$>.))

#ifndef DEVELOPMENT
import           Class.Display (display')
#endif

-- | App environment.
data App = App
    { startup     :: UTCTime
    , timestamp   :: Int64
    , settings    :: Settings
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
    , queue        :: TChan Queue.Message
      -- ^ Broadcast channel for users to queue and be matched with each other.
    , characterIDs :: Bimap CharacterId Text
    }

-- Generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

getPrivilege :: ∀ m. (MonadHandler m, App ~ HandlerSite m) => m Privilege
getPrivilege = liftHandler . cached $
               maybe Guest (userPrivilege . snd) <$> Auth.maybeAuthPair

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | The set of constraints for persisted types.
type AppPersistEntity a = ( PersistEntity a
                          , PersistRecordBackend a
                            (BaseBackend (YesodPersistBackend App))
                          )

getNavLinks :: Handler [(Route App, Html)]
getNavLinks = do
    showAdmin <- isAuthenticated Admin
    return $ admin showAdmin
      [ (HomeR,   "Home")
      , (GuideR,  "Guide")
      , (ForumsR, "Forums")
      ]
  where
    admin Authorized xs = xs ++ [(AdminR, "Admin")]
    admin _          xs = xs

origin :: Route App -> Route App
origin BoardR{}     = ForumsR
origin ChangelogR   = HomeR
origin CharacterR{} = GuideR
origin CharactersR  = GuideR
origin GroupsR      = GuideR
origin MechanicsR   = GuideR
origin NewTopicR{}  = ForumsR
origin ProfileR{}   = ForumsR
origin TopicR{}     = ForumsR
origin UsageR       = AdminR
origin x            = x

-- | This function should only be used in handlers with completely static
-- content. It sets an
-- [ETag](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/ETag)
-- based on the time when the server started up and the user currently logged
-- in, if there is one. The server respects
-- [If-None-Match](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-None-Match):
-- if the client sends the same ETag, the server sends 304 Not Modified instead
-- of the web page's body. The client then loads their cached version of the
-- page.
-- Correct usage of this function can save a lot of bandwidth.
-- Incorrect usage will lead to clients seeing outdated cached versions of pages
-- because the server refuses to send them updates.
unchanged304 :: Handler ()
#ifdef DEVELOPMENT
unchanged304 = return ()
#else
unchanged304 = whenM (isNothing <$> getMessage) $
               setEtag . toStrict . display'
               =<< maybeAdd <$> getsYesod timestamp <*> maybeAuthId
  where
    maybeAdd x = maybe x $ (+ x) . Sql.fromSqlKey
#endif

-- | Sets the
-- [Last-Modified](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Last-Modified)
-- header and
-- [ETag](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/ETag)
-- to a given time.
-- As with 'unchanged304', the browser can respond with its latest timestamp.
-- If the timestamp matches, the server sends Unchanged 304.
lastModified :: UTCTime -> Handler ()
lastModified time = do
    timestamp <- max time <$> getsYesod startup
    whenM (isNothing <$> getMessage) . setEtag $ tshow timestamp
    replaceOrAddHeader "Last-Modified" . pack $ formatAsLastModified timestamp

formatAsLastModified :: UTCTime -> String
formatAsLastModified time = Format.formatTime Format.defaultTimeLocale
                            "%a, %d %b %Y %H:%M:%S GMT" $
                            LocalTime.utcToLocalTime (read "GMT") time

instance Yesod App where
    approot :: Approot App
    approot = ApprootRequest \app req ->
        fromMaybe (getApprootText guessApproot app req) .
        Settings.root $ settings app

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
        muser            <- snd <$><$> Auth.maybeAuthPair
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
    isAuthorized AdminR _ = isAuthenticated Admin
    isAuthorized UsageR _ = isAuthenticated Admin
    -- isAuthorized PlayR _ = isAuthenticated Normal
    -- Routes not requiring authentication.
    isAuthorized _     _ = return Authorized

    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        staticDir <- getsYesod $ Settings.staticDir . settings
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
    shouldLogIO _ _ LevelWarn  = return True
    shouldLogIO _ _ LevelError = return True
    shouldLogIO app _ _        = return . Settings.shouldLogAll $ settings app

    makeLogger :: App -> IO Logger
    makeLogger = return . logger

instance YesodBreadcrumbs App where
    breadcrumb AdminR         = return ("Admin", Just HomeR)
    breadcrumb AuthR{}        = return ("Login", Just HomeR)
    breadcrumb (BoardR x)     = return (boardName x, Just ForumsR)
    breadcrumb ChangelogR     = return ("Changelog", Just HomeR)
    breadcrumb (CharacterR x) = return (Character.format x, Just CharactersR)
    breadcrumb CharactersR    = return ("Characters", Just GuideR)
    breadcrumb ForumsR        = return ("Forums", Just HomeR)
    breadcrumb GroupsR        = return ("Groups", Just GuideR)
    breadcrumb GuideR         = return ("Guide", Just HomeR)
    breadcrumb HomeR          = return ("Home", Nothing)
    breadcrumb MechanicsR     = return ("Game Mechanics", Just GuideR)
    breadcrumb (NewTopicR x)  = return ("New Topic", Just $ BoardR x)
    breadcrumb PlayR          = return (mempty, Nothing)
    breadcrumb (ProfileR x)   = return ("User: " ++ x, Just HomeR)
    breadcrumb UsageR         = return ("Character Usage", Just AdminR)
    breadcrumb (TopicR x)     = do
        ForumTopic{forumTopicTitle, forumTopicBoard} <- runDB $ get404 x
        return (forumTopicTitle, Just $ BoardR forumTopicBoard)

    breadcrumb FaviconR = return (mempty, Nothing)
    breadcrumb RobotsR = return (mempty, Nothing)
    breadcrumb StaticR{} = return (mempty, Nothing)

    -- API routes.
    breadcrumb DeleteTopicR{} = return (mempty, Nothing)
    breadcrumb LockTopicR{} = return (mempty, Nothing)
    breadcrumb DeletePostR{} = return (mempty, Nothing)
    breadcrumb LikePostR{} = return (mempty, Nothing)
    breadcrumb MissionR{} = return (mempty, Nothing)
    breadcrumb MuteR{} = return (mempty, Nothing)
    breadcrumb PracticeActR{} = return (mempty, Nothing)
    breadcrumb PracticeQueueR{} = return (mempty, Nothing)
    breadcrumb PracticeWaitR{} = return (mempty, Nothing)
    breadcrumb ReanimateR{} = return (mempty, Nothing)
    breadcrumb UpdateR{} = return (mempty, Nothing)

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: ∀ a. SqlPersistT Handler a -> Handler a
    runDB action = Sql.runSqlPool action =<< getsYesod connPool
    {-# INLINE runDB #-}

liftDB :: ∀ m a. (MonadHandler m, App ~ HandlerSite m)
       => SqlPersistT Handler a -> m a
liftDB = liftHandler . runDB
{-# INLINE liftDB #-}

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner connPool

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
    authenticate creds = liftDB do
        x <- getBy $ UniqueUser ident
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> do
                UTCTime day _ <- liftIO getCurrentTime
                who           <- insert $ Model.newUser ident Nothing day
                return $ Authenticated who
      where
        ident = Auth.credsIdent creds
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = AuthEmail.authEmail : extraAuthPlugins
        -- Enable authDummy login if enabled.
        where
          extraAuthPlugins =
              [Dummy.authDummy | Settings.authDummyLogin $ settings app]

isAuthenticated :: Privilege -> Handler AuthResult
isAuthenticated level = do
    muser <- Auth.maybeAuthPair
    return case muser of
        Just (_, user)
          | userPrivilege user >= level -> Authorized
        -- Should this maybe just return a 404?
        Just _  -> Unauthorized "You are not authorized to access this page."
        Nothing -> Unauthorized "You must login to access this page."

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
        UTCTime day _ <- liftIO getCurrentTime
        liftDB . insert $ Model.newUser email (Just verkey) day

    sendVerifyEmail email _ verurl =
        liftIO do
            putStrLn $ "VERIFICATION LINK: " ++ verurl
            Mail.renderSendMail
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
    getVerifyKey = liftDB . (join . (userVerkey <$>) <$>) . get
    setVerifyKey uid key = liftDB $ update uid [UserVerkey =. Just key]
    verifyAccount uid = liftDB do
        mu <- get uid
        case mu of
          Nothing -> return Nothing
          Just _  -> do
                update uid [UserVerified =. True]
                return $ Just uid
    getPassword = liftDB . (join . (userPassword <$>) <$>) . get
    setPassword uid pass = liftDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = liftDB do
        mu <- getBy . UniqueUser $ toLower email
        return $ mu <&> \(Entity uid u) -> AuthEmail.EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                , emailCredsEmail = toLower email
                }
    getEmail = liftDB . (userIdent <$>) <$>. get
