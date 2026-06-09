{-# LANGUAGE QuasiQuotes #-}
module Import
    ( module Import
    , module Import'
    ) where

import ClassyPrelude as Import' hiding (Handler, delete, deleteBy)

import Application.App as Import'
import Application.Model as Import'
import Database.Persist as Import' hiding (PersistEntity, get)
import Test.Hspec as Import'
import Yesod.Auth as Import'
import Yesod.Test as Import'

import Application (makeFoundation)
import Application.Logger (makeLogWare)
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle)
import Text.Shakespeare.Text (st)
import Yesod.Core.Unsafe (fakeHandlerGetLogger)
import Yesod.Default.Config2 (useEnv, loadYamlSettings)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (connPool app)

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger logger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp = before do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app do
    tables <- rawSql [st|
        SELECT '"' || table_name || '"'
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []
    let query = "TRUNCATE TABLE " ++ intercalate ", " (unSingle <$> tables)
    rawExecute query []

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl $ AuthR $ PluginR "dummy" []

createUser :: Privilege -> Text -> YesodExample App (Entity User)
createUser userPrivilege ident = runDB $ insertEntity user { userPrivilege }
  where
    user = newUser ident Nothing $ ModifiedJulianDay 0
