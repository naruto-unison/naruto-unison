{-# LANGUAGE QuasiQuotes #-}
module Import
    ( module Import
    , module X
    ) where

import Application.Logger    (makeLogWare)
import Application           (makeFoundation)
import ClassyPrelude         as X hiding (delete, deleteBy)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlBackend, SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Application.App       as X
import Application.Fields    as X
import Application.Model     as X
import Test.Hspec            as X
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X
import Yesod.Test            as X
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)

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
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = connEscapeName sqlBackend . DBName <$> tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: ∀ m. MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ unSingle <$> tables

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
