{-# LANGUAGE CPP          #-}
{-# LANGUAGE NoStrictData #-}
-- | Settings loaded from @config/settings.yml@.
module Core.AppSettings (AppSettings(..)) where

import ClassyPrelude

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.!=), (.:), (.:?), FromJSON)
import           Database.Persist.Postgresql (PostgresConf)
import qualified Network.Wai.Handler.Warp as Warp

data AppSettings = AppSettings
    { staticDir              :: String
    -- ^ Directory from which to serve static files.
    , databaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , root                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , host                   :: Warp.HostPreference
    -- ^ Host/interface the server should bind to.
    , port                   :: Int
    -- ^ Port to listen on
    , ipFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , detailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , shouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , reloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , mutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , skipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , copyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , analytics              :: Maybe Text
    -- ^ Google Analytics code

    , authDummyLogin         :: Bool
    -- ^ Indicate if auth dummy login should be enabled.
    }

instance FromJSON AppSettings where
    parseJSON = Aeson.withObject "AppSettings" \o -> do
        let defaultDev =
#ifdef DEVELOPMENT
                True
#else
                False
#endif
        staticDir              <- o .: "static-dir"
        databaseConf           <- o .: "database"
        root                   <- o .:? "approot"
        host                   <- fromString <$> o .: "host"
        port                   <- o .: "port"
        ipFromHeader           <- o .: "ip-from-header"

        dev                       <- o .:? "development"   .!= defaultDev

        detailedRequestLogging <- o .:? "detailed-logging" .!= dev
        shouldLogAll           <- o .:? "should-log-all"   .!= dev
        reloadTemplates        <- o .:? "reload-templates" .!= dev
        mutableStatic          <- o .:? "mutable-static"   .!= dev
        skipCombining          <- o .:? "skip-combining"   .!= dev

        copyright              <- o .:  "copyright"
        analytics              <- o .:? "analytics"

        authDummyLogin         <- o .:? "auth-dummy-login" .!= dev

        return AppSettings {..}
