{-# LANGUAGE CPP               #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Yesod settings.
module Core.Settings
  ( configSettingsYmlValue
  , widgetFile
  ) where

import ClassyPrelude

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import           Data.Aeson (Result(..), Value)
import           Data.Default (def)
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Yaml as Yaml
import qualified Language.Haskell.TH.Syntax as TH
import qualified Yesod.Default.Config2 as DefaultConfig
import qualified Yesod.Default.Util as Util
import           Yesod.Default.Util (WidgetFileSettings)

import qualified Core.AppSettings as AppSettings
import           Core.AppSettings (AppSettings)

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> TH.Q TH.Exp
widgetFile = (if AppSettings.reloadTemplates compileTimeAppSettings
                then Util.widgetFileReload
                else Util.widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(FileEmbed.embedFile DefaultConfig.configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ Yaml.decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case Aeson.fromJSON json of
        Error e -> error e
        Success settings -> settings
  where
    json = DefaultConfig.applyEnvValue False mempty configSettingsYmlValue

{-
-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: TH.Name -> [Route Static] -> TH.Q TH.Exp
combineStylesheets = combineStylesheets'
    (AppSettings.skipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: TH.Name -> [Route Static] -> TH.Q TH.Exp
combineScripts = combineScripts'
    (AppSettings.skipCombining compileTimeAppSettings)
    combineSettings

-}
