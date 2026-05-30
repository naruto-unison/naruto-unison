module Handler.Client.Data where

import ClassyPrelude

import           Data.Aeson (ToJSON(..), fromEncoding)
import qualified Data.ByteString.Builder as BS
import           Data.List (nub)
import           Class.Display (shorten)
import           UnliftIO.Directory (createDirectoryIfMissing)
import           Yesod (MonadWidget, addScriptRemote)

import qualified Game.Characters as Characters
import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character
import qualified Game.Model.Class as Class
import qualified Game.Model.Skill as Skill

dataJS :: ByteString
dataJS = toStrict . builderToLazy
    $ "characters=" ++ encodeBytes Characters.list
    ++ ";\nvisibles=" ++ encodeBytes visibles
    ++ ";\navatars=" ++ encodeBytes avatars
    ++ ";"
  where
    visibles = filter Class.visible [minBound..maxBound]
    encodeBytes:: ∀ a. ToJSON a => a -> BS.Builder
    encodeBytes = fromEncoding . toEncoding

addDataJS :: ∀ m. MonadWidget m => m ()
addDataJS = addScriptRemote "/js/data.js"

writeDataJS :: FilePath -> IO ()
writeDataJS staticDir = do
    createDirectoryIfMissing True $ staticDir ++ "/js"
    writeFile (staticDir ++ "/js/data.js") dataJS

-- | Icons from all of a character's skills.
charAvatars :: Character -> [Text]
charAvatars Character{ident, skills} = toFile . shorten
    <$> "icon" : (nub $ Skill.name <$> concatMap toList skills)
  where
    toFile path = "/img/ninja/" ++ ident ++ "/" ++ path ++ ".jpg"

-- | Icons that users can set as their avatars.
avatars :: [Text]
avatars = icons ++ concatMap charAvatars Characters.list
  where
    icons = toFile <$> [ "default.jpg"
                       , "gaaraofthefunk.jpg"
                       , "ninjainfocards.jpg"
                       , "kabugrin.jpg"
                       ]
    toFile path = "/img/icon/" ++ path
