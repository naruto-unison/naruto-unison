module Handler.Client.Data where

import ClassyPrelude

import           Data.Aeson (ToJSON(..), fromEncoding)
import qualified Data.ByteString.Builder as BS
import           Data.List (nub)
import           Class.Display (shorten)
import           UnliftIO.Directory (createDirectoryIfMissing)
import           Yesod (addScript)
import           Yesod.Static (base64md5)

import qualified Application.App as App
import           Class.Display (buildStrict)
import qualified Game.Characters as Characters
import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character
import qualified Game.Model.Class as Class
import qualified Game.Model.Skill as Skill
import           Mission (freeChars)

dataJS :: ByteString
dataJS = buildStrict
    $ "characters=" ++ encodeBytes Characters.list
    ++ ";\nvisibles=" ++ encodeBytes visibles
    ++ ";\navatars=" ++ encodeBytes avatars
    ++ ";\nalwaysUnlocked=" ++ encodeBytes freeChars
    ++ ";\n"
  where
    visibles = filter Class.visible [minBound..maxBound]
    encodeBytes:: ∀ a. ToJSON a => a -> BS.Builder
    encodeBytes = fromEncoding . toEncoding

addDataJS :: ∀ m. App.MonadWidget m => m ()
addDataJS = addScript $ App.StaticR
    $ App.StaticRoute ["js", "data.js"] [("etag", etag)]
  where
    etag = pack $ base64md5 $ fromStrict dataJS

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
