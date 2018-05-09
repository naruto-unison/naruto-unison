{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Interface for the PureScript game client.
module Handler.PlayPage
    ( getChangelogR
    , getPlayR
    , getMuteR
    , getUpdateR
    ) where

import qualified Data.Text           as T
import qualified Data.HashMap.Strict as M

import Data.Maybe
import Data.List
import Data.Text  (Text)
import Text.Hamlet
import Yesod.WebSockets

import Calculus
import Core.Import
import Game.Structure
import Game.Characters
import Handler.Play (gameSocket)
{-
shorten ∷ Text → Text
shorten = T.map shorten' ∘ T.filter (∉ filterOut)
  where filterOut    = " -:()®'/?" ∷ String
        shorten' 'ō' = 'o'
        shorten' 'Ō' = 'O'
        shorten' 'ū' = 'u'
        shorten' 'Ū' = 'U'
        shorten' 'ä' = 'a'
        shorten'  a  =  a

charAvatars ∷ Character → [Text]
charAvatars char = (root ☩ "icon.jpg")
                 : map (((root ☩) ∘ ( ☩ ".jpg")) ∘ shorten ∘ label ∘ head)
                       (take 4 $ characterSkills char)
  where root = "/img/ninja/" ☩ shorten (characterName char) ☩ "/"

avatars ∷ [Text]
avatars = map ("/img/icon/" ☩)
            [ "default.jpg"
            , "gaaraofthefunk.jpg"
            , "ninjainfocards.jpg"
            , "kabugrin.jpg"
            ]
       ⧺ concatMap charAvatars cs'

maybeBg ∷ Maybe User → Maybe Text
maybeBg = (≫= userBackground)

onTeam ∷ Text → Maybe User → Bool --
onTeam chara = maybe False $ maybe False (chara ∈) ∘ userTeam

-}
isMuted ∷ Maybe User → Bool
isMuted = maybe False userMuted

-- | Renders the gameplay client.
getPlayR ∷ Handler Html
getPlayR = do
    webSockets gameSocket
    ma ← maybeAuth
    let (_, muser) = case ma of
          Just (Entity who user) → (Just who, Just user)
          Nothing                → (Nothing, Nothing)
    let team          = maybe [] (mapMaybe (`M.lookup` cs)) $ userTeam =≪ muser
    let bg = fromMaybe "/img/bg/valley2.jpg" $ userBackground =≪ muser
    defaultLayout $ do
        setTitle "Naruto Unison"
        addStylesheetRemote "/css/embeds.css"
        $(widgetFile "include/progressbar.min")
        $(widgetFile "include/soundjs.min")
        $(widgetFile "include/normalize")
        $(widgetFile "play/play")
        $(widgetFile "play/ps")

legalChars ∷ String
legalChars = ['0'..'9'] ⧺ ['a'..'z'] ⧺ ['A'..'z']

-- | Updates a user's profile.
getUpdateR ∷ Text → Bool → Text → Text → Handler Value
getUpdateR updateName updateFlipped updateBackground updateAvatar
  | "/img/icon/" ≠ T.take 10 updateAvatar = invalidArgs ["Invalid avatar"]
  | T.any (∉ legalChars) updateName = invalidArgs ["Invalid name"]
  | otherwise = do
    (accId, _) ← requireAuthPair
    user ← runDB $ updateGet accId [ UserName       =. updateName
                                   , UserBackground =. updateBackground''
                                   , UserAvatar     =. updateAvatar
                                   , UserFlipped    =. updateFlipped
                                   ]     
    returnJson user
  where updateBackground'  = tTail updateBackground
        updateBackground'' | T.null updateBackground' = Nothing
                           | otherwise                = Just updateBackground'

getChangelogR ∷ Handler Html
getChangelogR = defaultLayout $ do
    setTitle "Naruto Unison: Changelog"
    $(widgetFile "changelog/changelog")

(!) ∷ Text → Text → Html
usr ! l = [shamlet| $newline never
<a .skill data-usr=#{usr}>#{l}|]

data LogType = Added | New | Rework
data CharacterType = O | R | S

changelog ∷ LogType → Text → CharacterType → Html
changelog logType name characterType = [shamlet|
#{change logType} #{display characterType}
<ul>
  $forall skills <- maybe [] (take 4 ∘ characterSkills) (M.lookup tagName cs)
    <li>
      $forall skill <- separate skills
        <span .skill>
          <a data-name=#{tagName}>#{label skill}
|]
  where separate      = nubBy $ eqs label
        tagName       = tag characterType
        tag O         = name
        tag R         = name ☩ " (R)"
        tag S         = name ☩ " (S)"
        change Added  = "Character added:"  ∷ Text
        change New    = "New character:"    ∷ Text
        change Rework = "Character rework:" ∷ Text
        display O     = [shamlet|#{name}|]
        display R     = [shamlet|#{name}
<a .minor>ℝ|]
        display S     = [shamlet|#{name}
<a .minor>𝕊|]

s ∷ Html
s = [shamlet|<a .minor>𝕊|]

--r ∷ Html
--r = [shamlet|<a .minor>ℝ|]

-- | Updates a user's muted status.
getMuteR ∷ Bool → Handler Value
getMuteR mute = do
  (who, _) ← requireAuthPair
  runDB $ update who [ UserMuted =. mute ]
  returnJson mute
