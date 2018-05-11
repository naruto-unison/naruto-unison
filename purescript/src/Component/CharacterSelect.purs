module Component.CharacterSelect 
    ( Output(..)
    , State(..)
    , component
    ) where

import Prelude

import Network.HTTP.Affjax     as AX
import Halogen                 as Halogen
import Halogen.HTML            as H
import Halogen.HTML.Events     as E
import Halogen.HTML.Properties as P

import Control.Monad.Aff           (Aff)
import Data.Array 
import Data.Maybe 
import Data.String                 (joinWith)
import Data.StrMap                 (lookup)
import DOM                         (DOM)
import DOM.Event.Types             (MouseEvent)
import Global                      (encodeURIComponent)
import Halogen.HTML                (HTML, img)
import Network.HTTP.Affjax         (AJAX)
import Network.HTTP.StatusCode     (StatusCode(..))
import Halogen                     ( Component, ComponentDSL, ComponentHTML
                                   , get, liftAff, liftEff, modify, raise
                                   )

import FFI.Form   (getForm)
import FFI.Import (avatars, cs', getPageSize, reload, user, userTeam)
import FFI.Sound  (AUDIO, Sound(..), sound)

import Operators
import Structure
import Functions
import Component.Common 

csSize ∷ Int
csSize = length cs'

settingsId ∷ String
settingsId = "accountSettings"

type Effects e = (ajax ∷ AJAX, audio ∷ AUDIO, dom ∷ DOM | e)

data Output = Queued QueueType (Array Character) | UpdateMsg SocketMsg | Blank

type State = { queueing   ∷ Boolean
             , showLogin  ∷ Boolean
             , index      ∷ Int
             , cols       ∷ Int
             , previewing ∷ Previewing
             , team       ∷ Array Character
             , variants   ∷ Array Int
             , avatar     ∷ Maybe String
             , pageSize   ∷ Int
             , updateFail ∷ Boolean
             }

click ∷ ∀ a. SelectQuery → P.IProp (onClick ∷ MouseEvent | a) (ChildQuery Unit)
click a = E.onClick ∘ E.input_ $ (QuerySelect a)

preview ∷ ∀ a. Previewing 
        → P.IProp (onMouseEnter ∷ MouseEvent | a) (ChildQuery Unit)
preview = E.onMouseEnter ∘ E.input_ ∘ QuerySelect ∘ Preview

component ∷ ∀ m. Component HTML ChildQuery Unit Output (Aff (Effects m))
component = Halogen.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState ∷ State
  initialState = { queueing:   false
                 , showLogin:  true
                 , index:      0 
                 , cols:       11
                 , previewing: maybe NoPreview PreviewUser user
                 , team:       userTeam
                 , variants:   [0,0,0,0]
                 , avatar:     avatar_ ↤ user
                 , pageSize:   36
                 , updateFail: false
                 }
 
  render ∷ State → ComponentHTML ChildQuery
  render st@{avatar, previewing, index, pageSize, showLogin, team, variants} =
      H.div [_i "charSelect"] 
        $ userBox showLogin team 
        ⧺ previewBox st
        [ H.div [_c "characterButtons parchment"] 
          [ H.div 
            [ _i "prevPage" 
            , _c ∘ atMin ? ("wraparound " ⧺ _) $ "click"
            , click $ Scroll (-1)
            ] []
          , H.div 
            [ _i "nextPage" 
            , _c ∘ atMax ? ("wraparound " ⧺ _) $ "click"
            , click $ Scroll 1
            ] []
          , H.div [_i "charScroll"] $ displays ↦ \c → 
              if c ∈ team then 
                H.div [_c "char disabled"] 
                [H.img [cIcon c "icon", preview $ PreviewChar c]]
              else if length team < 3 then
                H.div [_c "char click"]
                [ H.img 
                  [cIcon c "icon", preview $ PreviewChar c, click $ Team Add c]
                ]
              else
                H.div [_c "char click"]
                [H.img [cIcon c "icon", preview $ PreviewChar c]] 
          ]
        ]
    where displays   = drop (index) cs'
          playButton = (length team ≡ 3) ? ("click " ⧺ _) 
                     $ "playButton parchment"
          atMin      = index ≡ 0
          atMax      = index + pageSize ≥ csSize
  eval ∷ ChildQuery ~> ComponentDSL State ChildQuery Output (Aff (Effects m))
  eval (QuerySelect query next) = (_ ≫ next) $ case query of
      SwitchLogin →
        modify \state@{showLogin} → state { showLogin = not showLogin }
      Scroll a → do
        sound SFXScroll
        pageSize ← liftEff getPageSize
        modify \state@{index} → 
          state { index = index' a pageSize index, pageSize = pageSize }
      Preview previewing → 
        modify _{ previewing = previewing, variants = [0,0,0,0] }
      Vary slot i → do
        sound SFXClick
        modify \state@{variants} → state 
          { variants = updateAt' slot i variants }
      Team Add character → do
        sound SFXClick
        modify \state@{team} → state { team = character : team }
      Team Delete character → do
        sound SFXCancel
        modify \state@{team} → state { team = delete character team }
      Enqueue queueType → do
        sound SFXApplySkill
        {team: team} ← get
        raise $ Queued queueType team
      ChooseAvatar avatar → do
        sound SFXClick
        modify _{ avatar = Just avatar }
      TryUpdate → do
        sound SFXTarget
        {avatar} ← get
        mform    ← liftEff $ getForm settingsId
        let murl = do
              avatar'    ← avatar
              form       ← mform
              background ← lookup "background" form
              name       ← lookup "name"       form
              pure $ "api/update/" ⧺ name ⧺ "/b" ⧺ background ⧺ "/" 
                   ⧺ encodeURIComponent avatar'
        case murl of
          Nothing → 
            modify _{ updateFail = true }
          Just url → do
            {status} ∷ AX.AffjaxResponse String ← liftAff $ AX.get url
            if status ≡ StatusCode 200 then liftEff reload 
                                       else modify _{ updateFail = true }
    where index' a pageSize index
            | a ≡ 1 ∧ index + pageSize ≥ csSize = 0
            | a ≡ 1            = index + pageSize
            | index ≡ 0        = csSize - csSize % pageSize
            | index < pageSize = 0
            | otherwise        = index - pageSize
  eval (QueryPlay _ next) = pure next

previewBox ∷ ∀ a. State
           → Array (HTML a (ChildQuery Unit)) → Array (HTML a (ChildQuery Unit))
previewBox {previewing: NoPreview} = id
previewBox { avatar, updateFail
           , previewing: PreviewUser (User {name, background})
           } = cons $
  H.form [_i settingsId, _c "parchment"]
  [ H.h1_ $ _txt "Account Settings"
  , H.p_ $ catMaybes
    [ Just $ _span "Name"
    , Just $ H.input [P.type_ P.InputText, P.name "name", P.value name]
    , updateFail ?? H.span [_i "userfail"] [H.text "Username already taken!"]
    ]
  , H.p_
    [ _span "Background" 
    , H.input 
      [ P.type_ P.InputText
      , P.name "background"
      , P.value $ fromMaybe "" background
      ]
    ]
  , H.p_ [_span "Avatars"]
  , H.div [_i "avatars"] $ avatars ↦ \avatar' → H.img
      if Just avatar' ≡ avatar
      then [_src avatar', _c "noclick"]
      else [_src avatar', _c "click", click $ ChooseAvatar avatar']
  , H.div [_i "updateButton", _c "click", click TryUpdate] $ _txt "Update"
  , H.a [P.href "auth/logout"] 
    [H.div [_i "logoutButton", _c "click"] $ _txt "Log out"]
  ]
previewBox { variants, previewing: PreviewChar character@Character 
                         {characterName, characterBio, characterSkills}
           } = cons ∘
  H.div [_i "charPreview", _c "parchment"]
  $ [img [_i "charIcon", _c "char", cIcon character "icon"]
    , H.div [_i "charName"] [ H.text characterName ]
    , H.div [_i "charDesc"] [ H.text characterBio ]
    ] 
  ⧺ concat 
    (zip3 (previewSkill character) (0..3) characterSkills variants)

previewSkill ∷ ∀ a. Character → Int → Array Skill → Int 
             → Array (HTML a (ChildQuery Unit))
previewSkill character slot skills i = case skills !! i of
  Nothing → []
  Just (Skill {label, cost, charges, classes, desc, cd}) → 
      [ H.div [_c "skillPreview"] $ catMaybes
        [ Just $ img [_c "char", cIcon character label]
        , vPrev ↦ \v → H.a [_c "prevSkill click", click $ Vary slot v][]
        , vNext ↦ \v → H.a [_c "nextSkill click", click ∘ Vary slot $ v + i][]
        ]
      , H.div [_c "skillname"]
        $ H.text label
        : hCost cost `snoc`                 
          H.div [_c "skillClasses"]
          [ H.text ∘ joinWith ", " $ filterClasses false classes ]
      , H.div [_c "skilldesc"] 
        $ parseDesc desc ⧺ catMaybes
          [ charges > 1 ?? _minor (show charges ⧺ " charges.")
          , charges ≡ 1 ?? _minor (show charges ⧺ " charge.")
          , cd > 0      ?? _minor ("CD: " ⧺ show cd)
          ]
      ]
  where vPrev = do
            skill ← skills !! i
            findLastIndex (not ∘ lMatch skill) $ take i skills
        vNext = do
            skill ← skills !! i
            findIndex (not ∘ lMatch skill) $ drop i skills
        
userBox ∷ ∀ a. Boolean → Array Character → Array (HTML a (ChildQuery Unit))
userBox showLogin team = case user of
  Just u@(User {avatar, name, clan, wins, losses, streak}) →
    [ H.div [_c "playButtons"] $
      [ _a "mainsite" "playButton parchment click" "/home" "Main Site"
      , H.div 
        [_i "queue", _c playButton, click $ Enqueue Quick]
        [H.text "Start Quick Match"]
      , H.div 
        [_i "practicequeue", _c playButton, click $ Enqueue Practice]
        [H.text "Start Practice Match"]
      , H.div 
        [_i "private", _c playButton, click $ Enqueue Private]
        [H.text "Start Private Match"]
      ]
    , H.div [_c "teamContainer"]
      [ H.div [_i "userbox", _c "parchment loggedin", preview $ PreviewUser u]
        [ img [_c "userimg char", _src avatar]
        , _b       name
        , H.br_
        , H.text $ userRank u
        , H.br_
        , _b       "Clan: "
        , H.text $ fromMaybe "Clanless" clan
        , H.br_
        , _b       "Level: "
        , H.text $ show (userLevel u) ⧺ " (" ⧺ show (userXP u) ⧺ " XP)"
        , H.br_
        , _b       "Ladder Rank: "
        , H.text   "None"
        , H.br_
        , _b       "Record: "
        , H.text $ show wins ⧺ " - " ⧺ show (wins + losses) 
                 ⧺ " (+" ⧺ show streak ⧺ ")"
        ] 
      , H.div [_i "teamButtons"] $ team ↦ \c →
          H.div [_c "char click"]
            [ H.img 
              [cIcon c "icon", preview $ PreviewChar c, click $ Team Delete c]
            ]
      , H.div [_i "underTeam", _c "parchment"] []
      ]
    ] 
  Nothing → 
    [ H.div [_c "playButtons"] 
      [_a "mainsite" "playButton parchment click" "/home" "Main Site"]
    , H.div [_c "teamContainer"]
      [ H.div [_i "userbox", _c "parchment"] 
        [ H.form 
          [ _i $ if showLogin then "loginForm" else "registerForm"
          , _c "userForm"
          , P.method P.POST, P.action "/auth/page/email/login"
          ] $ catMaybes
            [ Just $ H.div_ 
              [ H.input 
                [ _c            "email"
                , P.name        "email" 
                , P.type_       P.InputEmail
                , P.required    true
                , P.autofocus   true
                , P.placeholder "Email" 
                ]
              ]
            , showLogin ?? H.div_ 
              [ H.input 
                [ _c            "password"
                , P.name        "password" 
                , P.type_       P.InputPassword
                , P.required    true
                , P.placeholder "Password"
                ]
              ]
            , Just $ H.div [_c "controls"]
              if showLogin then
                [ H.button 
                  [ _c          "playButton click"
                  , P.type_     P.ButtonSubmit
                  ] 
                  [H.text "Log in"]
                , H.a 
                  [ _c          "click"
                  , click       SwitchLogin
                  ]
                  [H.text "Register"]
                ]
              else
              [ H.a 
                [ _c            "click"
                , click         SwitchLogin
                ]
                [H.text "Log in"]
              , H.button 
                [ _c            "playButton click"
                , P.type_       P.ButtonSubmit
                ] 
                [H.text "Register"]
              ]
            ]
          ]
      , H.div [_i "teamButtons"] $ team ↦ \c →
          H.div [_c "char click"]
            [ H.img 
              [cIcon c "icon", preview $ PreviewChar c, click $ Team Delete c]
            ]
      , H.div [_i "underTeam", _c "parchment"] []
      ]
    ]  
  where playButton = (length team ≡ 3) ? ("click " ⧺ _) $ "playButton parchment" 
