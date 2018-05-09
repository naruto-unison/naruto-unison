module Component.CharacterSelect 
  ( Selection(..)
  , State(..)
  , component
  ) where

import Prelude

import Halogen                 as HH
import Halogen.HTML            as H
import Halogen.HTML.Events     as E
import Halogen.HTML.Properties as P

import Control.Monad.Aff (Aff)
import Data.Array 
import Data.Maybe 
import Data.String       (joinWith)
import DOM.Event.Types   (MouseEvent)
import Halogen           (Component, ComponentDSL, ComponentHTML)
import Halogen.HTML      (HTML, img)

import FFI.Import        (avatars, cs', user, userTeam)
import FFI.Sound         (AUDIO, Sound(..), sound)

import Operators
import Structure
import Functions
import Component.Common 

type Effects e = (audio ∷ AUDIO | e)

data Selection = Queued QueueType (Array Character)

type State = { queueing   ∷ Boolean
             , showLogin  ∷ Boolean
             , index      ∷ Int
             , cols       ∷ Int
             , previewing ∷ Previewing
             , team       ∷ Array Character
             , variants   ∷ Array Int
             , avatar     ∷ Maybe String
             }

pageSize ∷ Int
pageSize = 36

click ∷ ∀ a. SelectQuery → P.IProp (onClick ∷ MouseEvent | a) (ChildQuery Unit)
click a = E.onClick ∘ E.input_ $ (QuerySelect a)

preview ∷ ∀ a. Character 
        → P.IProp (onMouseEnter ∷ MouseEvent | a) (ChildQuery Unit)
preview = E.onMouseEnter ∘ E.input_ ∘ QuerySelect ∘ Preview ∘ PreviewCharacter

component ∷ ∀ m. Component HTML ChildQuery Unit Selection (Aff (Effects m))
component =
  HH.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { queueing:   false
                 , showLogin:  true
                 , index:      0 
                 , cols:       11
                 , previewing: NoPreview
                 , team:       userTeam
                 , variants:   [0,0,0,0]
                 , avatar:     Nothing
                 }
 
  render :: State -> ComponentHTML ChildQuery
  render {previewing, index, showLogin, team, variants} =
      H.div [_i "charSelect"] 
        $ userBox showLogin team 
        ⧺ previewBox variants previewing
        [ H.div [_c "characterButtons parchment"] 
          [ H.div 
            [ _i "prevPage" 
            , _c ∘ atMin ? ("wraparound " ⧺ _) $ "click"
            , click $ Scroll back
            ] []
          , H.div 
            [ _i "nextPage" 
            , _c ∘ atMax ? ("wraparound " ⧺ _) $ "click"
            , click $ Scroll forward
            ] []
          , H.div [_i "charScroll"] $ displays ↦ \c → 
              if c ∈ team then 
                H.div [_c "char disabled"] 
                [H.img [cIcon c "icon", preview c]]
              else if length team < 3 then
                H.div [_c "char click"]
                [ H.img 
                  [cIcon c "icon", preview c, click $ Team Add c]
                ]
              else
                H.div [_c "char click"]
                [H.img [cIcon c "icon", preview c]] 
          ]
        ]
    where pageAction = if showLogin then "login" else "register"
          displays   = drop (index) cs'
          size       = length cs'
          playButton = (length team ≡ 3) ? ("click " ⧺ _) 
                     $ "playButton parchment"
          atMin      = index ≡ 0
          atMax      = index + pageSize ≥ size
          back       | atMin            = size - size % pageSize
                     | index < pageSize = 0
                     | otherwise        = index - pageSize
          forward    | atMax            = 0
                     | otherwise        = index + pageSize
  eval ∷ ChildQuery ~> ComponentDSL State ChildQuery Selection (Aff (Effects m))
  eval (QuerySelect query next) = (_ ≫ next) case query of
    SwitchLogin →
      HH.modify \state@{showLogin} → state { showLogin = not showLogin }
    Scroll a → do
      sound SFXScroll
      HH.modify _{ index = a }
    Preview previewing → 
      HH.modify _{ previewing = previewing, variants = [0,0,0,0] }
    Vary slot i → do
      sound SFXClick
      HH.modify \state@{variants} → state 
        { variants = updateAt' slot i variants }
    Team Add character → do
      sound SFXClick
      HH.modify \state@{team} → state { team = character : team }
    Team Delete character → do
      sound SFXCancel
      HH.modify \state@{team} → state { team = delete character team }
    Enqueue queueType → do
      sound SFXApplySkill
      {team: team} ← HH.get
      HH.raise $ Queued queueType team
    ChooseAvatar avatar → do
      sound SFXClick
  eval (QueryPlay _ next) = pure next

previewBox ∷ ∀ a. Array Int → Previewing
           → Array (HTML a (ChildQuery Unit)) → Array (HTML a (ChildQuery Unit))
previewBox _ NoPreview = id
previewBox _ PreviewUser = case user of 
  Nothing → id
  Just (User {name, background}) → cons $
    H.div [_i "charPreview", _c "parchment"]
    [ H.h1_ $ _txt "Account Settings"
    , H.p_ 
      [ _span "Name"
      , H.input [P.type_ P.InputText, P.name "name", P.value name]
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
    , H.div [_i "avatars"] $ avatars ↦ \avatar → H.img [_src avatar]
    ]
previewBox variants (PreviewCharacter
  character@Character {characterName, characterBio, characterSkills}) = cons ∘
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
  Just user@(User {avatar, name, clan, wins, losses, streak}) →
    [ H.div [_c "playButtons"] $
      [ H.div 
        [_i "queue", _c playButton, click $ Enqueue Quick]
        [ H.text "Start Quick Match" ]
      , H.div 
        [_i "practicequeue", _c playButton, click $ Enqueue Practice]
        [ H.text "Start Practice Match"]
      , H.div 
        [_i "private", _c playButton, click $ Enqueue Private]
        [ H.text "Start Private Match" ]
      , _a "mainsite" "playButton parchment click" "/changelog" "Changelog"
      ]
    , H.div [_c "teamContainer"]
      [ H.div [_i "userbox", _c "parchment loggedin"]
        [ img [_c "userimg char", _src avatar]
        , _b       name
        , H.br_
        , H.text $ userRank user
        , H.br_
        , _b       "Clan: "
        , H.text $ fromMaybe "Clanless" clan
        , H.br_
        , _b       "Level: "
        , H.text $ show (userLevel user) ⧺ " (" ⧺ show (userXP user) ⧺ " XP)"
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
            [H.img [cIcon c "icon", preview c, click $ Team Delete c]]
      , H.div [_i "underTeam", _c "parchment"] []
      ]
    ] 
  Nothing → 
    [ H.div [_c "playButtons"] 
      [_a "mainsite" "playButton parchment click" "/changelog" "Changelog"]
    , H.div [_c "teamContainer"]
      [ H.div [_i "userbox", _c "parchment"] 
        [ H.form 
          [_c "userForm", P.method P.POST, P.action "/auth/page/email/login"]
          [ H.div 
            [_i $ if showLogin then "loginForm" else "registerForm"] 
            $ catMaybes
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
        ]
      , H.div [_i "teamButtons"] $ team ↦ \c →
          H.div [_c "char click"]
            [ H.img 
              [cIcon c "icon", preview c, click $ Team Delete c]
            ]
      , H.div [_i "underTeam", _c "parchment"] []
      ]
    ]  
  where playButton = (length team ≡ 3) ? ("click " ⧺ _) $ "playButton parchment" 
