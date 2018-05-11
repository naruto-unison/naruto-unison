module Component.Play (Output(..), component) where

import Prelude

import Network.HTTP.Affjax     as AX
import Halogen                 as Halogen
import Halogen.HTML            as H
import Halogen.HTML.Events     as E
import Halogen.HTML.Properties as P

import Control.Monad.Aff   (Aff, delay)
import Data.Array
import Data.Either
import Data.Maybe 
import Data.Time.Duration  (Milliseconds(..))
import DOM                 (DOM)
import DOM.Event.Types     (MouseEvent)
import Halogen             ( Component, ComponentDSL, ComponentHTML
                           , get, modify, liftAff, liftEff, raise
                           )
import Halogen.HTML        (HTML)
import Network.HTTP.Affjax (AJAX)

import FFI.Import          (user)
import FFI.Progress        (progress)
import FFI.Sound           (AUDIO, Sound(..), sound)

import Operators 
import Functions 
import Info
import Component.Common
import Structure 

unknown ∷ Character
unknown = Character { characterName:   "unknown"
                    , characterBio:    ""
                    , characterSkills: []
                    }

getC ∷ Array Character → Int → Character
getC cs nId = fromMaybe unknown $ cs !! nId

type HTMLQ a = HTML a (ChildQuery Unit)

type Effects e = (ajax ∷ AJAX, audio ∷ AUDIO, dom ∷ DOM | e)

data Output = ActMsg SocketMsg | Finish Boolean

type State = { practice   ∷ Boolean
             , par        ∷ Int
             , vs         ∷ User
             , characters ∷ Array Character
             , game       ∷ Game
             , chakras    ∷ Chakras
             , randoms    ∷ Chakras
             , exchanged  ∷ Chakras
             , exchange   ∷ Boolean
             , viewing    ∷ Viewable
             , highlight  ∷ Array Int
             , toggled    ∷ Maybe Act
             , acts       ∷ Array Act
             , error      ∷ String
             }

pageSize ∷ Int
pageSize = 33

data ChakraPair = ChakraPair String Chakras Int Int

data Zipped = Zipped Character Ninja (Array (Array Int))

input ∷ ∀ a. PlayQuery → a → Maybe (ChildQuery Unit)
input a = E.input_ $ (QueryPlay a)

mClick ∷ ∀ e. String → String → Boolean → PlayQuery → Array 
         (P.IProp (id ∷ String, "class" ∷ String, onClick ∷ MouseEvent | e) 
         (ChildQuery Unit))
mClick i c false _ = [_i i, _c $ c ⧺ " noclick"]
mClick i c true  f = [_i i, _c $ c ⧺ " click", E.onClick $ input f]

hover ∷ ∀ e. Viewable 
      → P.IProp (onMouseEnter ∷ MouseEvent | e) (ChildQuery Unit)
hover = E.onMouseEnter ∘ input ∘ View

getChakra ∷ Game → Int → Chakras
getChakra (Game {gameChakra}) par = fromMaybe χØ $ gameChakra !! par

χrate ∷ Int
χrate = 5

affordable ∷ Chakras → Chakras → Boolean
affordable (Chakras χ) (Chakras del) = χrate < χSum χ'
  where χ' = Chakras { blood: if del.blood ≠ 0 then 0 else χ.blood 
                     , gen:   if del.gen   ≠ 0 then 0 else χ.gen 
                     , nin:   if del.nin   ≠ 0 then 0 else χ.nin 
                     , tai:   if del.tai   ≠ 0 then 0 else χ.tai
                     , rand: 0 
                     }

canExchange ∷ Chakras → Boolean
canExchange χ = any (affordable χ) $ χf ↤
                [_{ blood = 1 }, _{ gen = 1 }, _{ nin = 1 }, _{ tai = 1 }]

component ∷ ∀ m. Boolean → GameInfo 
          → Component HTML ChildQuery Unit Output (Aff (Effects m))
component practice0 (GameInfo {gameGame, gamePar, gameVsUser, gameCharacters}) =
  Halogen.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState ∷ State
  initialState = { practice:   practice0
                 , par:        gamePar
                 , vs:         gameVsUser
                 , characters: gameCharacters
                 , game:       gameGame
                 , chakras:    getChakra gameGame gamePar
                 , randoms:    χØ
                 , exchanged:  χØ
                 , exchange:   false
                 , viewing:    ViewUser gameVsUser
                 , highlight:  []
                 , toggled:    Nothing
                 , acts:       []
                 , error:      ""
                 }
  render ∷ State → ComponentHTML ChildQuery
  render { acts, chakras, characters, error, exchange, exchanged, highlight
         , par, practice, randoms, toggled, viewing, vs: vs@(User opponent)
         , game: (Game {gameNinjas, gameTargets, gamePlaying, gameVictor})
         } 
    = H.div [_i "game"] $
      [ H.span [_c "error"] [H.text error]
      , H.div [_i "top"] 
        [ H.div [_i "account0cont", _c "accountcont"]
          [ H.div [_c "account"] 
            [ H.div [_c "accountName"] [H.text account.name]
            , H.div [_c "accountRank"] [H.text $ userRank who]
            ]
          , H.div [_c "avatarcont"] [H.img [_c "charicon", _src account.avatar]]
          ]
        , H.div [_i "playbar", _c "parchment"] $ view cs viewing
        , H.div [_i "account1cont", _c "accountcont"]
          [ H.div [_c "avatarcont"] [H.img [_c "charicon", _src opponent.avatar]]
          , H.div [_c "account"] 
            [ H.div [_c "accountName"] [H.text opponent.name]
            , H.div [_c "accountRank"] [H.text $ userRank vs]
            ]
          ]
        ]
      , H.div [_i "player0", _c "player"] $ hNinja true  ↤ nsI
      , H.div [_i "player1", _c "player"] $ hNinja false ↤ nsU
      ] ⧺ case gameVictor of 
        Nothing → 
          [ H.div [_i "playchakra"] 
          $ (hChakra turn exchange χNet ↤ pairedChakras)
          ⧺ [ hRands (χSum χNet) χrand 
            , H.div 
              (mClick "exchange" "chakraButton" exchangeable $ ExchangeBegin) 
              [H.text "Exchange"]
            , H.div 
              (mClick "reset" "chakraButton" (exchanged ≠ χØ) $ ExchangeReset)
              [H.text "Reset"]
            ]
          , H.div [_i "playqueuecont"]
            [ H.div [_i "playqueue"] $ hAct cs ↤ acts
            , H.div (_i "ready" : readyMeta) []
            ]
          ]
        Just victor → 
          [ H.div [_i "endgame"]
            [ H.div [_i "endgamemsg"] 
              [H.text $ if victor ≡ par then "Victory" else "Defeat"]
            , _a "return" "playButton parchment click" "/" "Return"
            ] 
          ]
    where
      who@(User account)  = fromMaybe vs user
      turn                = gamePlaying ≡ par
      χNet@(Chakras net)  = exchanged +~ foldl (-~) chakras (actCost ↤ acts)
      χrand               = χSum randoms + net.rand - χrate * χSum exchanged
      freeRands           = χSum χNet + χrand
      free                = Chakras net { rand = freeRands }
      exchangeable        = freeRands ≥ 5 ∧ canExchange χNet ∧ turn
      acted               = actC_ ↤ acts
      readyMeta           | not turn  = [_c "noclick"]
                          | χrand ≠ 0 = [_c "noChakra"]
                          | otherwise = [ _c "click"
                                        , E.onClick ∘ input ∘ Ready practice
                                          $ enactUrl randoms exchanged acts  
                                        ]
      cs                  = zipWith mergeSkills characters gameNinjas
      hNinja              = hCharacter cs acted toggled highlight free turn
      pairedChakras       = [ pair "blood" _.blood $ χf _{ blood = 1 }
                            , pair "gen"   _.gen   $ χf _{ gen   = 1 }
                            , pair "nin"   _.nin   $ χf _{ nin   = 1 }
                            , pair "tai"   _.tai   $ χf _{ tai   = 1 }
                            ]
      pair name get χs    = ChakraPair name χs (get' χNet) (get' randoms)
        where get' (Chakras ks) = get ks
      pAlly (Zipped _ (Ninja {nId}) _) = allied par nId
      {yes: nsI, no: nsU} = partition pAlly
                          $ zip3 Zipped cs gameNinjas gameTargets
      
  eval ∷ ∀ m1. ChildQuery 
      ~> ComponentDSL State ChildQuery Output (Aff (Effects m1))
  eval (QueryPlay query next) = (_ ≫ next) case query of
      Enact Add act → do
        sound SFXApplySkill
        modify \state@{acts} → untoggle state { acts = acts `snoc` act }
      Enact Delete act → do
        sound SFXCancel
        modify \state@{acts} → untoggle state { acts = delete act acts }
      ExchangeBegin → do
        sound SFXTarget
        modify \state → state{ exchange = not state.exchange }
      ExchangeConclude χ → do
        sound SFXClick
        modify \state@{exchanged} → state { exchanged = exchanged +~ χ 
                                             , exchange  = false
                                             }
      ExchangeReset → do
        sound SFXCancel
        modify \state@{game, par} → state { chakras   = getChakra game par
                                             , randoms   = χØ
                                             , exchanged = χØ
                                             , exchange  = false 
                                             }
      ReceiveGame game@(Game g) → do
        case g.gameVictor of
          Just victor → do
            {par}      ← get
            let winner = victor ≡ par
            sound $ if winner then SFXWin else SFXLose
            raise $ Finish winner
          Nothing → do
            sound SFXStartTurn
            {par: oldPar, game: oldGame} ← get
            liftEff $ if g.gamePlaying ≡ oldPar 
                        then progress wait 1 0
                        else progress wait 0 1
            modify \state@{par} → state { game      = game
                                           , chakras   = getChakra game par 
                                           , randoms   = χØ
                                           , exchanged = χØ
                                           , acts      = []
                                           } 
            when (living oldPar oldGame > living oldPar game) $ sound SFXDeath
      Ready false url → do
        sound SFXNextTurn
        raise ∘ ActMsg $ SocketMsg url
      Ready true url → do
        modify $ untoggle ∘ _{ exchange = false, exchanged = χØ }
        {response} ← liftAff ∘ AX.get $ "/api/practiceact/" ⧺ url    
        case decodeGames $ response of
          Right [a,b] → do
            {par: oldPar, game: oldGame} ← get
            modify \state@{par} → state { game      = a
                                           , chakras   = getChakra a par 
                                           , randoms   = χØ
                                           , exchanged = χØ
                                           , acts      = []
                                           } 
            liftEff $ progress practiceWait 0 1
            liftAff $ delay practiceWait
            liftEff $ progress (Milliseconds 0.0) 1 1
            sound SFXStartTurn
            modify \state@{par} → state { game      = b
                                           , chakras   = getChakra b par
                                           , randoms   = χØ
                                           , exchanged = χØ
                                           , acts      = []
                                           , exchange  = false
                                           } 
            when (living oldPar oldGame > living oldPar b) $ sound SFXDeath
          Left error →
            modify _{ error = error }
          Right _ →
            modify _{ error = "Unknown error" }
      Spend χs → do
        sound SFXClick
        modify \state@{randoms, chakras} → 
            state { randoms = randoms +~ χs, chakras = chakras -~ χs }
      Toggle skill → do
        sound SFXTarget
        modify \state@{toggled} → if toggled ≡ Just skill 
                                     then state { toggled = Nothing }
                                     else state { toggled = Just skill }
      View viewing@(ViewSkill _ ts _) →
        modify _{ viewing = viewing, highlight = ts }
      View viewing → 
        modify _{ viewing = viewing }
      Unhighlight → 
        modify _{ highlight = [] }
    where untoggle     = _{ toggled = Nothing }
          wait         = Milliseconds 60000.0
          practiceWait = Milliseconds 1500.0
  eval (QuerySelect _ next) = pure next

_label ∷ ∀ a b. String → HTML a b
_label l = H.div [_c "label"] [H.text l]

enactUrl ∷ Chakras → Chakras → Array Act → String
enactUrl rand trade acts = intercalate "/" $ χList ⧺ actList
  where χList   = intercalate "," ∘ show ↤∘ χList' ↤ [rand, trade]
        actList = intercalate "," ∘ show ↤∘ actList' ↤ acts
        χList' (Chakras {blood, gen, nin, tai}) = [blood, gen, nin, tai]
        actList' (Act {actC, actS, actT}) = [actC, actS, actT]

hChakra ∷ ∀ a. Boolean → Boolean → Chakras → ChakraPair → HTMLQ a
hChakra turn exchange χs (ChakraPair chakra spend amount random) = H.div_ $
    [ H.div meta []
    , _span $ show amount
    , H.a 
      (mClick "" "more" (turn ∧ random > 0) ∘ Spend $ χNeg spend) 
      [H.text "+"]
    , H.a 
      (mClick "" "less" (turn ∧ amount > 0) $ Spend spend) 
      [H.text "—"]
    , H.div [_c   "chakra rand"]  []
    , _span $ show random
]
  where meta | exchange  = mClick "" ("chakra " ⧺ chakra) (affordable χs spend) 
                         $ ExchangeConclude spend
             | otherwise = [_c $ "chakra " ⧺ chakra]
             
hRands ∷ ∀ a. Int → Int → HTMLQ a
hRands amount random = H.div [_c "randbar"]
    [ H.span [_c "randT"] [H.text "T"]
    , _span $ show amount
    , H.a [_c "more noclick"] [H.text "+"]
    , H.a [_c "less noclick"] [H.text "—"]
    , H.span [_c "randT"] [H.text "T"]
    , H.span (if random < 0 then [_c "negrand"] else []) [H.text $ show random]
    ]

hAct ∷ ∀ a. Array Character → Act → HTMLQ a
hAct cs act@(Act {actSkill, actC}) = H.div 
    [_c "act click", E.onClick ∘ input $ Enact Delete act]
    [ H.img $ [cIcon (getC cs $ skillRoot actSkill actC) $ label_ actSkill]
    , H.div [_c "actcost"] ∘ hCost $ actCost act
    ]

actToggles ∷ Maybe Act → Array Int
actToggles Nothing = []
actToggles (Just (Act {actTs, actC, actSkill})) 
    = actTs ∩ skillTarget actC actSkill
    

hCharacter ∷ ∀ a. Array Character → Array Int → Maybe Act → Array Int → Chakras 
           → Boolean → Boolean → Zipped → HTMLQ a
hCharacter cs acted toggle highlighted χs turn onTeam 
  (Zipped character n@(Ninja 
    {nBarrier, nChannels, nCooldowns, nDefense, nFace, nHealth, nId, nSkills}) 
    ts)
    = H.div
      [_c ∘ (nHealth ≡ 0) ? ("dead " ⧺ _) $ "playerchar"]
      $ H.div [_c "channels"] hChannels 
      : mainBar 
      ⧺ [ H.div [_c "charhealthbar"]
        $ [ H.div [_c "charhealth", _style $ "width: " ⧺ hp] []
          , H.div [_c "charhealthtext", _style $ anchor ⧺ hp] 
            $ live [H.text $ show nHealth]
          ] 
        ⧺ hDefenses
      , H.div [_c "statuses"] hInfos
      ]
  where 
    hp        = show nHealth ⧺ "%"
    live els  | nHealth > 0 = els
              | otherwise   = []
    hChannels = live $ hChannel cs ↤ reverse nChannels
    hDefenses = live $ hDefense nId anchor nHealth 
                (reverse nBarrier) (reverse nDefense)
    hInfos    = live ∘ hInfo onTeam nId cs ↤∘ onTeam ? reverse $ infos n
    active    = onTeam ∧ turn ∧ nHealth > 0 ∧ nId ∉ acted
    classes   = intercalate " " 
              $ catMaybes [ Just                        "chariconcont" 
                          , nId ∈ highlighted        ?? " highlighted"
                          , nId ∈ actToggles toggle  ?? " toggled skill"
                          ] 
    mainMeta  = catMaybes [ Just $ _c classes 
                          , Just ∘ hover $ ViewCharacter character
                          , E.onClick ∘ input ∘ Enact Add ∘ retarget ↤ toggle
                          ]
    mainBar   = not onTeam ? reverse $
      [ H.div mainMeta
        [H.img [_c "charicon", faceIcon']]
      , H.div [_c "charmoves"] 
        ∘ zip4 (hSkill nId χs active cs) (0..3) ts nSkills 
        ∘ (nHealth > 0) ? (nCooldowns ⧺ _) $ [0, 0, 0, 0]
      ]
    anchor    | onTeam    = "left: "
              | otherwise = "right: "
    faceIcon' = case head nFace of
        Nothing → cIcon character "icon"
        Just (Face {faceSrc, faceIcon}) → 
          cIcon (getC cs faceSrc) $ "icon" ⧺ faceIcon
    retarget (Act act) = Act $ act { actT = nId }

sync ∷ ∀ a b. Int → HTML a b
sync 0   = H.text "Permanent"
sync dur = H.text ∘ show $ (dur + 1) / 2

hChannel ∷ ∀ a. Array Character → Channel → HTMLQ a
hChannel cs (Channel {channelDur, channelRoot, channelSkill}) = H.div
    [ _c    $ classF channelDur "status"
    , hover $ ViewSkill channelRoot [] channelSkill
    ]
    [ H.div [_c "statusDur"] $ if dur ≡ 0 then [] else [sync dur]
    , H.div [_c "statusBorder"] 
      [H.img [cIcon (getC cs channelRoot) $ label_ channelSkill]]
    ]
  where dur                = channelingDur channelDur
        classF (Action _)  = ("action "  ⧺ _)
        classF (Control _) = ("control " ⧺ _)
        classF _           = id
       
hBarrier ∷ ∀ a. Int → String → Int → Array Barrier 
         → Array (HTMLQ a)
hBarrier nId anchor track barriers = case uncons barriers of
    Nothing                            → []
    Just {head: b@(Barrier {barrierAmount}), tail} → H.div 
        [ _c "charbarrier"
        , _style $ anchor ⧺ show track ⧺ "%; width: " ⧺ show barrierAmount ⧺ "%"
        , hover $ ViewBarrier b
        ] 
        [] : hBarrier nId anchor (track + barrierAmount) tail
 
hDefense ∷ ∀ a. Int → String → Int → Array Barrier → Array Defense
         → Array (HTMLQ a)
hDefense nId anchor track barriers defenses = case uncons defenses of
    Nothing → hBarrier nId anchor track barriers
    Just {head: d@(Defense {defenseAmount}), tail} → H.div 
        [ _c "chardefense"
        , _style $ anchor ⧺ show track ⧺ "%; width: " ⧺ show defenseAmount ⧺ "%"
        , hover $ ViewDefense d
        ]
        [] : hDefense nId anchor (track + defenseAmount) barriers tail
 
cant ∷ ∀ a. Int → Skill → Array (HTMLQ a) → HTMLQ a
cant nId skill within = H.div
  [ _c "charmove noclick"
  , hover $ ViewSkill nId [] skill
  , E.onMouseLeave $ input Unhighlight
  ]
  within

hSkill ∷ ∀ a. Int → Chakras → Boolean → Array Character → Int → Array Int
       → Skill → Int → HTMLQ a
hSkill nId χs active cs sI ts skill@(Skill {cost, label, require}) cd
  | cd > 0 = cant nId skill
      [ H.img [cIcon (getC cs $ skillRoot skill nId) label]
      , H.div [_c "cd"] [H.text ∘ show ∘ max 1 $ cd / 2]
      ]
  | not active ∨ require ≡ Unusable ∨ lacks (χs -~ cost) ∨ null ts 
      = cant nId skill [H.img [cIcon (getC cs $ skillRoot skill nId) label]]
  | otherwise = H.div 
      [ _c "charmove click"
      , hover $ ViewSkill nId ts skill
      , E.onMouseLeave $ input Unhighlight
      , E.onClick $ input action
      ]
      [H.img [cIcon (getC cs $ skillRoot skill nId) label]]
  where act = Act { actC: nId, actS: sI, actT: nId, actSkill: skill, actTs: ts }
        action | skillTarget nId skill ≡ [nId] = Enact Add act
               | otherwise                     = Toggle act

fromAlly ∷ Int → Info → Boolean
fromAlly c {root, src} = allied c root ∧ allied c src

hInfo ∷ ∀ a. Boolean → Int → Array Character → Info → HTMLQ a
hInfo team nId cs info@{classes, dur, effects,ghost,  name, root, trap} = H.div
    [_c $ intercalate " " classes', hover $ ViewInfo removes info]
    [ H.div [_c "statusBorder"]
      [H.img [cIcon (getC cs root) name]]
    , H.div [_c "statusDur"] $ if dur ≡ 0 then [] else [sync dur]
    ]
  where classes' = catMaybes [ Just                       "status" 
                             , trap                    ?? "trap"
                             , ghost ∨ dur ≡ 1         ?? "tag"
                             , any removes effects     ?? "remove"
                             , ("Shifted" ∈ classes)   ?? "reflected"
                             , ("Invisible" ∈ classes) ?? "fade"
                             ]
        removes | fromAlly nId info = const false
                | otherwise         = removable team

viewBar ∷ ∀ a b. Character → String → Int → Int → Array (HTML a b)
viewBar src l amount dur
    = [ H.img [_c "char",           cIcon src l]
      , H.div [_i "barname"]        [H.text l]
      , _label "Amount: ",   H.div_ [H.text $ show amount]
      , _label "Duration: ", H.div_ [sync dur]
      , _label "Source: ",   H.div_ [H.text $ characterName_ src]
      ]

viewClasses ∷ ∀ a b. Boolean → Array String → Array (HTML a b)
viewClasses _ [] = []
viewClasses hideMore classes = 
  [ _label "Classes :"
  , H.div_ [H.text ∘ intercalate ", " $ filterClasses hideMore classes]
  ]

viewEffect ∷ ∀ a b. (Effect → Boolean) → Effect → HTML a b
viewEffect removes effect@(Effect {effectTrap})
  | effectTrap     = H.div [_c "trap"]   $ descEffect effect
  | removes effect = H.div [_c "remove"] $ descEffect effect
  | otherwise      = H.div_              $ descEffect effect

descEffect ∷ ∀ a b. Effect → Array (HTML a b)
descEffect (Effect {effectDesc}) = [H.text $ "- " ⧺ effectDesc]

view ∷ ∀ a. Array Character → Viewable → Array (HTMLQ a)
view cs (ViewBarrier
     (Barrier {barrierSrc, barrierL, barrierAmount, barrierDur})) 
    = viewBar (getC cs barrierSrc) barrierL barrierAmount barrierDur

view cs (ViewDefense (Defense {defenseSrc, defenseL, defenseAmount, defenseDur})) 
    = viewBar (getC cs defenseSrc) defenseL defenseAmount defenseDur

view cs (ViewCharacter c@(Character {characterName, characterBio})) =
    [ H.img [_c "char",    cIcon c "icon"]
    , H.div [_i "barname"] [H.text characterName]
    , H.div [_i "bardesc"] [H.text characterBio]
    ]

view cs (ViewInfo removes {classes, desc, effects, name, root, src})
    = [ H.img [_c "char",         cIcon (getC cs root) "icon"]
      , H.div [_i "barname"]      [name']
      , _label "Source: ", H.div_ [H.text $ characterName_ (getC cs src)]
      ] ⧺ viewClasses true        classes ⧺ 
      [ H.div [_i "bardesc"]    $ parseDesc desc
      , H.div [_i "bareffects"] $ viewEffect removes ↤ effects
      ]
  where 
        name' | "Shifted" ∈ classes = H.span [_c "reflected"] 
                                      [H.text $ name ⧺ " (Reflected)"]
              | otherwise           = H.span_ [H.text $ name]
view cs (ViewSkill nId _ skill@(Skill {cd, charges, classes, cost, desc, label}))
    = [ H.div [_c "skillPreview"] 
        $ H.img [_c "char", cIcon (getC cs $ skillRoot skill nId) label]
        : varyButtons
      , H.div [_i "barname"]   [H.text label]
      , _label "Cost: ",       H.div_ cost'
      , _label "Duration: ",   H.div_ [H.text $ skillDur skill]
      ] ⧺ viewClasses false    classes ⧺
      [ _label "Cooldown: ",   H.div_ [H.text cd']
      , H.div [_i "bardesc"] $ parseDesc desc ⧺ catMaybes
        [ charges > 1 ?? _minor (show charges ⧺ " charges.")
        , charges ≡ 1 ?? _minor (show charges ⧺ " charge.")
        ]
      ]
  where cd'   | cd ≡ 0 = "None"
              | otherwise    = show cd
        cost' = cost'' cost
        cost'' χs | χs ≡ χØ   = [H.text "Free"]
                  | otherwise = hCost χs    
        varyButtons = fromMaybe [] do
          Character {characterSkills} ← cs !! skillRoot skill nId
          skills ← find (any (lMatch skill)) characterSkills
          i ← findIndex (lMatch skill) skills
          pure $ catMaybes
            [ vPrev skills i ↦ \v → H.a 
              [ _c "prevSkill click"
              , E.onClick ∘ input ∘ View $ ViewSkill nId [] v
              ] []
            , vNext skills i ↦ \v → H.a 
              [ _c "nextSkill click"
              , E.onClick ∘ input ∘ View $ ViewSkill nId [] v
              ] []
            ] 

view cs (ViewUser user@(User {avatar, clan, name})) =
    [ H.img [_c "char",        _src avatar]
    , H.div [_i "barname"]     [H.text name]
    , H.div_                   [H.text $ userRank user]
    , _label "Clan: ",  H.div_ [H.text $ fromMaybe "Clanless" clan]
    , _label "Level: ", H.div_ [H.text ∘ show $ userLevel user]
    ]

vNext ∷ Array Skill → Int → Maybe Skill
vNext skills i = do
  skill ← skills !! i
  find (not ∘ lMatch skill) $ drop i skills

vPrev ∷ Array Skill → Int → Maybe Skill
vPrev skills i = do
  skill ← skills !! i
  find (not ∘ lMatch skill) ∘ reverse $ take i skills
        