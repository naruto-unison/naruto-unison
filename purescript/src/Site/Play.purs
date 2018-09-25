module Site.Play (Message(..), comp) where

import StandardLibrary
import Effect.Aff              as Aff
import Data.Array              as Array
import Halogen.HTML.Events     as E
import Halogen.HTML            as H
import Halogen.HTML.Properties as P

import Data.Time.Duration (Milliseconds(..))
import Halogen (Component, ComponentDSL, ComponentHTML, component, get, modify_, raise)
import Halogen.HTML (HTML)
import Web.UIEvent.MouseEvent (MouseEvent)

import Database.Functions
import Database.Info
import Database.Structure
import FFI.Import
import FFI.Progress
import FFI.Sound
import Site.Common


unknown :: Character
unknown = Character { characterName:   "unknown"
                    , characterBio:    ""
                    , characterSkills: []
                    }

getC :: Array Character -> Int -> Character
getC cs nId = fromMaybe unknown $ cs !! nId

data Message = ActMsg SocketMsg | Finish Boolean

type State = { practice   :: Boolean
             , par        :: Int
             , vs         :: User
             , characters :: Array Character
             , game       :: Game
             , chakras    :: Chakras
             , randoms    :: Chakras
             , exchanged  :: Chakras
             , exchange   :: Boolean
             , viewing    :: Viewable
             , highlight  :: Array Int
             , toggled    :: Maybe Act
             , acts       :: Array Act
             , error      :: String
             }

pageSize :: Int
pageSize = 33

data ChakraPair = ChakraPair String Chakras Int Int

data Zipped = Zipped Character Ninja (Array (Array Int))

input :: ∀ a. PlayQuery -> a -> Maybe (ChildQuery Unit)
input = E.input_ <<< QueryPlay

mClick :: ∀ e. String -> String -> Boolean -> PlayQuery -> Array 
         (P.IProp (id :: String, "class" :: String, onClick :: MouseEvent | e) 
         (ChildQuery Unit))
mClick i c false _ = [_i i, _c $ c <> " noclick"]
mClick i c true  f = [_i i, _c $ c <> " click", E.onClick $ input f]

hover :: ∀ e. Viewable 
      -> P.IProp (onMouseEnter :: MouseEvent | e) (ChildQuery Unit)
hover = E.onMouseEnter <<< input <<< View

getChakra :: Game -> Int -> Chakras
getChakra (Game {gameChakra}) par = fromMaybe zero $ gameChakra !! par

χrate :: Int
χrate = 5

affordable :: Chakras -> Chakras -> Boolean
affordable (Chakras χ) (Chakras del) = χrate < χSum χ'
  where χ' = Chakras { blood: if del.blood /= 0 then 0 else χ.blood 
                     , gen:   if del.gen   /= 0 then 0 else χ.gen 
                     , nin:   if del.nin   /= 0 then 0 else χ.nin 
                     , tai:   if del.tai   /= 0 then 0 else χ.tai
                     , rand: 0 
                     }

canExchange :: Chakras -> Boolean
canExchange χ = any (affordable χ) $ χf <$>
                [_{ blood = 1 }, _{ gen = 1 }, _{ nin = 1 }, _{ tai = 1 }]

comp :: ∀ m. MonadAff m => Boolean -> GameInfo 
     -> Component HTML ChildQuery Unit Message m
comp practice0 (GameInfo {gameGame, gamePar, gameVsUser, gameCharacters}) =
  component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState :: State
  initialState = { practice:   practice0
                 , par:        gamePar
                 , vs:         gameVsUser
                 , characters: gameCharacters
                 , game:       gameGame
                 , chakras:    getChakra gameGame gamePar
                 , randoms:    zero
                 , exchanged:  zero
                 , exchange:   false
                 , viewing:    ViewUser gameVsUser
                 , highlight:  []
                 , toggled:    Nothing
                 , acts:       []
                 , error:      ""
                 }
  render :: State -> ComponentHTML ChildQuery
  render st@{vs: vs@(User opponent), game: Game g } =
    H.div [_i "game"] $
    [ H.aside [_c "error"] [H.text st.error]
    , H.section [_i "top"] 
      [ H.section [_i "account0", hover $ ViewUser who]
        [ H.section_
          [ H.header_ [H.text account.name]
          , H.p_ [H.text $ userRank who]
          ]
        , H.aside_ [H.img [_c "charicon", _src account.avatar]]
        ]
      , H.article [_c "parchment"] $ view cs st.viewing
      , H.section [_i "account1", hover $ ViewUser vs]
        [ H.aside_ [H.img [_c "charicon", _src opponent.avatar]]
        , H.section_
          [ H.header_ [H.text opponent.name]
          , H.p_ [H.text $ userRank vs]
          ]
        ]
      ]
    , H.section [_i "player0", _c "player"] $ renderNinja true  <$> nsI
    , H.section [_i "player1", _c "player"] $ renderNinja false <$> nsU
    ] <> case g.gameVictor of 
      Nothing -> 
        [ H.section [_i "playchakra"] $
          (renderChakra turn st.exchange χNet <$> pairedChakras)
        <> [ renderRands (χSum χNet) χrand 
          , H.div 
            (mClick "exchange" "chakraButton" exchangeable $ ExchangeBegin) 
            [H.text "Exchange"]
          , H.div 
            ( mClick "reset" "chakraButton" (st.exchanged /= zero) $ 
              ExchangeReset
            ) [H.text "Reset"]
          ]
        , H.section [_i "playqueuecont"]
          [ H.div [_i "playqueue"] $ renderAct cs <$> st.acts
          , H.div (_i "ready" : readyMeta) []
          ]
        ]
      Just victor -> 
        [ H.section [_i "endgame"]
          [ H.p_
            [H.text $ if victor == st.par then "Victory" else "Defeat"]
          , _a "return" "playButton parchment click" "/" "Return"
          ] 
        ]
    where
      who@(User account)  = fromMaybe vs user
      χNet@(Chakras net)  = st.exchanged 
                            + foldl (-) st.chakras (actCost <$> st.acts)
      turn           = g.gamePlaying == st.par 
      χrand          = χSum st.randoms + net.rand - χrate * χSum st.exchanged
      freeRands      = χSum χNet + χrand
      free           = Chakras net { rand = freeRands }
      exchangeable   = freeRands >= 5 && canExchange χNet && turn
      acted          = _.actC <<< unwrap <$> st.acts
      readyMeta           
        | not turn   = [_c "noclick"]
        | χrand /= 0 = [_c "noChakra"]
        | otherwise  = [ _c "click"
                       , E.onClick <<< input <<< Ready st.practice $
                         enactUrl st.randoms st.exchanged st.acts  
                       ]
      cs             = zipWith mergeSkills st.characters g.gameNinjas
      renderNinja    = renderCharacter cs acted st.toggled st.highlight free turn
      pairedChakras  = [ pair "blood" _.blood $ χf _{ blood = 1 }
                       , pair "gen"   _.gen   $ χf _{ gen   = 1 }
                       , pair "nin"   _.nin   $ χf _{ nin   = 1 }
                       , pair "tai"   _.tai   $ χf _{ tai   = 1 }
                       ]
      pair name get χs = ChakraPair name χs (get net) (get $ unwrap st.randoms)
      pAlly (Zipped _ (Ninja {nId}) _) = allied st.par nId
      {yes: nsI, no: nsU} = partition pAlly $
                            zip3 Zipped cs g.gameNinjas g.gameTargets
      
  eval :: ChildQuery ~> ComponentDSL State ChildQuery Message m
  eval (QueryPlay query a) = a <$ case query of
      Enact Add act -> do
        sound SFXApplySkill
        modify_ \st -> untoggle st { acts = st.acts `snoc` act }
      Enact Delete act -> do
        sound SFXCancel
        modify_ \st -> untoggle st { acts = delete act st.acts }
      ExchangeBegin -> do
        sound SFXTarget
        modify_ \st -> st { exchange = not st.exchange }
      ExchangeConclude χ -> do
        sound SFXClick
        modify_ \st -> st { exchanged = st.exchanged + χ 
                          , exchange  = false
                          }
      ExchangeReset -> do
        sound SFXCancel
        modify_ \st -> st { chakras   = getChakra st.game st.par
                          , randoms   = zero :: Chakras
                          , exchanged = zero :: Chakras
                          , exchange  = false 
                          }
      ReceiveGame g'@(Game g) -> do
        case g.gameVictor of
            Just victor -> do
                {par}      <- get
                let winner = victor == par
                sound $ if winner then SFXWin else SFXLose
                raise $ Finish winner
            Nothing -> do
                sound SFXStartTurn
                {par, game} <- get
                liftEffect $ if g.gamePlaying == par 
                            then identity 1 0
                            else identity 0 1
                modify_ \st -> st { game    = g'
                                  , chakras   = getChakra g' par 
                                  , randoms   = zero :: Chakras
                                  , exchanged = zero :: Chakras
                                  , acts      = []
                                  } 
                when (living par game > living par g') $ sound SFXDeath
      Ready false url -> do
        sound SFXNextTurn
        raise <<< ActMsg $ SocketMsg url
      Ready true url -> do
        modify_ $ untoggle <<< _{ exchange = false, exchanged = (zero :: Chakras) }
        games <- liftAff <<< getJson $ "/api/practiceact/" <> url    
        case games of
            Left err -> modify_ _{ error = err }
            Right [x,y] -> do
              {par: oldPar, game: oldGame} <- get
              modify_ \st -> st { game      = x
                                , chakras   = getChakra x st.par 
                                , randoms   = zero :: Chakras
                                , exchanged = zero :: Chakras
                                , acts      = []
                                } 
              liftEffect $ progress practiceWait 0 1
              liftAff $ Aff.delay practiceWait
              liftEffect $ progress (Milliseconds 0.0) 1 1
              sound SFXStartTurn
              modify_ \st -> st { game      = y
                                , chakras   = getChakra y st.par
                                , randoms   = zero :: Chakras
                                , exchanged = zero :: Chakras
                                , acts      = []
                                , exchange  = false
                                } 
              when (living oldPar oldGame > living oldPar y) $ sound SFXDeath
            Right _ -> modify_ _{ error = "Unknown error" }
      Spend χs -> do
        sound SFXClick
        modify_ \st -> 
            st { randoms = st.randoms + χs, chakras = st.chakras - χs }
      Toggle skill -> do
        sound SFXTarget
        modify_ \st -> if st.toggled == Just skill 
                       then st { toggled = Nothing }
                       else st { toggled = Just skill }
      View viewing@(ViewSkill _ ts _ _) ->
        modify_ _{ viewing = viewing, highlight = ts }
      View viewing -> 
        modify_ _{ viewing = viewing }
      Unhighlight -> 
        modify_ _{ highlight = [] }
    where 
      untoggle     = _{ toggled = Nothing }
      identity     = progress $ Milliseconds 60000.0
      practiceWait = Milliseconds 1500.0
  eval (QuerySelect _ next) = pure next

_l :: ∀ a b. String -> HTML a b
_l l = H.span [_c "label"] [H.text l]

enactUrl :: Chakras -> Chakras -> Array Act -> String
enactUrl rand trade acts = intercalate "/" $ χList <> actList
  where 
    χList   = intercalate "," <<< map show <<< χList' <$> [rand, trade]
    actList = intercalate "," <<< map show <<< actList' <$> acts
    χList' (Chakras {blood, gen, nin, tai}) = [blood, gen, nin, tai]
    actList' (Act {actC, actS, actT}) = [actC, actS, actT]

renderChakra :: ∀ a. Boolean -> Boolean -> Chakras -> ChakraPair -> HTMLQ a
renderChakra turn exchange χs (ChakraPair chakra spend amount random) = H.div_ $
    [ H.div meta []
    , _span $ show amount
    , H.a 
      (mClick "" "more" (turn && random > 0) <<< Spend $ spend * intχ (-1))
      [H.text "+"]
    , H.a 
      (mClick "" "less" (turn && amount > 0) $ Spend spend) 
      [H.text "—"]
    , H.div [_c   "chakra rand"]  []
    , _span $ show random
]
  where 
    meta 
      | exchange  = mClick "" ("chakra " <> chakra) (affordable χs spend) $
                    ExchangeConclude spend
      | otherwise = [_c $ "chakra " <> chakra]
             
renderRands :: ∀ a. Int -> Int -> HTMLQ a
renderRands amount random = H.div [_c "randbar"]
    [ H.span [_c "randT"] [H.text "T"]
    , _span $ show amount
    , H.a [_c "more noclick"] [H.text "+"]
    , H.a [_c "less noclick"] [H.text "—"]
    , H.span [_c "randT"] [H.text "T"]
    , H.span (if random < 0 then [_c "negrand"] else []) [H.text $ show random]
    ]

renderAct :: ∀ a. Array Character -> Act -> HTMLQ a
renderAct cs x'@(Act x) = H.div 
    [_c "act click", E.onClick <<< input $ Enact Delete x']
    [ H.img $ [cIcon (getC cs $ skillRoot x.actSkill x.actC) $ show x.actSkill]
    , H.div [_c "actcost"] <<< hCost $ actCost x'
    ]

actToggles :: Maybe Act -> Array Int
actToggles Nothing = []
actToggles (Just (Act x)) = x.actTs `intersect` skillTarget x.actC x.actSkill

renderCharacter :: ∀ a. Array Character -> Array Int -> Maybe Act -> Array Int 
                -> Chakras -> Boolean -> Boolean -> Zipped -> HTMLQ a
renderCharacter cs acted toggle highlighted χs turn onTeam 
  (Zipped character n'@(Ninja n) ts) =
    (if n.nHealth == 0 then H.section [_c "dead"] else H.section_) $
      H.aside [_c "channels"] hChannels : mainBar <>
      [ H.div [_c "charhealth"] $
        [ H.div [_style $ "width: " <> hp] []
        , H.span [_c "charhealthtext", _style $ anchor <> hp] $
          live [H.text $ show n.nHealth]
        ] <> hDefenses
      , H.aside [_c "statuses"] hInfos
      ]
  where 
    hp        = show n.nHealth <> "%"
    live els  
      | n.nHealth > 0 = els
      | otherwise   = []
    hChannels = live $ renderChannel cs <$> reverse n.nChannels
    hDefenses = live $ renderDefense n.nId anchor n.nHealth 
                (reverse n.nBarrier) (reverse n.nDefense)
    hInfos    = live <<< map (renderInfo onTeam n.nId cs) <<< 
                onTeam ? reverse $ infos n'
    active    = onTeam && turn && n.nHealth > 0 && n.nId `notElem` acted
    classes   = catMaybes [ n.nId `elem` highlighted       ?? " highlighted"
                          , n.nId `elem` actToggles toggle ?? " toggled skill"
                          ] 
    mainMeta  = catMaybes [ not (null classes) ?? _c (intercalate " " classes)
                          , Just <<< hover $ ViewCharacter character
                          , E.onClick <<< input <<< Enact Add <<< retarget <$>
                            (guard (n.nId `elem` actToggles toggle) *> toggle)
                          ]
    mainBar   = not onTeam ? reverse $
                [ H.section mainMeta
                  [H.img [_c "charicon", faceIcon']]
                , H.div [_c "charmoves"]
                  <<< zip5 (renderSkill n.nId χs active cs) (0..3) ts 
                      (n.nCharges <> [0,0,0,0]) n.nSkills 
                  <<< (n.nHealth > 0) ? (n.nCooldowns <> _) $ [0, 0, 0, 0]
                ]
    anchor    
      | onTeam    = "left: "
      | otherwise = "right: "
    faceIcon' = case head n.nFace of
        Nothing       -> cIcon character "icon"
        Just (Face f) -> cIcon (getC cs f.faceSrc) $ "icon" <> f.faceIcon
    retarget (Act act) = Act $ act { actT = n.nId }

sync :: ∀ a b. Int -> HTML a b
sync 0   = H.text "Permanent"
sync dur = H.text <<< show $ (dur + 1) / 2

renderChannel :: ∀ a. Array Character -> Channel -> HTMLQ a
renderChannel cs (Channel ch) = H.div
    [ _c    $ classF ch.channelDur "status"
    , hover $ ViewSkill ch.channelRoot [] 0 ch.channelSkill
    ]
    [ H.span_ if dur == 0 then [] else [sync dur]
    , H.div_
      [ H.img [cIcon (getC cs ch.channelRoot) $ show ch.channelSkill] ]
    ]
  where 
    dur                = channelingDur ch.channelDur
    classF (Action _)  = ("action "  <> _)
    classF (Control _) = ("control " <> _)
    classF _           = identity
       
renderBarrier :: ∀ a. Int -> String -> Int -> Array Barrier 
         -> Array (HTMLQ a)
renderBarrier nId anchor track barriers = case uncons barriers of
    Nothing                           -> []
    Just {head: b'@(Barrier b), tail} -> H.div 
        [ _c "charbarrier"
        , _style $ 
          anchor <> show track <> "%; width: " <> show b.barrierAmount <> "%"
        , hover $ ViewBarrier b'
        ] [] : renderBarrier nId anchor (track + b.barrierAmount) tail
 
renderDefense :: ∀ a. Int -> String -> Int -> Array Barrier -> Array Defense
         -> Array (HTMLQ a)
renderDefense nId anchor track barriers defenses = case uncons defenses of
    Nothing -> renderBarrier nId anchor track barriers
    Just {head: d'@(Defense d), tail} -> H.div 
        [ _c "chardefense"
        , _style $ 
          anchor <> show track <> "%; width: " <> show d.defenseAmount <> "%"
        , hover $ ViewDefense d'
        ]
        [] : renderDefense nId anchor (track + d.defenseAmount) barriers tail

renderSkill :: ∀ a. Int -> Chakras -> Boolean -> Array Character -> Int 
            -> Array Int -> Int -> Skill -> Int -> HTMLQ a
renderSkill nId χs active cs sI ts charge s'@(Skill s) cd
    = case unit of
 _| cd > 0 -> cant
              [ H.img [cIcon (getC cs $ skillRoot s' nId) s.label]
              , H.span_ [H.text <<< show <<< max 1 $ cd / 2]
              ]
 _| not active || s.require == Unusable || lacks (χs - s.cost) || null ts 
            -> cant 
              [H.img [cIcon (getC cs $ skillRoot s' nId) s.label]]
 _| otherwise -> H.div 
              [ _c "charmove click"
              , hover $ ViewSkill nId ts charge s'
              , E.onMouseLeave $ input Unhighlight
              , E.onClick $ input action
              ]
              [H.img [cIcon (getC cs $ skillRoot s' nId) s.label]]
  where 
    act = Act { actC: nId, actS: sI, actT: nId, actSkill: s', actTs: ts }
    action 
      | skillTarget nId s' == [nId] = Enact Add act
      | otherwise                   = Toggle act
    cant = H.div
           [ _c "charmove noclick"
           , hover $ ViewSkill nId [] charge s'
           , E.onMouseLeave $ input Unhighlight
           ] 

fromAlly :: Int -> Info -> Boolean
fromAlly c info = allied c info.root && allied c info.src

renderInfo :: ∀ a. Boolean -> Int -> Array Character -> Info -> HTMLQ a
renderInfo team nId cs info = H.div
    [_c $ intercalate " " classes', hover $ ViewInfo removes info]
    [ H.div_ <<< (info.count > 1) ? consAfter (_span $ show info.count) $
      [H.img [cIcon (getC cs info.root) info.name]]
    , H.p_ $ if info.dur == 0 then [] else [sync info.dur]
    ]
  where 
    classes' = catMaybes 
        [ Just                                 "status" 
        , info.trap                         ?? "trap"
        , info.ghost || info.dur == 1       ?? "tag"
        , ("Shifted" `elem` info.classes)   ?? "reflected"
        , ("Invisible" `elem` info.classes) ?? "fade"
        , "Unremovable" `notElem` info.classes 
          && any removes info.effects ?? "remove"
        ]
    removes 
      | fromAlly nId info = const false
      | otherwise         = removable team

viewBar :: ∀ a. Character -> String -> Int -> Int -> Array (HTMLQ a)
viewBar src l amount dur =
    [ H.div_
      [ H.aside_ [ H.img [_c "char", cIcon src l ] ]
      , H.div_
        [ H.header_ [ H.text l ]
        , _l "Amount: ",   H.div_ [ H.text $ show amount ]
        , _l "Duration: ", H.div_ [ sync dur ]
        , _l "Source: ",   H.div_ (charName src)
        ]
      ]
    ]

viewClasses :: ∀ a b. Boolean -> Array String -> HTML a b
viewClasses hideMore classes = H.p [_c "skillClasses"]
  [ H.text <<< intercalate ", " $ filterClasses hideMore classes ]

viewEffect :: ∀ a b. (SkillEffect -> Boolean) -> SkillEffect -> HTML a b
viewEffect removes ef'@(SkillEffect ef) = case unit of 
 _| ef.effectTrap  -> H.div [_c "trap"] des
 _| removes ef'    -> H.div [_c "remove"] des
 _| otherwise      -> H.div_ des
  where 
    des = descEffect ef'
descEffect :: ∀ a b. SkillEffect -> Array (HTML a b)
descEffect (SkillEffect ef) = [H.text $ "- " <> ef.effectDesc]

view :: ∀ a. Array Character -> Viewable -> Array (HTMLQ a)
view cs (ViewBarrier (Barrier b)) =
    viewBar (getC cs b.barrierSrc) b.barrierL b.barrierAmount b.barrierDur

view cs (ViewDefense (Defense d)) =
    viewBar (getC cs d.defenseSrc) d.defenseL d.defenseAmount d.defenseDur

view cs (ViewCharacter c'@(Character c)) =
    [ H.div_
      [ H.aside_ [ H.img [_c "char", cIcon c' "icon"] ]
      , H.div_
        [ H.header_ $ charName c'
        , H.p_ [ H.text c.characterBio ]
        ]
      ]
    ]

view cs (ViewInfo removes info) =
    [ H.div_
      [ H.aside_ [H.img [_c "char", cIcon (getC cs info.root) "icon"]]
      , H.div_
        [ H.header_ [ H.text info.name ]
        , viewClasses true info.classes
        , _l "Source: ",   H.div_ <<< charName $ getC cs info.src
        , _l "Duration: ", H.div_ [sync info.dur]
        , H.section_     $ viewEffect removes <$> info.effects
        ]
      ]
    , H.p_             $ parseDesc info.desc
    ]
  where 
    name' 
      | "Shifted" `elem` info.classes = H.span [_c "reflected"] 
                                        [ H.text $ info.name <> " (Reflected)" ]
      | otherwise                     = H.span_ [ H.text $ info.name ]

view cs (ViewSkill nId _ charge s'@(Skill s)) =
    [ H.div_
      [ H.aside_ $ 
        H.img [_c "char", cIcon (getC cs $ skillRoot s' nId) s.label] :
        varyButtons
      , H.div_
        [ H.header_ [ H.text s.label ]
        , viewClasses false s.classes
        , _l "Cost: ",     H.div_ cost'
        , _l "Duration: ", H.div_ [H.text $ skillDur s']
        , _l "Cooldown: ", H.div_ [H.text $ show s.cd]
        ]
      ]
    , H.p_ <<< charged $ parseDesc s.desc
    ]
  where 
    cd'         
      | s.cd == 0    = "None"
      | otherwise = show s.cd
    cost'       = cost'' s.cost
    cost'' χs   
      | χs == zero = [H.text "Free"]
      | otherwise = hCost χs    
    charged     
      | s.charges == 0          = identity
      | s.charges - charge == 1 = (_ `snoc` _extra "1 charge.")
      | otherwise = (_ `snoc` _extra (show (s.charges - charge) <> " charges."))
    varyButtons = fromMaybe [] do
      Character {characterSkills} <- cs !! skillRoot s' nId
      skills <- find (any (lMatch s')) characterSkills
      i <- Array.findIndex (lMatch s') skills
      pure $ catMaybes
        [ vPrev skills i <#> \v -> H.a 
          [ _c "prevSkill click"
          , E.onClick <<< input <<< View $ ViewSkill nId [] charge v
          ] []
        , vNext skills i <#> \v -> H.a 
          [ _c "nextSkill click"
          , E.onClick <<< input <<< View $ ViewSkill nId [] charge v
          ] []
        ] 

view cs (ViewUser u'@(User u)) =
    [ H.div_
      [ H.aside_ [H.img [_c "char", _src u.avatar]]
      , H.div_
        [ H.header_ [ H.text u.name ]
        , H.div_ [ H.text $ userRank u' ]
        , _l "Clan: ",  H.div_ [ H.text $ fromMaybe "Clanless" u.clan ]
        , _l "Level: ", H.div_ [ H.text <<< show $ userLevel u' ]
        ]
      ]
    ]

vNext :: Array Skill -> Int -> Maybe Skill
vNext skills i = do
    skill <- skills !! i
    find (not <<< lMatch skill) $ drop i skills

vPrev :: Array Skill -> Int -> Maybe Skill
vPrev skills i = do
    skill <- skills !! i
    find (not <<< lMatch skill) <<< reverse $ take i skills
        