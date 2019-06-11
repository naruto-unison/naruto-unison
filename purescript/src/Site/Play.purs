module Site.Play (Message(..), comp) where

import Prelude
import Data.Array as Array
import Data.Array ((!!), (:), (..), catMaybes, delete, drop, filter, head, intersect, null, partition, reverse, snoc, take, uncons, zipWith)
import Data.Either (Either(..))
import Data.Foldable (any, elem, find, foldl, notElem, intercalate)
import Data.Int (even)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen (Component, ComponentDSL, ComponentHTML, component, get, modify_, raise)
import Halogen.HTML as H
import Halogen.HTML (HTML)
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Web.UIEvent.MouseEvent (MouseEvent)

import FFI.Import (getJson, user)
import FFI.Progress (progress)
import FFI.Sound (Sound(..), sound)
import Site.Common (ArrayOp(..), ChildQuery(..), HTMLQ, PlayQuery(..), SocketMsg(..), Viewable(..), _a, _c, _extra, _i, _span, _src, _style)
import Model (Act(..), Barrier(..), Channel(..), Defense(..), Face(..), Ninja(..), actCost)
import Model.Chakra as Chakra
import Model.Chakra (Chakras(..))
import Model.Channeling as Channeling
import Model.Channeling (Channeling(..))
import Model.Character as Character
import Model.Character (Character(..))
import Model.Class as Class
import Model.Game as Game
import Model.Game (Game(..), GameInfo(..))
import Model.Info (Info, infos)
import Model.Skill as Skill
import Model.Skill (Skill(..), Requirement(..))
import Model.Status as Status
import Model.Status (Effect(..))
import Model.User as User
import Model.User (User(..))
import Util ((?), appendIf, zip3, zip5)

data Message
    = ActMsg SocketMsg
    | Finish Boolean

type State = { practice   :: Boolean
             , player     :: Int
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

comp :: ∀ m. MonadAff m => Boolean -> GameInfo
     -> Component HTML ChildQuery Unit Message m
comp practice0 (GameInfo gi) =
  component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState :: State
  initialState = { practice:   practice0
                 , player:     gi.player
                 , vs:         gi.opponent
                 , characters: gi.characters
                 , game:       gi.game
                 , chakras:    Game.getChakra gi.game gi.player
                 , randoms:    zero
                 , exchanged:  zero
                 , exchange:   false
                 , viewing:    ViewUser gi.opponent
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
          , H.p_ [H.text $ User.rank who]
          ]
        , H.aside_ [H.img [_c "charicon", _src account.avatar]]
        ]
      , H.article [_c "parchment"] $ view cs st.viewing
      , H.section [_i "account1", hover $ ViewUser vs]
        [ H.aside_ [H.img [_c "charicon", _src opponent.avatar]]
        , H.section_
          [ H.header_ [H.text opponent.name]
          , H.p_ [H.text $ User.rank vs]
          ]
        ]
      ]
    , H.section [_i "player0", _c "player"] $ renderNinja true  <$> nsI
    , H.section [_i "player1", _c "player"] $ renderNinja false <$> nsU
    ] <> case g.victor of
      [] ->
        [ H.section [_i "playchakra"] $
          (renderChakra turn st.exchange kNet <$> pairedChakras)
        <> [ renderRands (Chakra.total kNet) kRand
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
      [victor] ->
        [ H.section [_i "endgame"]
          [ H.p_
            [H.text $ if victor == st.player then "Victory" else "Defeat"]
          , _a "return" "playButton parchment click" "/" "Return"
          ]
        ]
      _ ->
        [ H.section [_i "endgame"]
          [ H.p_
            [H.text "Tie"]
          , _a "return" "playButton parchment click" "/" "Return"
          ]
        ]
    where
      who@(User account)  = fromMaybe vs user
      kNet@(Chakras net)  = st.exchanged
                            + foldl (-) st.chakras (actCost <$> st.acts)
      turn           = g.playing == st.player
      kRand          = Chakra.total st.randoms + net.rand
                       - Chakra.rate * Chakra.total st.exchanged
      freeRands      = Chakra.total kNet + kRand
      free           = Chakras net { rand = freeRands }
      exchangeable   = freeRands >= 5 && Chakra.canExchange kNet && turn
      acted          = _.user <<< unwrap <$> st.acts
      readyMeta
        | not turn   = [_c "noclick"]
        | kRand /= 0 = [_c "noChakra"]
        | otherwise  = [ _c "click"
                       , E.onClick <<< input <<< Ready st.practice $
                         enactUrl st.randoms st.exchanged st.acts
                       ]
      cs             = zipWith Character.mergeSkills st.characters g.ninjas
      renderNinja    = renderCharacter cs acted st.toggled st.highlight free turn
      pairedChakras  = [ pair "blood" _.blood $ Chakra.map _{ blood = 1 }
                       , pair "gen"   _.gen   $ Chakra.map _{ gen   = 1 }
                       , pair "nin"   _.nin   $ Chakra.map _{ nin   = 1 }
                       , pair "tai"   _.tai   $ Chakra.map _{ tai   = 1 }
                       ]
      pair name get ks = ChakraPair name ks (get net) (get $ unwrap st.randoms)
      pAlly (Zipped _ (Ninja {slot}) _) = even st.player == even slot
      {yes: nsI, no: nsU} = partition pAlly $
                            zip3 Zipped cs g.ninjas g.targets

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
      ExchangeConclude k -> do
        sound SFXClick
        modify_ \st -> st { exchanged = st.exchanged + k
                          , exchange  = false
                          }
      ExchangeReset -> do
        sound SFXCancel
        modify_ \st -> st { chakras   = Game.getChakra st.game st.player
                          , randoms   = zero :: Chakras
                          , exchanged = zero :: Chakras
                          , exchange  = false
                          }
      ReceiveGame g'@(Game g) -> do
        case g.victor of
            [victor] -> do
                {player}      <- get
                let winner = victor == player
                sound $ if winner then SFXWin else SFXLose
                raise $ Finish winner
            [] -> do
                sound SFXStartTurn
                {player, game} <- get
                liftEffect $ if g.playing == player
                            then identity 1 0
                            else identity 0 1
                modify_ \st -> st { game    = g'
                                  , chakras   = Game.getChakra g' player
                                  , randoms   = zero :: Chakras
                                  , exchanged = zero :: Chakras
                                  , acts      = []
                                  }
                when (Game.living player game > Game.living player g') $
                    sound SFXDeath
            _ -> do
                sound SFXLose
                raise $ Finish false
      Ready false url -> do
        sound SFXNextTurn
        raise <<< ActMsg $ SocketMsg url
      Ready true url -> do
        modify_ $ untoggle <<< _{ exchange = false, exchanged = (zero :: Chakras) }
        games <- liftAff <<< getJson $ "/api/practiceact/" <> url
        case games of
            Left err -> modify_ _{ error = err }
            Right [x,y] -> do
              {player: oldPar, game: oldGame} <- get
              modify_ \st -> st { game      = x
                                , chakras   = Game.getChakra x st.player
                                , randoms   = zero :: Chakras
                                , exchanged = zero :: Chakras
                                , acts      = []
                                }
              liftEffect $ progress practiceWait 0 1
              liftAff $ Aff.delay practiceWait
              liftEffect $ progress (Milliseconds 0.0) 1 1
              sound SFXStartTurn
              modify_ \st -> st { game      = y
                                , chakras   = Game.getChakra y st.player
                                , randoms   = zero :: Chakras
                                , exchanged = zero :: Chakras
                                , acts      = []
                                , exchange  = false
                                }
              when (Game.living oldPar oldGame > Game.living oldPar y) $
                  sound SFXDeath
            Right _ -> modify_ _{ error = "Unknown error" }
      Spend ks -> do
        sound SFXClick
        modify_ \st ->
            st { randoms = st.randoms + ks, chakras = st.chakras - ks }
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
enactUrl rand trade acts = intercalate "/" $ kList <> actList
  where
    kList   = intercalate "," <<< (show <$> _) <<< kList' <$> [rand, trade]
    actList = intercalate "," <<< (show <$> _) <<< actList' <$> acts
    kList' (Chakras {blood, gen, nin, tai}) = [blood, gen, nin, tai]
    actList' (Act {user, skill, target}) = [user, skill, target]

renderChakra :: ∀ a. Boolean -> Boolean -> Chakras -> ChakraPair -> HTMLQ a
renderChakra turn exchange ks (ChakraPair chakra spend amount random) = H.div_ $
    [ H.div meta []
    , _span $ show amount
    , H.a
      (mClick "" "more" (turn && random > 0) <<<
        Spend $ spend * Chakra.fromInt (-1))
      [H.text "+"]
    , H.a
      (mClick "" "less" (turn && amount > 0) $ Spend spend)
      [H.text "—"]
    , H.div [_c   "chakra rand"]  []
    , _span $ show random
]
  where
    meta
      | exchange  = mClick "" ("chakra " <> chakra)
                    (Chakra.affordable ks spend) $ ExchangeConclude spend
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
    [ H.img $ [Character.icon (Character.get cs $ Skill.root x.skill' x.user)
               $ show x.skill']
    , H.div [_c "actcost"] <<< Chakra.cost $ actCost x'
    ]

actToggles :: Maybe Act -> Array Int
actToggles Nothing = []
actToggles (Just (Act x)) = x.targets `intersect` Skill.targets x.user x.skill'

renderCharacter :: ∀ a. Array Character -> Array Int -> Maybe Act -> Array Int
                -> Chakras -> Boolean -> Boolean -> Zipped -> HTMLQ a
renderCharacter cs acted toggle highlighted ks turn onTeam
  (Zipped character n'@(Ninja n) ts) =
    (if n.health == 0 then H.section [_c "dead"] else H.section_) $
      H.aside [_c "channels"] hChannels : mainBar <>
      [ H.div [_c "charhealth"] $
        [ H.div [_style $ "width: " <> hp] []
        , H.span [_c "charhealthtext", _style $ anchor <> hp] $
          live [H.text $ show n.health]
        ] <> hDefenses
      , H.aside [_c "statuses"] hInfos
      ]
  where
    hp        = show n.health <> "%"
    live els
      | n.health > 0 = els
      | otherwise    = []
    hChannels = live $ renderChannel cs <$> reverse n.channels
    hDefenses = live $ renderDefense n.slot anchor n.health
                (reverse n.barrier) (reverse n.defense)
    hInfos    = live <<< map (renderInfo onTeam n.slot cs) <<<
                (onTeam ? reverse) $ infos n'
    active    = onTeam && turn && n.health > 0 && n.slot `notElem` acted
    classes   = (fst <$> _) $ filter snd
                [ Tuple "highlighted"   $ n.slot `elem` highlighted
                , Tuple "toggled skill" $ n.slot `elem` actToggles toggle
                ]
    mainMeta  = (fst <$> _) $ filter snd
                [ Tuple (_c $ intercalate " " classes) <<< not $ null classes
                , Tuple (hover $ ViewCharacter character) true
                ]
    fullMeta  = case toggle of
        Just tog | n.slot `elem` actToggles toggle ->
            (E.onClick <<< input <<< Enact Add $ retarget tog) : mainMeta
        _ -> mainMeta
    mainBar   = (not onTeam ? reverse)
                [ H.section fullMeta
                  [H.img [_c "charicon", faceIcon']]
                , H.div [_c "charmoves"]
                  <<< zip5 (renderSkill n.slot ks active cs) (0..3) ts
                      (n.charges <> [0,0,0,0]) n.skills
                    $ (n.health > 0 ? (n.cooldowns <> _)) [0, 0, 0, 0]
                ]
    anchor
      | onTeam    = "left: "
      | otherwise = "right: "
    faceIcon' = case head n.face of
        Nothing       -> Character.icon character "icon"
        Just (Face f) -> Character.icon (Character.get cs f.source) $
                         "icon" <> f.icon
    retarget (Act act) = Act $ act { target = n.slot }

sync :: ∀ a b. Int -> HTML a b
sync 0   = H.text "Permanent"
sync dur = H.text <<< show $ (dur + 1) / 2

renderChannel :: ∀ a. Array Character -> Channel -> HTMLQ a
renderChannel cs (Channel chan) = H.div
    [ _c    $ classF chan.dur "status"
    , hover $ ViewSkill chan.root [] 0 chan.skill
    ]
    [ H.span_ if dur == 0 then [] else [sync dur]
    , H.div_
      [ H.img [Character.icon (Character.get cs chan.root) $
               show chan.skill] ]
    ]
  where
    dur                = Channeling.dur chan.dur
    classF (Action _)  = ("action "  <> _)
    classF (Control _) = ("control " <> _)
    classF _           = identity

renderBarrier :: ∀ a. Int -> String -> Int -> Array Barrier
         -> Array (HTMLQ a)
renderBarrier slot anchor track barriers = case uncons barriers of
    Nothing                           -> []
    Just {head: b'@(Barrier b), tail} -> H.div
        [ _c "charbarrier"
        , _style $
          anchor <> show track <> "%; width: " <> show b.amount <> "%"
        , hover $ ViewBarrier b'
        ] [] : renderBarrier slot anchor (track + b.amount) tail

renderDefense :: ∀ a. Int -> String -> Int -> Array Barrier -> Array Defense
         -> Array (HTMLQ a)
renderDefense slot anchor track barriers defenses = case uncons defenses of
    Nothing -> renderBarrier slot anchor track barriers
    Just {head: d'@(Defense d), tail} -> H.div
        [ _c "chardefense"
        , _style $
          anchor <> show track <> "%; width: " <> show d.amount <> "%"
        , hover $ ViewDefense d'
        ]
        [] : renderDefense slot anchor (track + d.amount) barriers tail

renderSkill :: ∀ a. Int -> Chakras -> Boolean -> Array Character -> Int
            -> Array Int -> Int -> Skill -> Int -> HTMLQ a
renderSkill slot ks active cs sI ts charge s'@(Skill s) cd = go
  where
    -- Since PureScript doesn't yet support where-binding across guard patterns
    go
      | cd > 0 =
          cant
          [ H.img [Character.icon
                   (Character.get cs $ Skill.root s' slot) s.name]
          , H.span_ [H.text <<< show <<< max 1 $ cd / 2]
          ]
      | not active || s.require == Unusable || Chakra.lack (ks - s.cost)
        || null ts =
          cant
          [H.img [Character.icon (Character.get cs $ Skill.root s' slot) s.name]]
      | otherwise =
          H.div
          [ _c "charmove click"
          , hover $ ViewSkill slot ts charge s'
          , E.onMouseLeave $ input Unhighlight
          , E.onClick $ input action
          ]
          [H.img [Character.icon
                  (Character.get cs $ Skill.root s' slot) s.name]]
  -- where
    act = Act { user: slot, skill: sI, target: slot, skill': s', targets: ts }
    action
      | Skill.targets slot s' == [slot] = Enact Add act
      | otherwise                     = Toggle act
    cant = H.div
           [ _c "charmove noclick"
           , hover $ ViewSkill slot [] charge s'
           , E.onMouseLeave $ input Unhighlight
           ]

fromAlly :: Int -> Info -> Boolean
fromAlly c info = even c == even info.root && even c == even info.src

renderInfo :: ∀ a. Boolean -> Int -> Array Character -> Info -> HTMLQ a
renderInfo team slot cs info = H.div
    [_c $ intercalate " " classes', hover $ ViewInfo removes info]
    [ H.div_ $ ((_span $ show info.count) `appendIf` (info.count > 1))
      [H.img [Character.icon (Character.get cs info.root) info.name]]
    , H.p_ $ if info.dur == 0 then [] else [sync info.dur]
    ]
  where
    classes' = (fst <$> _) $ filter snd
        [ Tuple "status"      true
        , Tuple "trap"        info.trap
        , Tuple "tag"       $ info.ghost || info.dur == 1
        , Tuple "reflected" $ "Shifted" `elem` info.classes
        , Tuple "remove"    $ "Unremovable" `notElem` info.classes
                              && any removes info.effects
        ]
    {-}
        [ Tuple true,                            "status")
        , Tuple info.trap,                       "trap")
        , Tuple info.ghost || info.dur == 1,     "tag")
        , "Shifted" `elem` info.classes,   "reflected")
        , ("Invisible" `elem` info.classes, "fade")
        , ("Unremovable" `notElem` info.classes && any removes info.effects
           , "remove")
        ]-}
    removes
      | fromAlly slot info = const false
      | otherwise         = Status.removable team

viewBar :: ∀ a. Character -> String -> Int -> Int -> Array (HTMLQ a)
viewBar src l amount dur =
    [ H.div_
      [ H.aside_ [ H.img [_c "char", Character.icon src l ] ]
      , H.div_
        [ H.header_ [ H.text l ]
        , _l "Amount: ",   H.div_ [ H.text $ show amount ]
        , _l "Duration: ", H.div_ [ sync dur ]
        , _l "Source: ",   H.div_ (Character.name src)
        ]
      ]
    ]

viewClasses :: ∀ a b. Boolean -> Array String -> HTML a b
viewClasses hideMore classes = H.p [_c "skillClasses"]
  [ H.text <<< intercalate ", " $ Class.filter hideMore classes ]

viewEffect :: ∀ a b. (Effect -> Boolean) -> Effect -> HTML a b
viewEffect removes ef'@(Effect ef) = go
  where
    -- Since PureScript doesn't yet support where-binding across guard patterns
    go
      | ef.trap     = H.div [_c "trap"] des
      | removes ef' = H.div [_c "remove"] des
      | otherwise   = H.div_ des
  -- where
    des = descEffect ef'
descEffect :: ∀ a b. Effect -> Array (HTML a b)
descEffect (Effect ef) = [H.text $ "- " <> ef.desc]

view :: ∀ a. Array Character -> Viewable -> Array (HTMLQ a)
view cs (ViewBarrier (Barrier b)) =
    viewBar (Character.get cs b.source) b.name b.amount b.dur

view cs (ViewDefense (Defense d)) =
    viewBar (Character.get cs d.source) d.name d.amount d.dur

view cs (ViewCharacter c'@(Character c)) =
    [ H.div_
      [ H.aside_ [ H.img [_c "char", Character.icon c' "icon"] ]
      , H.div_
        [ H.header_ $ Character.name c'
        , H.p_ [ H.text c.bio ]
        ]
      ]
    ]

view cs (ViewInfo removes info) =
    [ H.div_
      [ H.aside_
        [ H.img
          [ _c "char"
          , Character.icon (Character.get cs info.root) info.name
          ]
        ]
      , H.div_
        [ H.header_ [ H.text info.name ]
        , viewClasses true info.classes
        , _l "Source: ",   H.div_ <<< Character.name $ Character.get cs info.src
        , _l "Duration: ", H.div_ [sync info.dur]
        , H.section_     $ viewEffect removes <$> info.effects
        ]
      ]
    , H.p_             $ Skill.parseDesc info.desc
    ]
  where
    name'
      | "Shifted" `elem` info.classes = H.span [_c "reflected"]
                                        [ H.text $ info.name <> " (Reflected)" ]
      | otherwise                     = H.span_ [ H.text $ info.name ]

view cs (ViewSkill slot _ charge s'@(Skill s)) =
    [ H.div_
      [ H.aside_ $
        H.img [_c "char", Character.icon
                          (Character.get cs $ Skill.root s' slot) s.name] :
        varyButtons
      , H.div_
        [ H.header_ [ H.text s.name ]
        , viewClasses false s.classes
        , _l "Cost: ",     H.div_ cost'
        , _l "Duration: ", H.div_ [H.text $ Skill.dur s']
        , _l "Cooldown: ", H.div_ [H.text $ show s.cooldown]
        ]
      ]
    , H.p_ <<< charged $ Skill.parseDesc s.desc
    ]
  where
    cd'
      | s.cooldown == 0 = "None"
      | otherwise       = show s.cooldown
    cost'               = cost'' s.cost
    cost'' ks
      | ks == zero = [H.text "Free"]
      | otherwise = Chakra.cost ks
    charged
      | s.charges == 0          = identity
      | s.charges - charge == 1 = (_ `snoc` _extra "1 charge.")
      | otherwise = (_ `snoc` _extra (show (s.charges - charge) <> " charges."))
    varyButtons = fromMaybe [] do
      Character {skills} <- cs !! Skill.root s' slot
      matches <- find (any (Skill.match s')) skills
      i <- Array.findIndex (Skill.match s') matches
      pure $ catMaybes
        [ vPrev matches i <#> \v -> H.a
          [ _c "prevSkill click"
          , E.onClick <<< input <<< View $ ViewSkill slot [] charge v
          ] []
        , vNext matches i <#> \v -> H.a
          [ _c "nextSkill click"
          , E.onClick <<< input <<< View $ ViewSkill slot [] charge v
          ] []
        ]

view cs (ViewUser u'@(User u)) =
    [ H.div_
      [ H.aside_ [H.img [_c "char", _src u.avatar]]
      , H.div_
        [ H.header_ [ H.text u.name ]
        , H.div_ [ H.text $ User.rank u' ]
        , _l "Clan: ",  H.div_ [ H.text $ fromMaybe "Clanless" u.clan ]
        , _l "Level: ", H.div_ [ H.text <<< show $ User.level u' ]
        ]
      ]
    ]

vNext :: Array Skill -> Int -> Maybe Skill
vNext skills i = do
    skill <- skills !! i
    find (not <<< Skill.match skill) $ drop i skills

vPrev :: Array Skill -> Int -> Maybe Skill
vPrev skills i = do
    skill <- skills !! i
    find (not <<< Skill.match skill) <<< reverse $ take i skills
