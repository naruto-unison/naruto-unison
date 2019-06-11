module Site.CharacterSelect
    ( Message(..)
    , State(..)
    , comp
    ) where

import Prelude
import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Data.Array as Array
import Data.Array ((:), (!!), (..), catMaybes, cons, delete, drop, elem, filter, length, reverse, snoc, take)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty as NonEmpty
import Data.NonEmpty ((:|))
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Global.Unsafe as Global
import Halogen (Component, ComponentDSL, ComponentHTML, component, get, modify_, raise)
import Halogen.HTML as H
import Halogen.HTML (HTML)
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Web.UIEvent.MouseEvent (MouseEvent)

import Characters as Characters
import FFI.Import (avatars, csrf, getPageSize, reload, user, userTeam)
import FFI.Sound (Sound(..), sound)
import FFI.Form (getForm)
import Model.Chakra as Chakra
import Model.Character as Character
import Model.Character (Character(..))
import Model.Class as Class
import Model.Skill as Skill
import Model.Skill (Skill(..))
import Model.User as User
import Model.User (User(..))
import Site.Common (ArrayOp(..), ChildQuery(..), HTMLQ, Previewing(..), QueueType(..), SelectQuery(..), SocketMsg, _a, _b, _c, _extra, _i, _span, _src)
import Util ((?), unzeroMod, zip3)

condensed :: Boolean
condensed = maybe false (_.condense <<< unwrap) user

csSize :: Int
csSize
  | condensed = length Characters.groupList
  | otherwise = length Characters.list

settingsId :: String
settingsId = "accountSettings"

data Message
    = Queued QueueType (Array Character)
    | UpdateMsg SocketMsg
    | Blank

type State = { queueing   :: Boolean
             , showLogin  :: Boolean
             , index      :: Int
             , cols       :: Int
             , toggled    :: Maybe Character
             , previewing :: Previewing
             , team       :: Array Character
             , variants   :: Array Int
             , avatar     :: Maybe String
             , pageSize   :: Int
             , updateFail :: Maybe String
             }

click :: ∀ a. SelectQuery
      -> P.IProp (onClick :: MouseEvent | a) (ChildQuery Unit)
click = E.onClick <<< E.input_ <<< QuerySelect

preview :: ∀ a. Previewing
        -> P.IProp (onMouseEnter :: MouseEvent | a) (ChildQuery Unit)
preview = E.onMouseEnter <<< E.input_ <<< QuerySelect <<< Preview

comp :: ∀ m. MonadAff m => Component HTML ChildQuery Unit Message m
comp = component
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
                 , toggled:    Nothing
                 , previewing: NoPreview
                 , team:       userTeam
                 , variants:   [0,0,0,0]
                 , avatar:     _.avatar <<< unwrap <$> user
                 , pageSize:   36
                 , updateFail: Nothing
                 }

  render :: State -> ComponentHTML ChildQuery
  render st =
      H.section [_i "charSelect"] $
        userBox st.showLogin st.team <>
        previewBox st
        [ H.section [_i "characterButtons", _c "parchment"]
          [ H.aside
            [ _i "prevPage"
            , _c $ (atMin ? ("wraparound " <> _)) "click"
            , click $ Scroll (-1)
            ] []
          , H.aside
            [ _i "nextPage"
            , _c $ (atMax ? ("wraparound " <> _)) "click"
            , click $ Scroll 1
            ] []
          , H.div
            [ _i "charScroll"
            , E.onMouseEnter <<< E.input_ $ QuerySelect Untoggle
            ] $ displays <#> \c ->
                H.div [_c $ cClass c]
                [ H.img
                  [Character.icon c "icon"
                  , preview $ PreviewChar c
                  , click $ Team Add c
                  ]
                ]
          ]
        ]
    where
      displays
        | condensed = NonEmpty.head <$> drop (st.index) Characters.groupList
        | otherwise = drop (st.index) Characters.list
      playButton
        | length st.team == 3 = "click playButton parchment"
        |otherwise            = "playButton parchment"
      atMin      = st.index == 0
      atMax      = st.index + st.pageSize >= csSize
      cClass c
        | c `elem` st.team     = "char disabled"
        | Just c == st.toggled = "char click selected"
        | isJust st.toggled    = "char"
        | otherwise            = "char click"

  eval :: ChildQuery ~> ComponentDSL State ChildQuery Message m
  eval (QuerySelect query a) = a <$ case query of
      SwitchLogin ->
        modify_ \st -> st { showLogin = not st.showLogin }
      Scroll x -> do
        sound SFXScroll
        pageSize <- liftEffect getPageSize
        modify_ \st ->
          let index' = case x of
                           1 | st.index + pageSize >= csSize -> 0
                           1 -> st.index + pageSize
                           0 -> csSize - csSize `unzeroMod` pageSize
                           _ | st.index < pageSize -> 0
                           _ | otherwise           -> st.index - pageSize
          in st { index = index', pageSize = pageSize }
      Preview previewing ->
        modify_ \st ->
          if isJust st.toggled
          then st
          else st { previewing = previewing, variants = [0,0,0,0] }
      Vary slot i -> do
        sound SFXClick
        modify_ \st ->
            st { variants = fromMaybe st.variants $
                 Array.updateAt slot i st.variants }
      Team Add character -> do
        {team, toggled} <- get
        sound SFXClick
        case unit of
          _| character `elem` team     -> modify_ _{ toggled = Nothing }
          _| toggled /= Just character -> modify_ _{ toggled = Just character }
          _| length team < 3           -> modify_ _{ toggled = Nothing
                                                   , team = character : team
                                                   }
          _| otherwise                 -> modify_ _{ toggled = Nothing }
      Untoggle -> do
        modify_ _{ toggled = Nothing }
      Team Delete character -> do
        sound SFXCancel
        modify_ \st -> st { team = delete character st.team }
      Enqueue queueType -> do
        sound SFXApplySkill
        {team} <- get
        raise $ Queued queueType team
      ChooseAvatar avatar -> do
        sound SFXClick
        modify_ _{ avatar = Just avatar }
      TryUpdate -> do
        sound SFXTarget
        {avatar} <- get
        mform    <- liftEffect $ getForm settingsId
        let murl = do
              avatar'    <- avatar
              form       <- mform
              condense   <- form.lookup "condense"
              background <- form.lookup "background"
              name       <- form.lookup "name"
              pure $ "api/update/" <> name <> "/" <> condense
                     <> "/b" <> background <> "/"
                     <> Global.unsafeEncodeURIComponent avatar'
        case murl of
          Nothing ->
            modify_ _{ updateFail = Just "Incomplete form" }
          Just url -> do
            res <- liftAff $ Affjax.get ResponseFormat.string url
            case res.status of
                StatusCode 200 -> do
                    modify_ _{ updateFail = Nothing }
                    liftEffect reload
                StatusCode 500 -> modify_
                    _{ updateFail = Just "Username already taken" }
                StatusCode 400 -> modify_
                    _{ updateFail = Just "Name can only contain letters and numbers" }
                StatusCode code -> modify_
                    _{ updateFail = Just $ show code <> ": " <> res.statusText }
  eval (QueryPlay _ next) = pure next

failWarning :: ∀ a. Maybe String -> Array (HTMLQ a) -> Array (HTMLQ a)
failWarning Nothing        = identity
failWarning (Just warning) = flip snoc $ H.span [_i "userfail"] [H.text warning]

previewBox :: ∀ a. State -> Array (HTMLQ a) -> Array (HTMLQ a)
previewBox {previewing: NoPreview} = identity
previewBox p@{previewing: PreviewUser (User u)} = cons $
  H.article [_c "parchment"]
  [ H.form [_i settingsId]
    [ H.h1_ [ H.text "Account Settings" ]
    , H.p_ <<< failWarning p.updateFail $
      [ _span "Name"
      , H.input [P.type_ P.InputText, P.name "name", P.value u.name]
      ]
    , H.p_
      [ _span "Background"
      , H.input
        [ P.type_ P.InputText
        , P.name "background"
        , P.value $ fromMaybe "" u.background
        ]
      ]
    , H.p_
      [ H.input
        [ P.type_ P.InputCheckbox
        , P.name "condense"
        , P.checked condensed
        ]
      , _span
        "Show only the first version of each character in the selection grid"
      ]
    , H.p_ [_span "Avatars"]
    , H.section [_i "avatars"] $ avatars <#> \avatar' -> H.img
        if Just avatar' == p.avatar
        then [_src avatar', _c "noclick"]
        else [_src avatar', _c "click", click $ ChooseAvatar avatar']
    , H.div [_i "updateButton", _c "click", click TryUpdate] [ H.text "Update" ]
    , H.a [ P.href "auth/logout" ]
      [ H.div [_i "logoutButton", _c "click"] [ H.text "Log out" ] ]
    ]
  ]
previewBox p@{previewing: PreviewChar c'@(Character c)} = cons <<<
  H.article [_c "parchment"] $
    [ H.aside_ $
        case Map.lookup (Characters.shortName c') Characters.groupMap of
            Just (_:|[]) -> []
            Just cs  -> reverse $ Array.fromFoldable cs <#> \x ->
              H.img $
              if x `elem` p.team
              then
                [ _c $ (x == c' ? ("on " <> _)) "noclick disabled char"
                , Character.icon x "icon"
                , preview $ PreviewChar x
                ]
              else
                [ _c $ (x == c' ? ("on " <> _)) "click char"
                , Character.icon x "icon"
                , preview $ PreviewChar x
                , click $ Team Add x
                ]
            _ -> []
    , H.header_ $ H.img [_c "char", Character.icon c' "icon"]
      : Character.name c'
    , H.p_ [ H.text c.bio ]
    ]
  <> zip3 (previewSkill c') (0..3) c.skills p.variants

previewSkill :: ∀ a. Character -> Int -> Array Skill -> Int -> HTMLQ a
previewSkill character slot skills i = case skills !! i of
  Nothing -> H.section_ []
  Just (Skill {name, cost, charges, classes, desc, cooldown}) ->
      H.section_
      [ H.aside_ $ catMaybes
        [ Just $ H.img [_c "char", Character.icon character name]
        , vPrev <#> \v ->
            H.a [_c "prevSkill click", click $ Vary slot v][]
        , vNext <#> \v ->
            H.a [_c "nextSkill click", click <<< Vary slot $ v + i][]
        ]
      , H.header_ $ H.text name : Chakra.cost cost `snoc`
          H.div [_c "skillClasses"]
          [ H.text <<< String.joinWith ", " $ Class.filter false classes ]
      , H.p_ <<< (Skill.parseDesc desc <> _) <<< (snd <$> _) $ filter fst
          [ Tuple (charges > 1) <<< _extra $ show charges <> " charges."
          , Tuple (charges == 1) <<< _extra $ show charges <> " charge."
          , Tuple (cooldown > 0) <<< _extra $ "CD: " <> show cooldown
          ]
      ]
  where
    vPrev = do
        skill <- skills !! i
        Array.findLastIndex (not <<< Skill.match skill) $ take i skills
    vNext = do
        skill <- skills !! i
        Array.findIndex (not <<< Skill.match skill) $ drop i skills

userBox :: ∀ a. Boolean -> Array Character -> Array (HTMLQ a)
userBox showLogin team = case user of
  Just u'@(User u) ->
    [ H.nav [_c "playButtons"] $
      [ _a "mainsite" "playButton parchment click blacked" "/home" "Main Site"
      , H.a
        [_i "queue", _c playButton, click $ Enqueue Quick]
        [H.text "Start Quick Match"]
      , H.a
        [_i "practicequeue", _c playButton, click $ Enqueue Practice]
        [H.text "Start Practice Match"]
      , H.a
        [_i "private", _c playButton, click $ Enqueue Private]
        [H.text "Start Private Match"]
      ]
    , H.section [_i "teamContainer"]
      [ H.div [_c "parchment loggedin", preview $ PreviewUser u']
        [ H.img [_c "userimg char", _src u.avatar]
        , _b       u.name
        , H.br_
        , H.text $ User.rank u'
        , H.br_
        , _b       "Clan: "
        , H.text $ fromMaybe "Clanless" u.clan
        , H.br_
        , _b       "Level: "
        , H.text $ show (User.level u') <> " (" <> show (User.xp u') <> " XP)"
        , H.br_
        , _b       "Ladder Rank: "
        , H.text   "None"
        , H.br_
        , _b       "Record: "
        , H.text $ show u.wins <> " - " <> show (u.wins + u.losses)
                 <> " (+" <> show u.streak <> ")"
        ]
      , H.div [_i "teamButtons"] $ team <#> \c ->
          H.div [_c "char click"]
            [ H.img
              [ Character.icon c "icon"
              , preview $ PreviewChar c
              , click $ Team Delete c
              ]
            ]
      , H.div [_i "underTeam", _c "parchment"] []
      ]
    ]
  Nothing ->
    [ H.nav [_c "playButtons"]
      [_a "mainsite" "playButton parchment click blacked" "/home" "Main Site"]
    , H.section [_i "teamContainer"]
      [ H.div [_c "parchment"]
        [ H.form
          [ _i $ if showLogin then "loginForm" else "registerForm"
          , _c "userForm"
          , P.method P.POST, P.action $ "/auth/page/email/" <>
                             if showLogin then "login" else "register"
          ] <<< (snd <$> _) $ filter fst
            [ Tuple true $ H.input
                [ P.type_       P.InputHidden
                , P.name        "_token"
                , P.value       csrf
                ]
            , Tuple true $ H.div_
              [ H.input
                [ _c            "email"
                , P.name        "email"
                , P.type_       P.InputEmail
                , P.required    true
                , P.autofocus   true
                , P.placeholder "Email"
                ]
              ]
            , Tuple showLogin $ H.div_
              [ H.input
                [ _c            "password"
                , P.name        "password"
                , P.type_       P.InputPassword
                , P.required    true
                , P.placeholder "Password"
                ]
              ]
            , Tuple showLogin $ H.div [_c "controls"]
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
            , Tuple (not showLogin) $ H.div [_c "controls"]
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
      , H.div [_i "teamButtons"] $ team <#> \c ->
          H.div [_c "char click"]
            [ H.img
              [ Character.icon c "icon"
              , preview $ PreviewChar c
              , click $ Team Delete c
              ]
            ]
      , H.div [_i "underTeam", _c "parchment"] []
      ]
    ]
  where
    playButton
      | length team == 3 = "click playButton parchment"
      | otherwise        = "playButton parchment"
