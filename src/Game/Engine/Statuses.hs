-- | 'Status' processing.
module Game.Engine.Statuses
    ( apply
    , triggerStatusApplied
    , makeStatus
    , StatusParams(..)
    ) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..), hoistMaybe)
import Data.Enum.Set (EnumSet)

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Duration (Duration(..))
import qualified Game.Model.Duration as Duration
import           Game.Model.Effect (Constructor(..), Effect(..))
import qualified Game.Model.Effect as Effect
import qualified Game.Model.ID as ID
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as N
import           Game.Model.Runnable (Runnable)
import qualified Game.Model.Skill as Skill
import           Game.Model.Status (Bomb(..), Status(Status))
import qualified Game.Model.Status as Status
import           Game.Model.Trigger (Trigger(..))
import           Util ((∈), (∉), intersects)

triggerStatusApplied :: ∀ m. MonadPlay m => [Effect] -> m ()
triggerStatusApplied effects = do
    Context{user, target} <- P.context
    when (any isInvulnerable effects)
        $ P.trigger target [OnInvulnerable]
    when (any isReduce effects)
        $ P.trigger user [OnReduce]
    when (any Effect.isDisable effects) do
        P.trigger user [OnStun]
        P.trigger target [OnStunned]
    when (any isHeal effects)
        $ P.trigger user [OnHeal]
  where
    isHeal (Heal x)   = x > 0
    isHeal _          = False
    isReduce Reduce{} = True
    isReduce _        = False
    isInvulnerable Invulnerable{} = True
    isInvulnerable _              = False

-- | Status engine.
-- Uses 'Ninjas.addStatus' internally.
apply :: ∀ m. MonadPlay m
          => Int -> EnumSet Class -> [Runnable Bomb] -> Duration -> Text
          -> [Effect] -> m ()
apply amount classes bombs unthrottled name effects = void $ runMaybeT do
    context@Context{new, target} <- P.context
    nUser   <- P.nUser
    nTarget <- P.nTarget
    dur     <- if not new || isChanneled then return unthrottled else
                hoistMaybe $ Duration.throttle
                (Effects.throttle effects nUser) unthrottled
    let st   = status context nUser nTarget dur
        stID = ID.from st
    if N.has stID nTarget && Extending ∈ st.classes then
        P.modify target $ Ninjas.prolong st.dur stID
    else do
        let Status{effects = efs} = st
        guard $ null effects || not (null efs)
        P.modify target $ Ninjas.addStatus st
        triggerStatusApplied efs
  where
    isChanneled = setFromList [Continues, Controlled] `intersects` classes
    status context nUser nTarget dur = makeStatus StatusParams
        { context
        , amount
        , nUser
        , nTarget
        , classes
        , bombs
        , name
        , dur
        , effects
        }

data StatusParams = StatusParams
    { context :: Context
    , amount  :: Int
    , nUser   :: Ninja
    , nTarget :: Ninja
    , classes :: EnumSet Class
    , bombs   :: [Runnable Bomb]
    , name    :: Text
    , dur     :: Duration
    , effects :: [Effect]
    }

makeStatus :: StatusParams -> Status
makeStatus StatusParams
    { context = Context{skill, user, continues, new, target}
    , amount
    , nUser
    , nTarget
    , classes
    , bombs
    , name
    , dur
    , effects
    } =
    (Status.new user dur skill)
    { Status.name    = statusName
    , Status.effects = filterDmg . filter disable
                     $ Ninjas.apply nUser nTarget effects
    , Status.classes = modClasses $ extra ++ classes ++ skill.classes
    , Status.amount  = amount
    , Status.bombs   = bombs
    }
  where
    statusName
      | not $ null name  = name
      | Hidden ∈ classes = toLower skill.name
      | otherwise        = skill.name
    modClasses
      | continues && dur <= 1 = insertSet Continues
      | continues || new      = deleteSet Continues
      | otherwise             = deleteSet Continues . deleteSet Invisible
    baseClasses = classes ++ skill.classes
    noremove    = null effects && Bane ∉ baseClasses
                  || Hidden ∈ baseClasses
                  || user == target && any (not . Effect.helpful) effects
    extra       = setFromList $ fst <$> filter snd
                  [ (Soulbound,   any bind effects)
                  , (Unremovable, noremove)
                  ]
    silenced = nUser `is` Silence
    disabled = Effects.disabled nUser
    disable x
      | Effect.isDisable x = not $ nUser `is` Disable Stuns
      | otherwise          = x ∉ disabled
    filterDmg xs
      | silenced  = filter isDmg xs
      | otherwise = xs
    bind Redirect{}   = True
    bind _            = False
    isDmg (Afflict x) = x > 0
    isDmg _           = False
