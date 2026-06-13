-- | 'Status' processing.
module Game.Engine.Statuses (apply) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..), hoistMaybe)
import Data.Enum.Set (EnumSet)

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Channel (Channeling(..))
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

-- | Status engine.
-- Uses 'Ninjas.addStatus' internally.
apply :: ∀ m. MonadPlay m
          => Int -> EnumSet Class -> [Runnable Bomb] -> Text -> Duration
          -> [Effect] -> m ()
apply amount classes bombs name unthrottled effects = void $ runMaybeT do
    context@Context{new, target, user} <- P.context
    nUser   <- P.nUser
    nTarget <- P.nTarget
    dur     <- if not new || isChanneled then return unthrottled else
                hoistMaybe $ Duration.throttle
                (Effects.throttle effects nUser) unthrottled
    let st   = makeStatus context amount nUser nTarget
               classes bombs name dur effects
        stID = ID.from st
    if N.has stID nTarget && Extending ∈ Status.classes st then
        P.modify target $ Ninjas.prolong (Status.dur st) stID
    else do
        let Status{effects = efs} = st
        guard $ null effects || not (null efs)
        P.modify target $ Ninjas.addStatus st
        when (any isInvulnerable efs)
            $ P.trigger target [OnInvulnerable]
        when (any isReduce efs)
            $ P.trigger user [OnReduce]
        when (any Effect.isDisable efs) do
            P.trigger user [OnStun]
            P.trigger target [OnStunned]
        when (any isHeal efs)
            $ P.trigger user [OnHeal]
  where
    isChanneled = setFromList [Continues, Controlled] `intersects` classes
    isHeal (Heal x)   = x > 0
    isHeal _          = False
    isReduce Reduce{} = True
    isReduce _        = False
    isInvulnerable Invulnerable{} = True
    isInvulnerable _              = False

makeStatus :: Context -> Int -> Ninja -> Ninja
           -> EnumSet Class -> [Runnable Bomb] -> Text -> Duration
           -> [Effect] -> Status
makeStatus Context{skill, user, continues, new, target}
           amount nUser nTarget classes bombs name dur effects =
    (Status.new user dur skill)
    { Status.name    = statusName
    , Status.user
    , Status.effects = filterDmg . filter disable
                     $ Ninjas.apply nUser nTarget effects
    , Status.classes = modClasses $ extra ++ classes ++ Skill.classes skill
    , Status.amount
    , Status.bombs
    }
  where
    statusName
      | not $ null name  = name
      | Hidden ∈ classes = toLower $ Skill.name skill
      | otherwise        = Skill.name skill
    modClasses
      | continues && dur <= 1 = insertSet Continues
      | continues || new      = deleteSet Continues
      | otherwise             = deleteSet Continues . deleteSet Invisible
    baseClasses = classes ++ Skill.classes skill
    noremove    = null effects && Bane ∉ baseClasses
                  || Hidden ∈ baseClasses
                  || dur == 1 && Skill.dur skill /= Instant
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
