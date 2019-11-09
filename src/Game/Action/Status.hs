-- | Actions that characters can use to affect @Status@es.
module Game.Action.Status
  ( -- * Applying statuses
    apply, apply', applyWith, applyWith'
  , tag, tag'
  , flag, flag'
  , hide, hide'
  , addStack, addStack', addStacks, addStacks', applyStacks
    -- * Applying bombs
  , bomb, bomb', bombWith, bombWith'
  -- * Adjusting statuses
  , refresh, prolong, hasten
  -- * Removing statuses
  , cure, cureAll, cureBane, cureStun, purge, remove, removeStack, removeStacks
  -- * Specialized
  , makeRewind, commandeer
  ) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Enum.Set (EnumSet)

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Channel (Channeling(..))
import           Game.Model.Class (Class(..))
import          Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Duration (Duration(..), Turns, sync)
import qualified Game.Model.Duration as Duration
import           Game.Model.Effect (Constructor(..), Effect(..))
import qualified Game.Model.Effect as Effect
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Runnable (Runnable(To))
import qualified Game.Model.Skill as Skill
import           Game.Model.Status (Bomb(..), Status)
import qualified Game.Model.Status as Status
import           Game.Model.Trigger (Trigger(..))
import           Util ((∈), (∉))

-- | Refreshes the 'Status.dur' of 'Ninja.statuses' with matching 'Status.name'
-- to 'Status.maxDur'.
-- Uses 'Ninjas.refresh' internally.
refresh :: ∀ m. MonadPlay m => Text -> m ()
refresh name = P.unsilenced . P.fromUser $ Ninjas.refresh name

-- | Increases the 'Status.dur' of 'Ninja.statuses' with matching 'Status.name'.
-- Uses 'Ninjas.prolong' internally.
prolong :: ∀ m. MonadPlay m => Turns -> Text -> m ()
prolong (sync . Duration -> dur) name =
    P.unsilenced . P.fromUser $ Ninjas.prolong dur name

-- | Reduces the 'Status.dur' of 'Ninja.statuses' with matching 'Status.name'.
-- Uses 'Ninjas.prolong' internally.
hasten :: ∀ m. MonadPlay m => Turns -> Text -> m ()
hasten (sync . Duration -> dur) name =
    P.unsilenced . P.fromUser $ Ninjas.prolong (negate dur) name

-- | Adds a @Status@ to 'Ninja.statuses'.
apply :: ∀ m. MonadPlay m => Turns -> [Effect] -> m ()
apply = apply' ""
-- | 'apply' with a 'Status.name'.
apply' :: ∀ m. MonadPlay m => Text -> Turns -> [Effect] -> m ()
apply' = applyWith' mempty
-- | 'apply' with extra 'Status.classes'.
applyWith :: ∀ m. MonadPlay m
          => EnumSet Class -> Turns -> [Effect] -> m ()
applyWith classes = applyWith' classes ""
-- | 'applyWith' with a 'Status.name'.
applyWith' :: ∀ m. MonadPlay m
           => EnumSet Class -> Text -> Turns -> [Effect] -> m ()
applyWith' classes turns efs = P.unsilenced . applyFull 1 classes [] turns efs

-- | Adds a simple @Status@ with no 'Status.effects' or 'Status.dur'
-- 'Ninja.statuses'. Stacks are unremovable.
addStack :: ∀ m. MonadPlay m => m ()
addStack = do
    name <- Skill.name <$> P.skill
    addStacks' 0 name 1

-- | 'addStack' with a 'Status.name'.
addStack' :: ∀ m. MonadPlay m => Text -> m ()
addStack' name = addStacks' 0 name 1

-- | 'addStack' with a 'Status.name' and 'Status.amount'.
addStacks :: ∀ m. MonadPlay m => Text -> Int -> m ()
addStacks = addStacks' 0

-- | 'addStack' with a 'Status.dur', 'Status.name', and 'Status.amount'.
-- Uses 'Ninjas.addStatus' internally.
addStacks' :: ∀ m. MonadPlay m => Turns -> Text -> Int -> m ()
addStacks' (Duration -> dur) name i = do
    user   <- P.user
    st     <- Status.new user dur <$> P.skill
    target <- P.target
    P.modify target $ Ninjas.addStatus
        st { Status.name    = name
           , Status.amount  = i
           , Status.user    = user
           , Status.classes = Unremovable `insertSet` Status.classes st
           }

-- | Adds a hidden @Status@ with no effects that immediately expires.
flag :: ∀ m. MonadPlay m => m ()
flag = flag' . toLower . Skill.name =<< P.skill
-- | 'flag' with a 'Status.name'.
flag' :: ∀ m. MonadPlay m => Text -> m ()
flag' name =
    applyWith' (setFromList [Hidden, Unremovable, Nonstacking]) name (-1) []
-- | Applies a @Status@ with no effects, used as a marker for other
-- 'Skill.Skill's.
tag :: ∀ m. MonadPlay m => Turns -> m ()
tag = tag' ""
-- | 'tag' with a 'Status.name'.
tag' :: ∀ m. MonadPlay m => Text -> Turns -> m ()
tag' name dur = applyWith' (setFromList [Unremovable, Nonstacking]) name dur []

-- | Applies a 'Hidden' and 'Unremovable' @Status@.
hide :: ∀ m. MonadPlay m => Turns -> [Effect] -> m ()
hide dur efs = do
    skill <- P.skill
    hide' (toLower $ Skill.name skill) dur efs

-- | 'hide' with a 'Status.name'.
hide' :: ∀ m. MonadPlay m => Text -> Turns -> [Effect] -> m ()
hide' = applyWith' $ setFromList [Unremovable, Hidden]

-- | Adds a @Status@ with 'Status.bombs' to 'Ninja.statuses'.
-- @Bomb@s apply an effect when the @Status@ ends. If the @Bomb@ type is
-- 'Status.Expire', the bomb activates if the @Status@ naturally reaches the end
-- of its 'Status.dur'. If the @Bomb@ type is 'Status.Remove', the @Bomb@
-- activates if the @Status@ is removed before naturally reaching the end of its
-- 'Status.dur'. If the @Bomb@ type is 'Status.Done', the bomb activates in both
-- situations.
bomb :: ∀ m. MonadPlay m
     => Turns -> [Effect] -> [Runnable Bomb] -> m ()
bomb = bomb' ""
-- | @Bomb@ with a 'Status.name'.
bomb' :: ∀ m. MonadPlay m
      => Text -> Turns -> [Effect] -> [Runnable Bomb] -> m ()
bomb' = bombWith' mempty
-- | @Bomb@ with extra 'Status.classes'.
bombWith :: MonadPlay m
         => EnumSet Class -> Turns -> [Effect] -> [Runnable Bomb]
         -> m ()
bombWith classes = bombWith' classes ""
-- | 'bombWith' with a 'Status.name'.
bombWith' :: ∀ m. MonadPlay m
          => EnumSet Class -> Text -> Turns -> [Effect] -> [Runnable Bomb]
          -> m ()
bombWith' classes name dur effects bombs =
    P.unsilenced $ applyFull 1 classes bombs name dur effects

-- | 'addStacks' with 'Status.effect's.
applyStacks :: ∀ m. MonadPlay m
            => Text -> Int -> [Effect]
            -> m ()
applyStacks name amount =
    applyFull amount (setFromList [Unremovable]) mempty name 0

-- | Status engine.
-- Uses 'Ninjas.addStatus' internally.
applyFull :: ∀ m. MonadPlay m
          => Int -> EnumSet Class -> [Runnable Bomb] -> Text -> Turns
          -> [Effect] -> m ()
applyFull amount classes bombs name (Duration -> unthrottled) effects =
    void $ runMaybeT do
        context <- P.context
        user    <- P.user
        target  <- P.target
        nUser   <- P.nUser
        nTarget <- P.nTarget
        dur     <- if not $ Context.new context then return unthrottled else
                   MaybeT . return $ Duration.throttle
                   (Effects.throttle effects nUser) unthrottled
        let st   = makeStatus context amount nUser nTarget
                   classes bombs name dur effects
        if Ninja.has name user nTarget && Extending ∈ Status.classes st then
            let prolong' = mapMaybe .
                           Ninjas.prolong' (Status.dur st) name $ Status.user st
            in
            P.modify target \n ->
                n { Ninja.statuses = prolong' $ Ninja.statuses n }
        else do
            guard $ null effects || not (null $ Status.effects st)
            P.modify target $ Ninjas.addStatus st
            when (any isInvulnerable $ Status.effects st) $
                P.trigger target [OnInvulnerable]
            when (any isReduce $ Status.effects st) $
                P.trigger user [OnReduce]
            when (any Effect.isDisable $ Status.effects st) do
                P.trigger user [OnStun]
                P.trigger target [OnStunned]
            when (any isHeal $ Status.effects st) $ P.trigger user [OnHeal]
  where
    isHeal (Heal x) = x > 0
    isHeal _        = False
    isReduce Reduce{} = True
    isReduce _        = False
    isInvulnerable Invulnerable{} = True
    isInvulnerable _              = False

makeStatus :: Context -> Int -> Ninja -> Ninja
           -> EnumSet Class -> [Runnable Bomb] -> Text -> Duration
           -> [Effect] -> Status
makeStatus Context{skill, user, continues, new}
           amount nUser nTarget classes bombs name dur effects =
    (Status.new user dur skill)
    { Status.name    = Skill.defaultName name skill
    , Status.user
    , Status.effects = filterDmg . filter disable $ Ninjas.apply nTarget effects
    , Status.classes = classes'
    , Status.amount
    , Status.bombs
    }
  where
    skillClasses
      | continues && dur <= 1 = insertSet Continues $ Skill.classes skill
      | continues || new      = Skill.classes skill
      | otherwise             = deleteSet Invisible $ Skill.classes skill
    noremove = null effects && Bane ∉ skillClasses
               || dur == Duration 1 && Skill.dur skill /= Instant
               || user == Ninja.slot nTarget
                  && any (not . Effect.helpful) effects
    extra    = setFromList $ fst <$> filter snd
               [ (Soulbound,   any bind effects)
               , (Unremovable, noremove)
               ]
    classes' = extra ++ classes ++ skillClasses
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


-- | Removes non-'Effect.helpful' effects in 'Ninja.statuses' that match a
-- predicate.
-- Uses 'Ninjas.cure' internally.
cure :: ∀ m. MonadPlay m => (Effect -> Bool) -> m ()
cure match = P.unsilenced . P.toTarget $ Ninjas.cure match

-- | Removes all non-'Effect.helpful' 'effects in 'Ninja.statuses'.
-- Uses 'Ninjas.cure' internally.
cureAll :: ∀ m. MonadPlay m => m ()
cureAll = P.unsilenced . cure $ const True

-- | Removes all 'Ninja.statuses' with 'Bane' in their 'Status.classes'.
-- Uses 'Ninjas.cureBane' internally.
cureBane :: ∀ m. MonadPlay m => m ()
cureBane = P.unsilenced $ P.toTarget Ninjas.cureBane

-- | Cures all 'Stun' effects from 'Ninja.statuses'.
-- Uses 'Ninjas.cure' internally.
cureStun :: ∀ m. MonadPlay m => m ()
cureStun = P.unsilenced $ cure Effect.isDisable

-- | Cures all 'Effect.helpful' effects from 'Ninja.statuses'.
-- Uses 'Ninjas.purge' internally.
purge :: ∀ m. MonadPlay m => m ()
purge = P.toTarget Ninjas.purge

-- | Removes all @Status@es with matching 'Status.name' and whose 'Status.user'
-- is the one performing the action.
-- Uses 'Ninjas.clear' internally.
remove :: ∀ m. MonadPlay m => Text -> m ()
remove name = P.fromUser $ Ninjas.clear name

-- | Decreases the 'Status.amount' of a @Status@ with matching 'Status.name' by
-- 1, removing it if it reaches 0.
-- Uses 'Ninjas.removeStack' internally.
removeStack :: ∀ m. MonadPlay m => Text -> m ()
removeStack name = P.toTarget $ Ninjas.removeStack name

-- | Decreases the 'Status.amount' of a @Status@ with matching 'Status.name' and
-- whose 'Status.user is the one performing the action by some amount, removing
-- it if it reaches 0.
-- Uses 'Ninjas.removeStacks' internally.
removeStacks :: ∀ m. MonadPlay m => Text -> Int -> m ()
removeStacks name i = P.fromUser $ Ninjas.removeStacks name i

-- | In some number of turns from now, restores the target to their state at the
-- current moment.
makeRewind :: ∀ m. MonadPlay m => m (Bomb -> Runnable Bomb)
makeRewind = rewind <$> P.nTarget
  where
    rewind n b   = To b $ P.toTarget $ replace n
    replace n n' = n { Ninja.charges = Ninja.charges n' }

-- | Steals all of the target's 'Effect.helpful' 'Effect's.
commandeer :: ∀ m. MonadPlay m => m ()
commandeer = P.unsilenced do
    nUser   <- P.nUser
    nTarget <- P.nTarget
    user    <- P.user
    P.modify user $ Ninjas.modifyStatuses
        (mapMaybe gainHelpful (Ninja.statuses nTarget) ++) . \n ->
        n { Ninja.defense  = Ninja.defense nTarget ++ Ninja.defense n
          , Ninja.barrier  = []
          }
    target  <- P.target
    P.modify target $ Ninjas.modifyStatuses (mapMaybe loseHelpful) . \n ->
        n { Ninja.defense  = []
          , Ninja.barrier  = Ninja.barrier nUser
         }
  where
    lose ef = Effect.helpful ef && not (Effect.sticky ef)
    loseHelpful st
      | Unremovable ∈ Status.classes st    = Just st
      | null $ Status.effects st           = Just st
      | not . any lose $ Status.effects st = Just st
      | all lose $ Status.effects st       = Nothing
      | otherwise = Just st
          { Status.effects = filter (not . lose) $ Status.effects st }
    gainHelpful st
      | Unremovable ∈ Status.classes st    = Nothing
      | null $ Status.effects st           = Nothing
      | not . any lose $ Status.effects st = Nothing
      | all lose $ Status.effects st       = Just st
      | otherwise = Just
          st { Status.effects = filter lose $ Status.effects st }
