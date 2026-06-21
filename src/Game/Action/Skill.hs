-- | Actions that characters can use to affect @Skill@s.
module Game.Action.Skill
  ( -- * Cooldowns and charges
    alterCooldown
  , resetCooldown, resetCooldowns, recharge, rechargeAll
  -- * Copying
  , copyAll, copyLast, teach
  -- * Alternates
  , getAlternates, nextAlternate

  -- * Other
  , factory, replaceWith
  ) where

import ClassyPrelude

import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Enum.Set (EnumSet)
import qualified Data.Vector as Vector

import           Class.Classed (Classed(..))
import           Class.Play (MonadPlay)
import qualified Class.Play as P
import           Game.Engine (unSoulbound)
import qualified Game.Engine.Cooldown as Cooldown
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Duration (Duration(..))
import           Game.Model.Effect (Effect(..))
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import qualified Game.Model.Status as Status
import           Game.Model.Trigger (Trigger(..))
import           Util ((!?), (∉), (∈))

-- | Changes the 'Skill.cooldown' of a @Skill@ by 'Skill.name'.
-- Uses 'Cooldown.alter' internally.
alterCooldown :: ∀ m. MonadPlay m => Text -> Int -> m ()
alterCooldown name cd = P.toUserFromUser (Cooldown.alter cd) name

-- | Resets 'N.cooldowns' with a matching 'Skill.name' of a @Ninja@.
-- Uses 'Cooldown.reset' internally.
resetCooldown :: ∀ m. MonadPlay m => Text -> m ()
resetCooldown = P.toUserFromUser Cooldown.reset

-- | Resets all Instant 'N.cooldowns' of a @Ninja@.
-- Uses 'Cooldown.resetAll' internally.
resetCooldowns :: ∀ m. MonadPlay m => m ()
resetCooldowns = P.toTarget Cooldown.resetAll

-- | Resets an element in 'N.charges' of a @Ninja@.
-- Uses 'Ninjas.recharge' internally.
recharge :: ∀ m. MonadPlay m => Text -> m ()
recharge = P.toUserFromUser Ninjas.recharge

-- | Resets all 'N.charges' of a @Ninja@.
-- Uses 'Ninjas.rechargeAll' internally.
rechargeAll :: ∀ m. MonadPlay m => m ()
rechargeAll = P.toTarget Ninjas.rechargeAll

alternateClasses :: EnumSet Class
alternateClasses = setFromList [Nonstacking, Unremovable]

userSkills :: ∀ m. MonadPlay m => m (NonNull Vector (NonNull Vector Skill))
userSkills = getSkills <$> P.nUser
  where
    getSkills Ninja{character = Character{skills}} = skills

-- | Adjusts all 'N.alternates' at once.
getAlternates :: ∀ m. MonadPlay m
          => [Int] -- ^ Index offsets.
          -> m [Effect] -- ^ Recalculates every alternate of a target @Ninja@.
getAlternates loadout = do
    skills <- userSkills
    return $ catMaybes $ zipWith load loadout $ toList skills
  where
    load alt (x:|xs) = Alternate x.name . Skill.name <$> xs !? (alt - 1)

-- | Cycles a skill through its list of alternates.
-- | Uses 'Ninjas.nextAlternate' internally.
nextAlternate :: ∀ m. MonadPlay m => Text -> m ()
nextAlternate name = do
    Context{user, skill} <- P.context
    let name' = Skill.provideName skill name
    nUser <- P.nUser
    case Ninjas.nextAlternate name' nUser of
        Nothing  -> return ()
        Just alt -> P.modify user . Ninjas.addStatus
                  $ Status.addClasses alternateClasses
                        (Status.new user 1 skill)
                        { Status.effects = [Alternate name' alt] }

-- | Copies all @Skill@s from the target into the user's 'N.copies'.
-- Uses 'Ninjas.copyAll' internally.
copyAll :: ∀ m. MonadPlay m => Duration -> m ()
copyAll dur = P.uncopied do
    Context{user} <- P.context
    nTarget <- P.nTarget
    P.modify user $ Ninjas.copyAll dur nTarget

-- | Copies the 'N.lastSkill' of the target into a specific skill slot
-- of the user's 'N.copies'. Uses 'Execute.copy' internally.
copyLast :: ∀ m. MonadPlay m => Duration -> m ()
copyLast (succ -> dur) = P.uncopied . void $ runMaybeT do
    Context{skill = Skill{name}, user} <- P.context
    Just skill <- N.lastSkill <$> P.nTarget
    Just s     <- Vector.findIndex (any $ (== name) . Skill.name)
                . toNullable <$> userSkills
    P.modify user $ Ninjas.copy dur [s] skill

teach :: ∀ m. MonadPlay m
       => Duration -- ^ 'Copy.dur'.
       -> Text
       -> [Int]
       -> m ()
teach dur name slots = do
    Context{target} <- P.context
    mskill <- find ((== name) . Skill.name) . join <$> userSkills
    mapM_ (P.modify target . Ninjas.copy dur slots) mskill

-- | Resets a 'N.Ninja' to their initial state.
-- Uses 'Ninjas.factory' internally.
factory :: ∀ m. MonadPlay m => m ()
factory = do
    Context{target, user} <- P.context
    alive <- N.alive <$> P.nTarget
    P.modify target Ninjas.factory
    P.modifyAll $ unSoulbound target
    alive' <- N.alive <$> P.nTarget
    when (alive' && not alive) do
        P.trigger user [OnHeal]
        P.trigger target [OnResurrected]

-- | Restores a target to an earlier state. Charges are preserved.
replaceWith :: ∀ m. MonadPlay m => Ninja -> m ()
replaceWith n = P.toTarget \n' ->
    n { N.defense  = replace N.defense  n n' }
      { N.barrier  = replace N.barrier  n n' }
      { N.statuses = replace N.statuses n n' }
      { N.traps    = replace N.traps    n n' }
  where
    replace :: ∀ a. Classed a => (Ninja -> [a]) -> Ninja -> Ninja -> [a]
    replace getter old current =
        filter ((Atemporal ∈) . getClasses) (getter current)
        ++ filter ((Atemporal ∉) . getClasses) (getter old)
