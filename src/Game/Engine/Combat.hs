-- | Actions that characters can use to affect
-- 'N.health', 'N.barrier', and 'N.defense'.
module Game.Engine.Combat
  ( build, adjustHealth, formula, attack
  ) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Enum.Set (EnumSet)

import qualified Class.Labeled as Labeled
import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Traps as Traps
import           Game.Model.Attack (Attack)
import qualified Game.Model.Attack as Attack
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Destructible (Destructible(Destructible))
import qualified Game.Model.Destructible as Destructible
import           Game.Model.Duration (Duration(..))
import           Game.Model.Effect (Amount(..), Effect(..))
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Game.Model.Trigger (Trigger(..))

build :: ∀ m. MonadPlay m => Duration -> Int -> [Effect] -> m Destructible
build dur amount effects = create <$> P.context <*> P.nUser
  where
    create Context{skill, user} n = Destructible
        { user
        , skill
        , dur
        , amount = amount + Effects.build n
        , effects
        }

adjustHealth :: ∀ m. MonadPlay m => (Int -> Int) -> m ()
adjustHealth adjust = do
    Context{target, user, skill = Skill{classes}} <- P.context
    health <- N.health <$> P.nTarget
    P.modify target $ Ninjas.adjustHealth adjust
    health' <- N.health <$> P.nTarget
    case health' `compare` health of
        LT -> P.trigger target $ OnDamaged <$> toList classes
        EQ -> return ()
        GT -> P.trigger user [OnHeal]

data DamageAbsorb = DamageAbsorb
    { broken    :: [Destructible]
    , overflow  :: Int
    , remaining :: [Destructible]
    }

absorbDamage :: Int -> [Destructible] -> DamageAbsorb
absorbDamage damage destructible = absorb $ DamageAbsorb [] damage destructible
  where
    absorb d@(DamageAbsorb _ _ []) = d
    absorb (DamageAbsorb broken dmg (x@Destructible{amount}:xs))
      | amount > dmg = DamageAbsorb broken 0 (damaged:xs)
      | otherwise    = absorb $ DamageAbsorb (damaged:broken) (dmg - amount) xs
      where damaged = x { Destructible.amount = max 0 $ amount - dmg }

userAdjust :: Attack -> EnumSet Class -> Ninja -> Float -> Float
userAdjust atk classes nUser x = x
    * max 0 (1 + strengthen Percent - weaken Percent)
    + strengthen Flat
    - weaken Flat
  where
    strengthen = Effects.strengthen classes nUser
    weaken
      | atk == Attack.Afflict = const 0
      | otherwise             = Effects.weaken classes nUser

targetAdjust :: Attack -> EnumSet Class -> Ninja -> Float -> Float
targetAdjust atk classes nTarget x = x
    * max 0 (1 + bleed Percent - reduceAfflic Percent - reduce Percent)
    + bleed Flat
    - reduceAfflic Flat
    - reduce Flat
  where
    bleed        = Effects.bleed classes nTarget
    reduceAfflic = Effects.reduce (singleton Affliction) nTarget
    reduce amt
      | atk == Attack.Damage = Effects.reduce classes nTarget amt
      | otherwise            = 0

-- | Damage formula.
formula :: Attack -- ^ Attack type.
        -> EnumSet Class -- ^ 'Skill.classes'.
        -> Ninja -- ^ User.
        -> Ninja -- ^ Target.
        -> Int -- ^ Base damage.
        -> Int
formula atk classes nUser nTarget = limit . round
    . targetAdjust atk' classes nTarget
    . userAdjust atk' classes nUser
    . fromIntegral
  where
    atk'
      | atk == Attack.Damage && nUser `is` Pierce = Attack.Pierce
      | otherwise = atk
    limit i
      | atk == Attack.Afflict = i
      | otherwise = case Effects.limit nTarget of
        Just x  -> min x i
        Nothing -> i

-- | Internal combat engine. Performs an 'Attack.Afflict', 'Attack.Pierce',
-- 'Attack.Damage', or 'Attack.Demolish' attack.
-- Uses 'Ninjas.adjustHealth' internally.
attack :: ∀ m. MonadPlay m => Attack -> Int -> m ()
attack atk dmg = void $ runMaybeT do
    nTarget <- P.nTarget
    guard . not $ nTarget `is` Invulnerable atkClass

    Context{target, user, skill = skill@Skill{classes}} <- P.context
    nUser <- P.nUser
    let classes'    = insertSet atkClass classes
        dmgCalc     = formula atk classes' nUser nTarget dmg
        fromBarrier = absorbDamage dmgCalc $ N.barrier nUser
        fromDefense
          | nTarget `is` Undefend = DamageAbsorb [] (overflow fromBarrier)
                                  $ N.defense nTarget
          | otherwise             = absorbDamage (overflow fromBarrier)
                                  $ N.defense nTarget

    guard $ dmgCalc > Effects.threshold nTarget -- Always 0 or higher

    if atk > Attack.Afflict && nTarget `is` DamageToDefense then
        let damageDefense = Destructible { user
                                         , skill
                                         , amount  = dmgCalc
                                         , dur     = Permanent
                                         , effects = []
                                         }
        in
        P.modify target \n -> n { N.defense = damageDefense : N.defense n }

    else if atk == Attack.Afflict then
        P.modify target $ Ninjas.adjustHealth (- dmgCalc)

    else do
        let setBarrier n = refreshEffects fromBarrier
                           n { N.barrier = remaining fromBarrier }
            setDefense n = refreshEffects fromDefense
                           n { N.defense = remaining fromDefense }
        P.modify user setBarrier
        if atk == Attack.Demolish || overflow fromDefense <= 0 then
            P.modify target setDefense
        else
            P.modify target
                $ Ninjas.adjustHealth (- overflow fromDefense) . setDefense

    damaged <- (N.health nTarget -) . N.health <$> P.nTarget

    P.trigger user $ OnBreak . Labeled.name <$> broken fromBarrier
    P.trigger target $ OnBreak . Labeled.name <$> broken fromDefense

    when (damaged > 0) do
        P.trigger user [OnDamage]
        P.trigger target $ OnDamaged <$> toList classes'
        P.modify target $ Traps.track PerDamaged damaged

  where
    refreshEffects destructibles n
      | all (null . Destructible.effects) $ broken destructibles = n
      | otherwise = Ninjas.processEffects n
    atkClass
      | atk == Attack.Afflict = Affliction
      | otherwise             = NonAffliction
