-- | Actions that characters can use to affect
-- 'N.health', 'N.barrier', and 'N.defense'.
module Game.Engine.Combat
  ( build, adjustHealth, formula, attack
  ) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Enum.Set (EnumSet)

import qualified Class.Parity as Parity
import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Attack (Attack)
import qualified Game.Model.Attack as Attack
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Destructible (Destructible(Destructible))
import qualified Game.Model.Destructible as Destructible
import           Game.Model.Duration (Duration(..))
import           Game.Model.Effect (Amount(..), Effect(..))
import           Game.Model.ID (ID)
import qualified Game.Model.ID as ID
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as N
import qualified Game.Model.Skill as Skill
import           Game.Model.Trigger (Trigger(..))

build :: ∀ m. MonadPlay m => Duration -> Int -> [Effect] -> MaybeT m Destructible
build dur amount effects = do
    n <- P.nUser
    let amount' = amount + Effects.build n
    guard $ amount' > 0
    Context{skill, user} <- P.context
    return Destructible
        { user
        , skill
        , dur
        , amount = amount'
        , effects
        }

adjustHealth :: ∀ m. MonadPlay m => (Int -> Int) -> m ()
adjustHealth adjust = do
    Context{target, user, skill} <- P.context
    nTarget <- P.nTarget
    let nTarget' = Ninjas.adjustHealth adjust nTarget
    P.write target nTarget'
    case nTarget'.health `compare` nTarget.health of
        LT -> P.trigger target $ OnDamaged <$> toList skill.classes
        EQ -> return ()
        GT -> P.trigger user [OnHeal]

data DamageAbsorb = DamageAbsorb
    { broken    :: [Destructible]
    , overflow  :: Int
    , remaining :: [Destructible]
    }

getBrokenIds :: DamageAbsorb -> [ID]
getBrokenIds DamageAbsorb { broken, remaining } =
    toList $ getIds broken \\ getIds remaining
  where
    getIds :: [Destructible] -> HashSet ID
    getIds destructibles = setFromList $ ID.from <$> destructibles

absorbDamage :: Int -> [Destructible] -> DamageAbsorb
absorbDamage damage destructible = absorb $ DamageAbsorb [] damage destructible
  where
    absorb d@(DamageAbsorb _ _ []) = d
    absorb (DamageAbsorb broken dmg (x:xs))
      | x.amount > dmg = DamageAbsorb broken 0 (damaged : xs)
      | otherwise = absorb $ DamageAbsorb (damaged : broken) (dmg - x.amount) xs
      where
        damaged = x { Destructible.amount = max 0 $ x.amount - dmg }

userAdjust :: Attack -> EnumSet Class -> Ninja -> Float -> Float
userAdjust atk classes nUser x = x
    * max 0 (1 + strengthen Percent - weaken Percent)
    + strengthen Flat
    - weaken Flat
  where
    strengthen = Effects.strengthen classes nUser
    weaken     = Effects.weaken weakenClasses nUser
    weakenClasses
      | atk == Attack.Afflict = singleton Affliction
      | otherwise             = insertSet Affliction classes

targetAdjust :: Attack -> EnumSet Class -> Ninja -> Float -> Float
targetAdjust atk classes nTarget x = x
    * max 0 (1 + bleed Percent - reduce Percent)
    + bleed Flat
    - reduce Flat
  where
    bleed  = Effects.bleed classes nTarget
    reduce = Effects.reduce reduceClasses nTarget
    reduceClasses
      | atk == Attack.Damage = insertSet Affliction classes
      | otherwise            = singleton Affliction

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
attack atk dmg
  | dmg <= 0  = return ()
  | otherwise = void $ runMaybeT do
    nTarget <- P.nTarget
    guard . not $ nTarget `is` Invulnerable atkClass

    context@Context{target, user, skill} <- P.context
    nUser <- P.nUser
    let classes'    = insertSet atkClass skill.classes
        dmgCalc     = formula atk classes' nUser nTarget dmg
        fromBarrier = absorbDamage dmgCalc nUser.barrier
        fromDefense
          | nTarget `is` Undefend = DamageAbsorb [] fromBarrier.overflow
                                    nTarget.defense
          | otherwise             = absorbDamage fromBarrier.overflow
                                    nTarget.defense

    if atk > Attack.Afflict && nTarget `is` DamageToDefense then
        forM_ (N.effectSource DamageToDefense nTarget) \(defUser, defSkill) ->
            P.modify target $ Ninjas.addDefense context Destructible
                { user    = defUser
                , skill   = Skill.removeClass Nonstacking defSkill
                , amount  = dmgCalc
                , dur     = Permanent
                , effects = mempty
                }

    else if atk == Attack.Afflict then
        P.modify target $ Ninjas.adjustHealth (- dmgCalc)

    else do
        let setBarrier n = refreshEffects fromBarrier
                           n { N.barrier = fromBarrier.remaining }
            setDefense n = refreshEffects fromDefense
                           n { N.defense = fromDefense.remaining }
        P.modify user setBarrier
        if atk == Attack.Demolish || overflow fromDefense <= 0 then
            P.modify target setDefense
        else
            P.modify target
                $ Ninjas.adjustHealth (- fromDefense.overflow) . setDefense

    damaged <- (nTarget.health -) . (.health) <$> P.nTarget

    P.trigger user $ OnBreak <$> getBrokenIds fromBarrier
    P.trigger target $ OnBreak <$> getBrokenIds fromDefense

    guard $ damaged > 0 && not (Parity.allied user target)

    P.trigger user [OnDamage]
    P.trigger target $ OnDamaged <$> toList classes'
  where
    refreshEffects destructibles n
      | all (null . Destructible.effects) $ destructibles.broken = n
      | otherwise = Ninjas.processEffects n
    atkClass
      | atk == Attack.Afflict = Affliction
      | otherwise             = NonAffliction
