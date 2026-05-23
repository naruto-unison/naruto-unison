-- | Actions that characters can use to affect
-- 'N.health', 'N.barrier', and 'N.defense'.
module Game.Engine.Combat
  ( formula, attack
  ) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Enum.Set (EnumSet)

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
import           Game.Model.Effect (Amount(..), Effect(..))
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Game.Model.Trigger (Trigger(..))

-- | Reduces incoming damage by depleting the user's 'N.barrier'.
absorbBarrier :: Int -> [Destructible] -> (Int, [Destructible])
absorbBarrier hp [] = (hp, [])
absorbBarrier hp (x@Destructible{amount}:xs)
  | amount <= hp = absorbBarrier (hp - amount) xs
  | otherwise    = (0, x { Destructible.amount = amount - hp } : xs)

-- | Reduces incoming damage by depleting the target's 'N.defense'.
absorbDefense :: Int -> [Destructible] -> (Int, [Destructible])
absorbDefense hp [] = (hp, [])
absorbDefense hp (x@Destructible{amount}:xs)
  | amount <= hp = absorbDefense (hp - amount) xs
  | otherwise    = (0, x { Destructible.amount = amount - hp } : xs)

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
    reduceAfflic = Effects.reduce (singletonSet Affliction) nTarget
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

    channeled <- isChanneled <$> P.context
    guard . not $ channeled && nTarget `is` AntiChannel

    context@Context{target, user, skill = Skill{classes}} <- P.context
    nUser <- P.nUser
    let classes'            = insertSet atkClass classes
        dmgCalc             = formula atk classes' nUser nTarget dmg
        (dmg'Destructible, barr) = absorbBarrier dmgCalc $ N.barrier nUser
        handleDefense
          | nTarget `is` Undefend = (,)
          | otherwise             = absorbDefense
        (dmg'Def, defense) = handleDefense dmg'Destructible $ N.defense nTarget

    guard $ dmgCalc > Effects.threshold nTarget -- Always 0 or higher

    if atk > Attack.Afflict && nTarget `is` DamageToDefense then
        let damageDefense = Destructible.new context 0 dmgCalc
        in
        P.modify target \n -> n { N.defense = damageDefense : N.defense n }

    else if atk == Attack.Afflict then
        P.modify target $ Ninjas.adjustHealth (- dmgCalc)

    else do
        P.modify user \n -> n { N.barrier = barr }
        if atk == Attack.Demolish || dmg'Def <= 0 then
            P.modify target \n -> n { N.defense = defense }
        else
            P.modify target $ Ninjas.adjustHealth (- dmg'Def) . \n ->
                n { N.defense = defense }

    damaged <- (N.health nTarget -) . N.health <$> P.nTarget
    when (damaged > 0) do
        P.trigger user [OnDamage]
        P.trigger target $ OnDamaged <$> toList classes'
        P.modify target $ Traps.track PerDamaged damaged

  where
    isChanneled Context{continues, new} = continues && not new
    atkClass
      | atk == Attack.Afflict = Affliction
      | otherwise             = NonAffliction
