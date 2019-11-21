module Game.Model.EffectSpec (spec) where

import ClassyPrelude

import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Enum.Set (EnumSet)
import Data.Maybe (fromJust)
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import           Class.Hook (MonadHook)
import qualified Class.Parity as Parity
import           Class.Play (MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Game.Action as Action
import           Game.Action.Combat (heal, setHealth)
import qualified Game.Action.Combat as Combat
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Skills as Skills
import           Game.Action.Status (addStack', apply, apply', cureAll)
import           Game.Action.Trap (trap)
import           Game.Characters.Import (self)
import           Game.Model.Act (Act(Act))
import qualified Game.Model.Act
import           Game.Model.Attack (Attack)
import qualified Game.Model.Attack as Attack
import           Game.Model.Chakra (Chakras)
import qualified Game.Model.Chakra as Chakra
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context)
import qualified Game.Model.Context as Context
import           Game.Model.Duration (Duration(..))
import           Game.Model.Effect (Amount(..), Constructor(..), Effect(..))
import qualified Game.Model.Game as Game
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Runnable (Runnable(To))
import           Game.Model.Skill (Skill, Target(..))
import qualified Game.Model.Skill as Skill
import qualified Game.Model.Slot as Slot
import           Game.Model.Trigger (Trigger(..))
import           Handler.Play.Wrapper (Wrapper)

import qualified Blank
import           OrphanInstances ()
import           Sim (as, at)
import qualified Sim as Sim

chunk :: ∀ a. (Int -> a) -> Int -> Int -> [a]
chunk _ _ 0 = []
chunk producer sizeSeed amount = producer r : replicate size (producer q)
  where
    size   = 1 + rem (abs sizeSeed) amount
    (q, r) = amount `quotRem` size

spec :: Spec
spec = parallel do
    describe "Absorb" do
        it "gains chakra from enemy skills" $
            tryAbsorb Enemy 1 `shouldBe` 1
        it "does not gain chakra from friendly skills" $
            tryAbsorb Ally 1 `shouldBe` 0

    describe "Afflict" do
        prop "damages every turn" \amount (Positive turns) -> simAt Enemy do
            apply Permanent [Afflict amount]
            Sim.turns $ turns - 1
            targetHealth <- Ninja.health <$> P.nTarget
            return $ 100 - targetHealth === healthBound (amount * turns)

    describe "Alone" do
        let tryTarget = apply Permanent [Alone] >> canTarget

        it "blocks allies"    . not $ simAt Ally  tryTarget
        it "does not block enemies" $ simAt Enemy tryTarget
        it "does not block self"    $ simAt Self  tryTarget

    -- describe "Alternate" (redundant)
    -- describe "AntiChannel" (redundant, more or less)

    describe "AntiCounter" do
        it "ignores counters and reflects" $ simAt Enemy do
            apply Permanent [Reflect, ReflectAll All]
            trap Permanent (Counter All) $ return ()
            trap Permanent (CounterAll All) $ return ()
            self do
                trap Permanent (Countered All) $ return ()
                apply Permanent [AntiCounter]
            canTarget

    describe "Bleed" do
        prop "is additive"        $ isAdditive Bleed
        prop "complements Reduce" $ Bleed `complements` Reduce

    describe "Bless" do
        prop "adds to healing" \bless hp -> simEffects [Bless bless] [] Ally do
            setHealth 1
            heal hp
            targetHealth <- Ninja.health <$> P.nTarget
            return $ targetHealth === healthBound (1 + hp + bless)

    describe "Block" do
        let tryTarget t = do apply Permanent [Block $ Sim.targetSlot t]
                             at XEnemies $ canTargetAs Enemy

        it "blocks vs. subject"  . not . simAt Enemy $ tryTarget XEnemies
        it "does not block vs. others" $ simAt Enemy $ tryTarget REnemy

    describe "BlockAllies" do
        let tryTarget = self (apply Permanent [BlockAllies]) >> canTarget

        it "blocks vs. allies"    . not $ simAt Ally  tryTarget
        it "does not block vs. enemies" $ simAt Enemy tryTarget
        it "does not block vs. self"    $ simAt Self  tryTarget

    describe "BlockEnemies" do
        let tryTarget = self (apply Permanent [BlockEnemies]) >> canTarget

        it "blocks vs. enemies"  . not $ simAt Enemy tryTarget
        it "does not block vs. allies" $ simAt Ally  tryTarget

    describe "Boost" do
        let boostAmount = 4
            damage      = 70
            reduce      = 3

        it "boosts effects from allies" $ simAt Self do
            as Ally $ apply Permanent [Reduce (singletonSet All) Flat reduce]
            self $ apply Permanent [Boost boostAmount]
            as Enemy $ Combat.damage damage
            userHealth <- Ninja.health <$> P.nUser
            return $ damage - (100 - userHealth) `shouldBe` boostAmount * reduce

        it "does not boost own effects" $ simAt Self do
            apply Permanent [Reduce (singletonSet All) Flat reduce, Boost boostAmount]
            as Enemy $ Combat.damage damage
            userHealth <- Ninja.health <$> P.nUser
            return $ damage - (100 - userHealth) `shouldBe` reduce

    describe "Build" do
        prop "adds to barrier" \build hp -> simEffects [Build build] [] Ally do
            Combat.barricade Permanent hp
            if build + hp >= 0 then do
                targetBarrier <- Ninja.totalBarrier <$> P.nTarget
                return $ targetBarrier === build + hp
            else do
                targetDefense <- Ninja.totalDefense <$> P.nTarget
                return $ targetDefense === negate (build + hp)

        prop "adds to defense" \build hp -> simEffects [Build build] [] Ally do
            Combat.defend Permanent hp
            if build + hp >= 0 then do
                targetDefense <- Ninja.totalDefense <$> P.nTarget
                return $ targetDefense === build + hp
            else do
                targetBarrier <- Ninja.totalBarrier <$> P.nTarget
                return $ targetBarrier === negate (build + hp)

    describe "Bypass" do
        it "makes all skills bypass" $ simAt Enemy do
            self $ apply Permanent [Bypass]
            apply Permanent [Invulnerable All]
            canTarget

    describe "DamageToDefense" do
        prop "absorbs damage"        damageToDefense
        prop "converts into defense" damageFromDefense

    describe "Disable" do
        it "stuns stuns" $ simAt Enemy do
            apply Permanent [Disable Stuns]
            as Enemy $ apply Permanent [Stun All]
            userStunned <- Effects.stun <$> P.nUser
            return $ userStunned `shouldBe` mempty

        it "stuns counters" $ simAt Enemy do
            apply Permanent [Disable Counters]
            as Enemy do
                self do
                    trap Permanent (Counter All) $ return ()
                    trap Permanent (CounterAll All) $ return ()
                trap Permanent (Countered All) $ return ()
            canTarget

        it "stuns others" $ simAt Enemy do
            apply Permanent [Disable $ Any ReflectAll]
            as Enemy $ self $ apply Permanent $ ReflectAll <$> [minBound..maxBound]
            canTarget

    describe "Duel" do
        let tryTarget t = do as XAlly $ apply Permanent [Duel $ Sim.targetSlot t]
                             canTarget

        it "invulnerable to enemies" . not . simAt Enemy $ tryTarget XEnemies
        it "invulnerable to allies"  . not . simAt Ally  $ tryTarget XEnemies
        it "not invulnerable to subject"   . simAt Ally  $ tryTarget Self

    describe "Endure" do
        prop "constraints health" constrainsHealth

    describe "Enrage" do
        let tryApply effect = do apply' "1" Permanent [effect]
                                 apply' "2" Permanent [Enrage]
                                 apply' "3" Permanent [effect]
                                 (`is` effect) <$> P.nTarget

        it "ignores negative effects" . not  . simAt Ally $ tryApply Plague
        it "does not ignore helpful effects" . simAt Ally $ tryApply Focus
        it "does not ignore self-applied"    . simAt Self $ tryApply Plague

    describe "Exhaust" do
        prop "increases skill costs" \(Positive exhaust) ->
            let effects = replicate exhaust . Exhaust $ singletonSet All in
            Skill.cost (getSkill effects) === 0 { Chakra.rand = exhaust }

    describe "Expose" do
        it "prevents target from becoming invulnerable" $ simAt Enemy do
            apply Permanent [Expose]
            as Enemy $ self $ apply Permanent [Invulnerable All]
            canTarget

        it "prevents target from reducing damage" $ simAt Enemy do
            apply Permanent [Reduce (singletonSet All) Flat 100, Expose]
            Combat.damage 1
            targetHealth <- Ninja.health <$> P.nTarget
            return $ 100 - targetHealth `shouldBe` 1

    -- describe "Face" (nothing to test)

    describe "Focus" do
        it "ignores stuns" $ simAt Enemy do
            self $ apply Permanent [Focus, Disable $ Only Reveal, Silence, Stun All]
            apply Permanent [Reveal]
            (`is` Reveal) <$> P.nTarget

    describe "Heal" do
        prop "heals every turn" \amount (Positive turns) -> simAt Enemy do
            setHealth 1
            apply Permanent [Heal amount]
            Sim.turns $ turns - 1
            targetHealth <- Ninja.health <$> P.nTarget
            return $ targetHealth === healthBound (1 + amount * turns)

    describe "Invulnerable" do
        let ignore atk dmg = do apply Permanent [Invulnerable atk]
                                dmg 50
                                targetHealth <- Ninja.health <$> P.nTarget
                                return $ targetHealth `shouldBe` 100

        it "ignores damage"   . simAt Enemy $ ignore NonAffliction Combat.damage
        it "ignores piercing" . simAt Enemy $ ignore NonAffliction Combat.pierce
        it "ignores affliction" . simAt Enemy $ ignore Affliction Combat.afflict

    describe "Limit" do
        prop "limits damage" isLimited

    describe "NoIgnore" do
        it "ignores ignores" $ simAt Enemy do
            apply Permanent [Focus, NoIgnore, Stun All]
            targetStunned <- Effects.stun <$> P.nTarget
            return $ targetStunned `shouldBe` singletonSet All

    describe "Nullify" do
        it "nullifies harm"  . not $ simEffects [] [Nullify] Enemy canTarget
        it "does not nullify help" $ simEffects [] [Nullify] Ally  canTarget

    describe "Pierce" do
        it "ignores damage reduction" $ simAt Enemy do
            self $ apply Permanent [Pierce]
            apply Permanent [Reduce (singletonSet All) Flat 100]
            Combat.damage 1
            targetHealth <- Ninja.health <$> P.nTarget
            return $ 100 - targetHealth `shouldBe` 1

    describe "Plague" do
        it "blocks healing" $ simAt Enemy do
            setHealth 1
            apply Permanent [Plague, Heal 100]
            heal 100
            targetHealth <- Ninja.health <$> P.nTarget
            return $ targetHealth `shouldBe` 1

        it "blocks curing" $ simAt Enemy do
            apply Permanent [Plague]
            cureAll
            (`is` Plague) <$> P.nTarget

    describe "Reduce" do
        prop "is additive"       $ isAdditive Reduce
        prop "complements Bleed" $ Reduce `complements` Bleed

    describe "Redirect" do
        let harmed = harmedWith . Redirect $ Sim.targetSlot Enemy

        it "reflects attacks from user" . not $ harmed P.nUser
        it "reflects attacks to target"       $ harmed P.nTarget

    describe "Reflect" do
        it "reflects from target" . not $ harmedWith Reflect P.nUser
        it "reflects to user"           $ harmedWith Reflect P.nTarget

    describe "ReflectAll" do
        it "reflects from target" . not $ harmedWith (ReflectAll All) P.nUser
        it "reflects to user"           $ harmedWith (ReflectAll All) P.nTarget

    describe "Restrict" do
        it "restricts multi-target to single-target" $
            Skill.targets (getSkill [Restrict])
            `shouldBe` setFromList [minBound..maxBound]
            `difference` setFromList [Enemies, XEnemies, Everyone]

    -- describe "Reveal" (nothing to test)

    describe "Seal" do
        let tryApply effect = do apply' "1" Permanent [effect]
                                 apply' "2" Permanent [Seal]
                                 apply' "3" Permanent [effect]
                                 (`is` effect) <$> P.nTarget
        it "ignores helpful effects"   . not . simAt Self $ tryApply Focus
        it "does not ignore harmful effects" . simAt Self $ tryApply Reveal

    describe "Share" do
        let harms target = do apply Permanent [Share $ Sim.targetSlot Enemy]
                              as target $ apply Permanent [Reveal]
                              (`is` Reveal) <$> Sim.get Enemy

        it "shares harm"                . simAt Ally $ harms XEnemies
        it "does not share other" . not . simAt Ally $ harms XAlly

    describe "Silence" do
        it "blocks non-damage" $ simAt Enemy do
            self $ apply Permanent [Silence]
            Combat.damage 1
            Combat.heal 100
            targetHealth <- Ninja.health <$> P.nTarget
            return $ 100 - targetHealth `shouldBe` 1

    describe "Snare" do
        prop "increases cooldowns" \cd snare ->
            let n  = ninjaWithCooldown cd
                n' = n { Ninja.effects = [Snare snare] }
            in
            simCooldown n' === max 0 (simCooldown n + 2 * snare)

    describe "Strengthen" do
        prop "is additive"        $ isAdditive Strengthen
        prop "complements Weaken" $ Strengthen `complements` Weaken

    -- describe "Stun" (redundant)

    describe "Swap" do
        it "swaps allies and enemies as targets" $
            Skill.targets (getSkill [Swap])
            `shouldBe` Skill.targets (Skills.swap $ getSkill [])

    describe "Taunt" do
        let tryTarget = do self $ apply Permanent [Taunt $ Sim.targetSlot Enemy]
                           canTarget

        it "does not block against subject" $ simAt Enemy    tryTarget
        it "does not block against self"    $ simAt Self     tryTarget
        it "blocks against others"    . not $ simAt XEnemies tryTarget

    describe "Threshold" do
        prop "constraints damage" thresholdConstrains

    describe "Throttle" do
        it "throttles counters" $ simAt Enemy do
            apply Permanent [Throttle 1 Counters]
            as Enemy $ trap 5 (Countered All) $ apply Permanent [Reveal]
            Sim.turns $ 5 - 2
            as Self $ return ()
            not . (`is` Reveal) <$> P.nUser
        it "does not remove counters" $ simAt Enemy do
            apply Permanent [Throttle 1 Counters]
            as Enemy $ trap 5 (Countered All) $ apply Permanent [Reveal]
            Sim.turns $ 5 - 3
            as Self $ return ()
            (`is` Reveal) <$> P.nUser

        it "throttles stuns" $ simAt Enemy do
            apply Permanent [Throttle 1 Stuns]
            as Enemy $ apply 5 [Stun All]
            Sim.turns $ 5 - 1
            userStunned <- Effects.stun <$> P.nUser
            return $ userStunned `shouldBe` mempty
        it "does not remove stuns" $ simAt Enemy do
            apply Permanent [Throttle 1 Stuns]
            as Enemy $ apply 5 [Stun All]
            Sim.turns $ 5 - 2
            userStunned <- Effects.stun <$> P.nUser
            return $ userStunned `shouldBe` singletonSet All

        it "throttles others" $ simAt Enemy do
            apply Permanent [Throttle 1 $ Only Reveal]
            as Enemy $ apply 5 [Reveal]
            Sim.turns $ 5 - 1
            not . (`is` Reveal) <$> P.nUser
        it "does not remove others" $ simAt Enemy do
            apply Permanent [Throttle 1 $ Only Reveal]
            as Enemy $ apply 5 [Reveal]
            Sim.turns $ 5 - 2
            (`is` Reveal) <$> P.nUser

    describe "Undefend" do
        it "ignores own defense" $ simAt Enemy do
            apply Permanent [Undefend]
            Combat.defend Permanent 100
            Combat.damage 1
            targetHealth <- Ninja.health <$> P.nTarget
            return $ 100 - targetHealth `shouldBe` 1

    describe "Uncounter" do
        it "ignores own counters and reflects" $ simAt Enemy do
            apply Permanent [Reflect, ReflectAll All, Uncounter]
            trap Permanent (Counter All) $ return ()
            trap Permanent (CounterAll All) $ return ()
            canTarget

    describe "Unreduce" do
        prop "lessens applied Reduce effects" unreduces

    describe "Weaken" do
        prop "is additive"            $ isAdditive Weaken
        prop "complements Strengthen" $ Weaken `complements` Strengthen

canTargetAs :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m)
            => Target -> m Bool
canTargetAs target = do
    as target $ addStack' fakeStatus
    (== 1) . Ninja.numAnyStacks fakeStatus <$> P.nTarget
  where
    fakeStatus = ""

canTarget :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m) => m Bool
canTarget = canTargetAs Self

simOf :: ∀ a. Wrapper -> Target -> ReaderT Context (StateT Wrapper Identity) a
      -> a
simOf game target action =
    runIdentity $ evalStateT (runReaderT action targeted) game
  where
    targeted = Blank.context { Context.target = Sim.targetSlot target }

harmedWith :: Effect -> ReaderT Context (StateT Wrapper Identity) Ninja -> Bool
harmedWith effect target = simAt Enemy do
    self $ apply 2 [effect]
    as Enemy $ apply Permanent [Reveal]
    (`is` Reveal) <$> target

healthBound :: Int -> Int
healthBound x = max 0 $ min 100 x

simAt :: ∀ a. Target -> ReaderT Context (StateT Wrapper Identity) a -> a
simAt = simOf Blank.game

simEffects :: ∀ a. [Effect] -- ^ User.
           -> [Effect] -- ^ Target.
           -> Target
           -> ReaderT Context (StateT Wrapper Identity) a
           -> a
simEffects userEffects targetEffects = simOf $ Blank.gameOf
    [ Blank.ninja { Ninja.effects = userEffects },   Blank.ninja, Blank.ninja
    , Blank.ninja { Ninja.effects = targetEffects }, Blank.ninja, Blank.ninja
    ]

damageToDefense :: Attack -> Int -> Property
damageToDefense attackType damage = simEffects [] [DamageToDefense] Enemy do
    Combat.attack attackType damage
    targetHealth <- Ninja.health <$> P.nTarget
    return $ 100 - targetHealth === case attackType of
        Attack.Afflict -> healthBound damage
        _              -> 0

damageFromDefense :: Attack -> Int -> Property
damageFromDefense attackType damage = simEffects [] [DamageToDefense] Enemy do
    Combat.attack attackType damage
    targetDefense <- Ninja.totalDefense <$> P.nTarget
    return $ targetDefense === case attackType of
        Attack.Afflict  -> 0
        Attack.Demolish -> 0
        _               -> max 0 damage

attack :: Attack   -- ^ Attack type.
       -> Int      -- ^ Amount.
       -> [Effect] -- ^ Attacker.
       -> [Effect] -- ^ Defender.
       -> Int      -- ^ Result.
attack attackType damage attacker defender =
    Combat.formula attackType (singletonSet All)
    Blank.ninja { Ninja.effects = attacker }
    Blank.ninja { Ninja.effects = defender }
    damage

constrainsHealth :: Bool -> Int -> Property
constrainsHealth endurable health =
    Ninja.health (Ninjas.setHealth health ninja) ===
    max (fromEnum endurable) (min 100 health)
  where
    ninja
      | endurable = Blank.ninja { Ninja.effects = [Endure] }
      | otherwise = Blank.ninja

type Con = EnumSet Class -> Amount -> Int -> Effect

isLimited :: Attack -> Int -> Int -> Property
isLimited attackType amount damage =
    attack attackType damage [] [Limit amount] === case attackType of
        Attack.Afflict -> damage
        _              -> min amount damage

isAdditive :: Con -> Amount -> Attack -> Int -> Int -> Int -> Property
isAdditive effect amount attackType damage size val =
    atk [reducer val] === atk (chunk reducer size val)
  where
    atk efs = attack attackType damage efs efs
    reducer = effect (singletonSet All) amount

complements :: Con -> Con -> Amount -> Int -> Int -> Property
complements effectA effectB amount damage val = atk effects === atk []
  where
    effects  = [effect effectA val, effect effectB val]
    atk efs  = attack Attack.Damage damage efs efs
    effect x = x (singletonSet All) amount

tryAbsorb :: Target -> Chakras -> Chakras
tryAbsorb target cost = simAt target do
    P.alter $ Game.setChakra True cost
    apply Permanent [Absorb]
    Action.act True Act { user = Sim.targetSlot Self, target = t, skill }
    Parity.getOf t . Game.chakra <$> P.game
  where
    t     = Sim.targetSlot target
    skill = Right Skill.new { Skill.cost    = cost
                            , Skill.effects = [ To target $ return () ]
                            }

thresholdConstrains :: Attack -> Int -> Int -> Property
thresholdConstrains attackType damage v = simEffects [] [Threshold v] Enemy do
    Combat.attack attackType damage
    targetHealth <- Ninja.health <$> P.nTarget
    return $ 100 - targetHealth === damageOutput
  where
    damageOutput
      | attackType == Attack.Demolish = 0
      | damage <= v                   = 0
      | otherwise                     = healthBound damage

unreduces :: Int -> Int -> Int -> Property
unreduces damage reduce unreduce = simAt Enemy do
    self $ apply Permanent [Unreduce unreduce]
    apply Permanent [Reduce (singletonSet All) Flat reduce]
    Combat.damage damage
    targetHealth <- Ninja.health <$> P.nTarget
    return $ 100 - targetHealth === healthBound (damage + unreduce - reduce)

getSkill :: [Effect] -> Skill
getSkill effects = fromJust $
                   Ninjas.getSkill (Left 0) ninja { Ninja.effects = effects }
  where
    targets = (`To` return ()) <$> [minBound..maxBound]
    skill   = Skill.new { Skill.effects = targets } :| []
    ninja   = Ninja.new (unsafeHead Slot.all) Blank.character
              { Character.skills = skill :| [skill, skill, skill] }

ninjaWithCooldown :: Int -> Ninja
ninjaWithCooldown cooldown =
    Ninja.new (unsafeHead Slot.all)
    Blank.character { Character.skills = sk :| [sk, sk, sk] }
  where
    sk = Skill.new { Skill.cooldown = fromIntegral cooldown } :| []

simCooldown :: Ninja -> Int
simCooldown n = simOf game Self do
    Action.act True Act { user = slot, target = slot, skill = Left 0 }
    snd . unsafeHead . mapToList . Ninja.cooldowns <$> P.nUser
  where
    slot = Ninja.slot n
    game = Blank.gameOf $ n : (Blank.ninjaWithSlot <$> unsafeTail Slot.all)
