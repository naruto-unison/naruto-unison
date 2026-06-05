{-# LANGUAGE OverloadedLists #-}

module Game.Model.EffectSpec (spec) where

import Import hiding (it, shouldBe, shouldNotBe)

import Data.Enum.Set (EnumSet)
import Data.Maybe (fromJust)
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Test.Hspec (it, shouldBe)

import           Class.Hook (MonadHook)
import qualified Class.Parity as Parity
import           Class.Play (MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Game.Action as Action
import qualified Game.Engine.Combat as Combat
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Skills as Skills
import           Game.Model.Attack (Attack)
import qualified Game.Model.Attack as Attack
import           Game.Model.Chakras (Chakras(Chakras))
import qualified Game.Model.Chakras as Chakras
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as N
import qualified Game.Model.Skill as Skill

import qualified Blank
import           OrphanInstances ()
import qualified Sim as Sim
import           Wrapper (WrapperM)
import qualified Wrapper

chunk :: ∀ a. (Int -> a) -> Int -> Int -> [a]
chunk _ _ 0 = []
chunk producer sizeSeed amount = producer r : replicate size (producer q)
  where
    size   = 1 + rem (abs sizeSeed) amount
    (q, r) = amount `quotRem` size

spec :: Spec
spec = parallel do
    describe "Absorb" do
        let tryAbsorb t cost = simAt t do
                apply Permanent [ Absorb ]
                Action.act Context
                    { new       = True
                    , user      = Sim.targetSlot Self
                    , target    = slot
                    , continues = False
                    , skill     = Skill.new
                        { Skill.cost    = cost
                        , Skill.effects = [ To t $ return () ]
                        }
                    }
                Parity.getOf slot . Game.chakra <$> P.game
              where
                slot = Sim.targetSlot t
        it "gains chakra from enemy skills" $
            tryAbsorb Enemy (Chakras 1 1 1 1 1) `shouldBe` (Chakras 1 1 1 1 1)
        it "does not gain chakra from friendly skills" $
            tryAbsorb Ally (Chakras 2 2 2 2 2) `shouldBe` mempty

    describe "Afflict" do
        prop "damages every turn" \amount (Positive turns) -> simAt Enemy do
            apply Permanent [Afflict amount]
            damaged <- measureDamage $ Sim.turns -turns
            return $ damaged === healthBound (amount * turns)

    describe "Alone" do
        let tryTarget = apply Permanent [Alone] >> canTarget

        it "blocks allies"    . not $ simAt Ally  tryTarget
        it "does not block enemies" $ simAt Enemy tryTarget
        it "does not block self"    $ simAt Self  tryTarget

    -- describe "Alternate" (redundant)

    describe "AntiCounter" do
        it "ignores counters and reflects" $ simAt Enemy do
            apply Permanent [Reflect, ReflectAll All]
            trap Permanent (Counter All) $ return ()
            trap Permanent (CounterAll All) $ return ()
            targeting Self do
                trap Permanent (Countered All) $ return ()
                apply Permanent [AntiCounter]
            canTarget

    describe "Bleed" do
        prop "is additive"        $ isAdditive Bleed
        prop "complements Reduce" $ Bleed `complements` Reduce

    describe "Bless" do
        prop "adds to healing" \(NonNegative i) (Positive hp) ->
            simEffects [ Bless i ] [] Ally do
                setHealth 1
                heal hp
                targetHealth <- target health
                return $ targetHealth === healthBound (1 + hp + i)

    describe "Block" do
        let tryTarget t = apply Permanent [ Block $ Sim.targetSlot t ]
                          >> Sim.at XEnemies (canTargetAs Enemy)

        it "blocks vs. subject"  . not . simAt Enemy $ tryTarget XEnemies
        it "does not block vs. others" $ simAt Enemy $ tryTarget REnemy

    describe "BlockAllies" do
        let tryTarget = targeting Self (apply Permanent [ BlockAllies ])
                        >> canTarget

        it "blocks vs. allies"    . not $ simAt Ally  tryTarget
        it "does not block vs. enemies" $ simAt Enemy tryTarget
        it "does not block vs. self"    $ simAt Self  tryTarget

    describe "BlockEnemies" do
        let tryTarget = targeting Self (apply Permanent [BlockEnemies])
                        >> canTarget

        it "blocks vs. enemies"  . not $ simAt Enemy tryTarget
        it "does not block vs. allies" $ simAt Ally  tryTarget

    describe "Boost" do
        let boostAmount = 4
            dmg         = 70
            reduce      = 3

        it "boosts helpful effects" $ simAt Self do
            Sim.as Ally $ apply Permanent [ Reduce [All] Flat reduce ]
            targeting Self $ apply Permanent [ Boost boostAmount ]
            damaged <- measureDamage $ Sim.as Enemy $ damage dmg
            return $ dmg - damaged `shouldBe` boostAmount * reduce

    describe "Build" do
        prop "adds to barrier" \i (Positive hp) ->
            simEffects [ Build i ] [] Ally do
                barricade Permanent hp
                if i + hp >= 0 then do
                    amount <- target totalBarrier
                    return $ amount === i + hp
                else do
                    amount <- target totalDefense
                    return $ amount === negate (i + hp)

        prop "adds to defense" \i (Positive hp) ->
            simEffects [ Build i ] [] Ally do
                defend Permanent hp
                if i + hp >= 0 then do
                    amount <- target totalDefense
                    return $ amount === i + hp
                else do
                    amount <- target totalBarrier
                    return $ amount === negate (i + hp)

    describe "Bypass" do
        it "makes all skills bypass" $ simAt Enemy do
            targeting Self $ apply Permanent [ Bypass ]
            apply Permanent [ Invulnerable All ]
            canTarget

    describe "DamageToDefense" do
        prop "absorbs damage" \attackType (Positive dmg) ->
            simEffects [] [ DamageToDefense ] Enemy do
                Combat.attack attackType dmg
                targetHealth <- target health
                return $ 100 - targetHealth === case attackType of
                    Attack.Afflict -> healthBound dmg
                    _              -> 0

        prop "converts into defense" \attackType (Positive dmg) ->
            simEffects [] [ DamageToDefense ] Enemy do
                Combat.attack attackType dmg
                targetDefense <- target totalDefense
                return $ targetDefense === case attackType of
                    Attack.Afflict  -> 0
                    Attack.Demolish -> 0
                    _               -> dmg

    describe "Disable" do
        it "stuns stuns" $ simAt Enemy do
            apply Permanent [ Disable Stuns ]
            Sim.as Enemy $ apply Permanent [ Stun All ]
            userStunned <- user Effects.stun
            return $ userStunned `shouldBe` mempty

        it "stuns counters" $ simAt Enemy do
            apply Permanent [ Disable Counters ]
            Sim.as Enemy do
                targeting Self do
                    trap Permanent (Counter All) $ return ()
                    trap Permanent (CounterAll All) $ return ()
                trap Permanent (Countered All) $ return ()
            canTarget

        it "stuns others" $ simAt Enemy do
            apply Permanent [ Disable $ Any ReflectAll ]
            Sim.as Enemy $ targeting Self $
                apply Permanent $ ReflectAll <$> [minBound..maxBound]
            canTarget

    describe "Duel" do
        let tryTarget t = Sim.as XAlly
                          (apply Permanent [ Duel $ Sim.targetSlot t ])
                          >> canTarget

        it "invulnerable to enemies" . not . simAt Enemy $ tryTarget XEnemies
        it "invulnerable to allies"  . not . simAt Ally  $ tryTarget XEnemies
        it "not invulnerable to subject"   . simAt Ally  $ tryTarget Self

    describe "Endure" do
        prop "constraints health" \endurable (Positive currentHealth) ->
            let
                ninja
                  | endurable = Blank.ninja { effects = [Endure] }
                  | otherwise = Blank.ninja
            in
            health (Ninjas.setHealth currentHealth ninja)
            === max (fromEnum endurable) (min 100 currentHealth)

    describe "Enrage" do
        let tryApply effect = do
                apply' "1" Permanent [ effect ]
                apply' "2" Permanent [ Enrage ]
                apply' "3" Permanent [ effect ]
                target (`is` effect)

        it "ignores negative effects" . not  . simAt Ally $ tryApply Plague
        it "does not ignore helpful effects" . simAt Ally $ tryApply Focus

    describe "Exhaust" do
        prop "increases skill costs" \(Positive exhaust) ->
            Skill.cost (getSkill $ replicate exhaust $ Exhaust [All])
            === mempty { Chakras.rand = exhaust }

    describe "Expose" do
        it "prevents target from becoming invulnerable" $ simAt Enemy do
            apply Permanent [ Expose ]
            Sim.as Enemy $ targeting Self $ apply Permanent [ Invulnerable All ]
            canTarget

        it "prevents target from reducing damage" $ simAt Enemy do
            apply Permanent [ Reduce [All] Flat 100
                            , Expose
                            ]
            damaged <- measureDamage $ damage 1
            return $ damaged `shouldBe` 1

    -- describe "Face" (nothing to test)

    describe "Focus" do
        it "ignores stuns" $ simAt Enemy do
            targeting Self $ apply Permanent [ Focus
                                             , Disable (Only Reveal)
                                             , Silence
                                             , Stun All
                                             ]
            apply Permanent [ Reveal ]
            target (`is` Reveal)

    describe "Heal" do
        prop "heals every turn" \i (Positive turns) -> simAt Enemy do
            setHealth 1
            apply Permanent [ Heal i ]
            Sim.turns -turns
            targetHealth <- target health
            return $ targetHealth === healthBound (1 + i * turns)

    describe "Invulnerable" do
        let ignore atk dmg = do
                apply Permanent [ Invulnerable atk ]
                dmg 50
                targetHealth <- target health
                return $ targetHealth `shouldBe` 100

        it "ignores damage"     . simAt Enemy $ ignore NonAffliction damage
        it "ignores piercing"   . simAt Enemy $ ignore NonAffliction pierce
        it "ignores affliction" . simAt Enemy $ ignore Affliction afflict

    describe "Limit" do
        prop "limits damage" \attackType amount (Positive dmg) ->
            attackAmount attackType dmg [] [Limit amount] === case attackType of
                Attack.Afflict -> dmg
                _              -> min amount dmg

    describe "NoIgnore" do
        it "ignores ignores" $ simAt Enemy do
            apply Permanent [ Focus
                            , NoIgnore
                            , Stun All
                            ]
            targetStunned <- target Effects.stun
            return $ targetStunned `shouldBe` [All]

    describe "Nullify" do
        it "nullifies harm"  . not $ simEffects [] [ Nullify ] Enemy canTarget
        it "does not nullify help" $ simEffects [] [ Nullify ] Ally  canTarget

    describe "Pierce" do
        it "ignores damage reduction" $ simAt Enemy do
            targeting Self $ apply Permanent [ Pierce ]
            apply Permanent [ Reduce [All] Flat 100 ]
            damaged <- measureDamage $ damage 1
            return $ damaged `shouldBe` 1

    describe "Plague" do
        it "blocks healing" $ simAt Enemy do
            setHealth 1
            apply Permanent [ Plague
                            , Heal 100
                            ]
            healed <- measureHealing $ heal 100
            return $ healed `shouldBe` 0

        it "blocks curing" $ simAt Enemy do
            apply Permanent [ Plague ]
            cureAll
            target (`is` Plague)

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
            Skill.targets (getSkill [ Restrict ])
            `shouldBe` setFromList [minBound..maxBound]
            \\ setFromList [Enemies, XEnemies, Everyone]

    -- describe "Reveal" (nothing to test)

    describe "Seal" do
        let tryApply effect = do
                apply' "1" Permanent [ effect ]
                apply' "2" Permanent [ Seal ]
                apply' "3" Permanent [ effect ]
                target (`is` effect)
        it "ignores helpful effects"   . not . simAt Self $ tryApply Focus
        it "does not ignore harmful effects" . simAt Self $ tryApply Reveal

    describe "Share" do
        let harms t = do
                apply Permanent [ Share $ Sim.targetSlot Enemy ]
                Sim.as t $ apply Permanent [ Reveal ]
                (`is` Reveal) <$> Sim.targets Enemy

        it "shares harm"                . simAt Ally $ harms XEnemies
        it "does not share other" . not . simAt Ally $ harms XAlly

    describe "Silence" do
        it "blocks non-damage" $ simAt Enemy do
            targeting Self $ apply Permanent [ Silence ]
            damage 1
            heal 100
            targetHealth <- target health
            return $ 100 - targetHealth `shouldBe` 1

    describe "Snare" do
        prop "increases cooldowns" \cd snare ->
            let
                skill = Skill.new { Skill.cooldown = cd }
                simCooldown n@Ninja{slot} = Wrapper.run game do
                    Action.act ctx
                    snd . unsafeHead . mapToList . cooldowns <$> P.ninja slot
                  where
                    game = Wrapper.new $ n : unsafeTail Blank.ninjas
                    ctx  = Context
                        { new       = True
                        , user      = slot
                        , target    = slot
                        , skill     = skill
                        , continues = False
                        }
                nCd     = Blank.ninjaWithSkill skill
                nSnared = nCd { effects = [ Snare snare ] }
            in
            simCooldown nSnared === max 0 (simCooldown nCd + 2 * snare)

    describe "Strengthen" do
        prop "is additive"        $ isAdditive Strengthen
        prop "complements Weaken" $ Strengthen `complements` Weaken

    -- describe "Stun" (redundant)

    describe "Swap" do
        it "swaps allies and enemies as targets" $
            Skill.targets (getSkill [ Swap ])
            `shouldBe` Skill.targets (Skills.swap $ getSkill [])

    describe "Taunt" do
        let tryTarget = targeting Self
                        (apply Permanent [ Taunt $ Sim.targetSlot Enemy ])
                        >> canTarget

        it "does not block against subject" $ simAt Enemy    tryTarget
        it "does not block against self"    $ simAt Self     tryTarget
        it "blocks against others"    . not $ simAt XEnemies tryTarget

    describe "Throttle" do
        it "throttles counters" $ simAt Enemy do
            apply Permanent [ Throttle 1 Counters ]
            Sim.as Enemy $ trap 5 (Countered All) $ apply Permanent [ Reveal ]
            Sim.turns $ 5 - 2
            Sim.as Self $ return ()
            not <$> user (`is` Reveal)
        it "does not remove counters" $ simAt Enemy do
            apply Permanent [ Throttle 1 Counters ]
            Sim.as Enemy $ trap 5 (Countered All) $ apply Permanent [ Reveal ]
            Sim.turns $ 5 - 3
            Sim.as Self $ return ()
            user (`is` Reveal)

        it "throttles stuns" $ simAt Enemy do
            apply Permanent [ Throttle 1 Stuns ]
            Sim.as Enemy $ apply 5 [ Stun All ]
            Sim.turns $ 5 - 1
            userStunned <- user Effects.stun
            return $ userStunned `shouldBe` mempty
        it "does not remove stuns" $ simAt Enemy do
            apply Permanent [ Throttle 1 Stuns ]
            Sim.as Enemy $ apply 5 [ Stun All ]
            Sim.turns $ 5 - 2
            userStunned <- user Effects.stun
            return $ userStunned `shouldBe` [All]

        it "throttles others" $ simAt Enemy do
            apply Permanent [ Throttle 1 (Only Reveal) ]
            Sim.as Enemy $ apply 5 [ Reveal ]
            Sim.turns $ 5 - 1
            not <$> user (`is` Reveal)
        it "does not remove others" $ simAt Enemy do
            apply Permanent [ Throttle 1 (Only Reveal) ]
            Sim.as Enemy $ apply 5 [ Reveal ]
            Sim.turns $ 5 - 2
            user (`is` Reveal)

    describe "Undefend" do
        it "ignores own defense" $ simAt Enemy do
            apply Permanent [ Undefend ]
            defend Permanent 100
            damaged <- measureDamage $ damage 1
            return $ damaged `shouldBe` 1

    describe "Uncounter" do
        it "ignores own counters and reflects" $ simAt Enemy do
            apply Permanent [ Reflect
                            , ReflectAll All
                            , Uncounter
                            ]
            trap Permanent (Counter All) $ return ()
            trap Permanent (CounterAll All) $ return ()
            canTarget

    describe "Unreduce" do
        prop "lessens applied Reduce effects" \(Positive dmg) reduce unreduce ->
            simAt Enemy do
                targeting Self $ apply Permanent [ Unreduce unreduce ]
                apply Permanent [ Reduce [All] Flat reduce ]
                damaged <- measureDamage $ damage dmg
                return $ damaged === healthBound (dmg + unreduce - reduce)

    describe "Weaken" do
        prop "is additive"            $ isAdditive Weaken
        prop "complements Strengthen" $ Weaken `complements` Strengthen

canTargetAs :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m)
            => Target -> m Bool
canTargetAs t = do
    Sim.as t $ addStack' fakeStatus
    target $ N.has fakeStatus (Sim.targetSlot t)
  where
    fakeStatus = ""

canTarget :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m) => m Bool
canTarget = canTargetAs Self

harmedWith :: Effect -> ReaderT Context WrapperM Ninja -> Bool
harmedWith effect t = simAt Enemy do
    targeting Self $ apply 2 [ effect ]
    Sim.as Enemy $ apply Permanent [ Reveal ]
    (`is` Reveal) <$> t

healthBound :: Int -> Int
healthBound x = max 0 $ min 100 x

simEffects :: ∀ a. [Effect] -- ^ User.
           -> [Effect] -- ^ Target.
           -> Target
           -> ReaderT Context WrapperM a
           -> a
simEffects userEffects targetEffects t =
    simOf (Wrapper.new $ applyEffects <$> Blank.ninjas) t
  where
    applyEffects n@Ninja{slot}
      | slot == Sim.targetSlot Self = n { effects = userEffects }
      | slot == Sim.targetSlot t    = n { effects = targetEffects }
      | otherwise                   = n

attackAmount :: Attack   -- ^ Attack type.
             -> Int      -- ^ Amount.
             -> [Effect] -- ^ Attacker.
             -> [Effect] -- ^ Defender.
             -> Int      -- ^ Result.
attackAmount attackType dmg attacker defender =
    Combat.formula attackType [All]
    Blank.ninja { effects = attacker }
    Blank.ninja { effects = defender }
    dmg

type Con = EnumSet Class -> Amount -> Int -> Effect

isAdditive :: Con -> Amount -> Attack -> Positive Int -> Int -> Int -> Property
isAdditive effect amount attackType (Positive dmg) size val =
    atk [reducer val] === atk (chunk reducer size val)
  where
    atk efs = attackAmount attackType dmg efs efs
    reducer = effect [All] amount

complements :: Con -> Con -> Amount -> Positive Int -> Int -> Property
complements effectA effectB amount (Positive dmg) val = atk effects === atk []
  where
    effects  = [effect effectA val, effect effectB val]
    atk efs  = attackAmount Attack.Damage dmg efs efs
    effect x = x [All] amount

getSkill :: [Effect] -> Skill
getSkill effects = fromJust
    $ Ninjas.getSkill 0 ninja { effects = effects }
  where
    targets = (`To` return ()) <$> [minBound..maxBound]
    ninja   = Blank.ninjaWithSkill Skill.new { Skill.effects = targets }
