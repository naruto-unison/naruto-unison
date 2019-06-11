{-# LANGUAGE ImpredicativeTypes    #-}
module Action.Combat
  ( damage, pierce, afflict, demolish, demolishAll
  , heal, restore, setHealth
  , leech
  , barrier, barrierDoes
  , defend, addDefense
  , sacrifice
  , kill, killHard
  , factory
  ) where

import ClassyPrelude.Yesod
import           Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.List as List

import           Core.Util ((—), (∈), incr, intersects, sync)
import qualified Class.Classed as Classed
import qualified Class.Labeled as Labeled
import qualified Class.Play as P
import           Class.Play (Play(..), PlayConstraint, PlayT)
import qualified Model.Attack as Attack
import           Model.Attack (Attack)
import qualified Model.Barrier as Barrier
import           Model.Barrier (Barrier)
import           Model.Class (Class(..))
import qualified Model.Context as Context
import qualified Model.Copy as Copy
import qualified Model.Defense as Defense
import           Model.Defense (Defense)
import           Model.Effect (Amount(..), Effect(..))
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Model.Skill as Skill
import qualified Engine.Effects as Effects
import qualified Engine.Execute as Execute
import           Engine.Execute (Affected(..))

absorbDefense :: Int -> [Defense] -> (Int, [Defense])
absorbDefense hp [] = (hp, [])
absorbDefense hp (x:xs)
  | Defense.amount x <= hp = absorbDefense (hp - Defense.amount x) xs
  | otherwise = (0, x { Defense.amount = Defense.amount x - hp } : xs)

absorbBarrier :: Int -> [Barrier] -> (Int, [Barrier])
absorbBarrier hp [] = (hp, [])
absorbBarrier hp (x:xs)
  | Barrier.amount x <= hp = absorbBarrier (hp - Barrier.amount x) xs
  | otherwise = (0, x { Barrier.amount = Barrier.amount x - hp } : xs)

attack :: ∀ m. PlayT m => Attack -> Int -> m ()
attack _ 0 = return ()
attack atk dmg = void $ runMaybeT do
    skill      <- P.skill
    source     <- P.source
    user       <- P.user
    target     <- P.target
    nUser      <- P.nUser
    nTarget    <- P.nTarget
    let classes = atkClass : Skill.classes skill
        direct  = Direct ∈ classes
        dmg'User
          | direct = toRational dmg
          | otherwise = (* Effects.strengthen classes nUser Percent) .
                        (+ Effects.strengthen classes nUser Flat) .
                        toRational $ dmg + Effects.link source nTarget
        reduceAfflic = Effects.reduce [Affliction] nTarget
        reduce
          | atk /= Attack.Damage    = const 0
          | Ninja.is Pierce nUser   = const 0
          | Ninja.is Expose nTarget = const 0
          | otherwise               = Effects.reduce classes nTarget
        weaken
          | atk == Attack.Afflict = const 0
          | direct                = const 0
          | otherwise             = Effects.weaken classes nUser
        bleed = Effects.bleed classes nTarget
        dmg'Target = truncate .
                    (— reduceAfflic Flat) .
                    (— reduce Flat) .
                    (— weaken Flat) .
                    (+ bleed Flat) .
                    (* reduceAfflic Percent) .
                    (* reduce Percent) .
                    (* weaken Percent) .
                    (* bleed Percent) $
                    toRational dmg'User
        (dmg'Barrier, barr) = absorbBarrier dmg'Target $ Ninja.barrier nUser
        handleDefense
          | Ninja.is Undefend nTarget = (,)
          | otherwise            = absorbDefense
        (dmg'Def, defense)
          | direct    = handleDefense dmg'Target $ Ninja.defense nTarget
          | otherwise = handleDefense dmg'Barrier $ Ninja.defense nTarget

    guard . not $ classes `intersects` Effects.invincible nTarget
               || dmg < Effects.threshold nTarget
               || not direct && Ninja.is (Stun atkClass) nUser
               || dmg'Target <= 0

    if atk == Attack.Afflict then
        P.modify . Game.adjust target $ Ninja.adjustHealth (— dmg'Target)
    else do
        unless direct .
            P.modify $ Game.adjust user \n -> n { Ninja.barrier = barr }
        if atk == Attack.Demolish || dmg'Def <= 0 then
            P.modify $ Game.adjust target \n -> n { Ninja.defense = defense }
        else if dmg'Def == 0 then return () else
            P.modify . Game.adjust target $ Ninja.adjustHealth (— dmg'Def) .
                \n -> n { Ninja.defense = defense }
  where
    atkClass = case atk of
        Attack.Afflict -> Affliction
        _              -> NonAffliction

afflict :: ∀ m. PlayT m => Int -> m ()
afflict = attack Attack.Afflict
pierce :: ∀ m. PlayT m => Int -> m ()
pierce = attack Attack.Pierce
damage :: ∀ m. PlayT m => Int -> m ()
damage = attack Attack.Damage

addDefense :: ∀ m. PlayT m => Text -> Int -> m ()
addDefense name amount = do
    source  <- P.source
    target  <- P.source
    nTarget <- P.nTarget
    case find (Labeled.match name source) $ Ninja.defense nTarget of
        Nothing      -> return ()
        Just defense -> P.modify $ Game.adjust target \n ->
            n { Ninja.defense =
                    defense { Defense.amount = Defense.amount defense + amount }
                    : List.deleteBy Labeled.eq defense (Ninja.defense n) }

barrierDoes :: ∀ m. PlayT m => Int -> (Int -> PlayConstraint ())
            -> PlayConstraint () -> Int -> m ()
barrierDoes dur finish while amount = do
    context    <- P.context
    amount'    <- (amount +) . Effects.build <$> P.nSource
    let skill   = Context.skill context
        source  = Context.source context
        target  = Context.target context
        dur'    = Copy.maxDur (Skill.copying skill) $ sync dur
        finish' amt
          | dur' < sync dur = (context, Play $ return ())
          | otherwise       = (context, Play $ Execute.wrap [Trapped] (finish amt))
        barr = Barrier.Barrier
            { Barrier.amount = amount'
            , Barrier.source = source
            , Barrier.name   = Skill.name skill
            , Barrier.while  = (context, Play $ Execute.wrap [Channeled, Trapped] while)
            , Barrier.finish = finish'
            , Barrier.dur    = dur'
            }
    if amount' < 0 then
        P.with Context.reflect $ damage (-amount')
    else when (amount' > 0) . P.modify $ Game.adjust target \n ->
        n { Ninja.barrier = Classed.nonStack skill barr $ Ninja.barrier n }

barrier :: ∀ m. PlayT m => Int -> Int -> m ()
barrier dur = barrierDoes dur (const $ return ()) (return ())

defend' :: ∀ m. PlayT m => Text -> Int -> Int -> m ()
defend' name dur amount = do
    skill      <- P.skill
    source     <- P.source
    target     <- P.target
    nSource    <- P.nSource
    nTarget    <- P.nTarget
    let amount' = Effects.boost source nTarget * amount + Effects.build nSource
        defense = Defense.Defense
                      { Defense.amount = amount'
                      , Defense.source = source
                      , Defense.name   = Skill.defaultName name skill
                      , Defense.dur    = Copy.maxDur (Skill.copying skill) .
                                         incr $ sync dur
                      }
    if amount' < 0 then
        damage (-amount')
    else when (amount' > 0) . P.modify $ Game.adjust target \n ->
        n { Ninja.defense = Classed.nonStack skill defense $ Ninja.defense n }

defend :: ∀ m. PlayT m => Int -> Int -> m ()
defend = defend' ""

demolishAll :: ∀ m. PlayT m => m ()
demolishAll = do
    user   <- P.user
    target <- P.target
    P.modify $ Game.adjust user Ninja.clearBarrier
    P.modify $ Game.adjust target Ninja.clearDefense

demolish :: ∀ m. PlayT m => Int -> m ()
demolish = attack Attack.Demolish

kill :: ∀ m. PlayT m => m ()
kill = P.toTarget $ Ninja.kill True

killHard :: ∀ m. PlayT m => m ()
killHard = P.toTarget $ Ninja.kill False

setHealth :: ∀ m. PlayT m => Int -> m ()
setHealth = P.toTarget . Ninja.setHealth

heal :: ∀ m. PlayT m => Int -> m ()
heal hp = do
    nTarget <- P.nTarget
    unless (Ninja.is Plague nTarget) do
        source  <- P.source
        target  <- P.target
        nSource <- P.nSource
        let hp'  = Effects.boost source nTarget * hp + Effects.bless nSource
        P.modify . Game.adjust target $ Ninja.adjustHealth (+ hp')

restore :: ∀ m. PlayT m => Int -> m ()
restore percent = do
    nTarget <- P.nTarget
    unless (Ninja.is Plague nTarget) do
        source  <- P.source
        target  <- P.target
        nSource <- P.nSource
        let hp'  = Effects.boost source nTarget * percent
                   * (100 - Ninja.health nTarget) `quot` 100
                   + Effects.bless nSource
        P.modify . Game.adjust target $ Ninja.adjustHealth (+ hp')

leech :: ∀ m. PlayT m => Int -> (Int -> m ()) -> m ()
leech hp f = do
    target   <- P.target
    hpBefore <- Ninja.health <$> P.nTarget
    P.modify . Game.adjust target $ Ninja.adjustHealth (— hp)
    hpAfter  <- Ninja.health <$> P.nTarget
    f $ hpBefore - hpAfter

sacrifice :: ∀ m. PlayT m => Int -> Int -> m ()
sacrifice minhp hp = do
    user    <- P.user
    target  <- P.target
    nTarget <- P.nTarget
    unless (user == target && Ninja.is ImmuneSelf nTarget) .
        P.modify . Game.adjust target $ Ninja.sacrifice minhp hp

-- | 'Ninja.reset'
factory :: ∀ m. PlayT m => m ()
factory = P.toTarget Ninja.factory
