module Action.Skill
  ( alterCd
  , reset, resetCharges, resetAll
  , copyAll, copyLast, teach, teachOne
  , vary, vary', varyLoadout, varyNext
  ) where

import ClassyPrelude.Yesod hiding ((<|))
import qualified Data.List as List
import           Data.List.NonEmpty ((<|), NonEmpty(..))
import qualified Data.Sequence as Seq

import           Core.Util (incr, sync)
import qualified Class.Play as P
import           Class.Play (PlayT)
import qualified Class.TurnBased as TurnBased
import           Model.Channel (Channeling(..))
import qualified Model.Character as Character
import qualified Model.Copy as Copy
import           Model.Copy (Copy, Copying)
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Model.Skill as Skill
import           Model.Slot (Slot)
import qualified Model.Variant as Variant
import qualified Engine.Adjust as Adjust
import qualified Engine.Cooldown as Cooldown
import qualified Engine.Execute as Execute
import qualified Engine.SkillTransform as SkillTransform

alterCd :: ∀ m. PlayT m => Int -> Int -> Int -> m ()
alterCd s v = P.toTarget . Cooldown.alter s v

-- | 'Ninja.reset'
reset :: ∀ m. PlayT m => Text -> Text -> m ()
reset name = P.toTarget . Cooldown.reset name

-- | 'Ninja.resetAll'
resetAll :: ∀ m. PlayT m => m ()
resetAll = P.toTarget Cooldown.resetAll

-- | 'Ninja.resetCharges'
resetCharges :: ∀ m. PlayT m => m ()
resetCharges = P.toTarget Ninja.resetCharges

unsafeVary :: ∀ m. PlayT m => Bool -> Int -> Int -> Int -> m ()
unsafeVary fromSkill dur s v = do
    skill      <- P.skill
    nUser      <- P.nUser
    let copying = Skill.copying skill
    unless (shallow copying) do
        target     <- P.target
        let dur'    = Copy.maxDur copying . sync $ incr dur
            variant = Variant.Variant
                { Variant.variant   = v
                , Variant.ownCd     = Skill.varicd $ Adjust.skill' nUser s v
                , Variant.name      = case Skill.channel skill of
                    Instant -> ""
                    _       -> Skill.name skill
                , Variant.fromSkill = fromSkill
                , Variant.dur       = dur'
                }
            adjust
              | dur' <= 0 = Seq.update s $ variant :| []
              | otherwise = Seq.adjust' (variant <|) s
        P.modify $ Game.adjust target \n ->
            n { Ninja.variants = adjust $ Ninja.variants n }
  where
    shallow Copy.Shallow{} = True
    shallow _              = False

varyFull :: ∀ m. PlayT m => Bool -> Int -> Text -> Text -> m ()
varyFull from dur name variant = do
    nUser <- P.nUser
    SkillTransform.safe (return ()) (unsafeVary from dur) nUser name variant

vary' :: ∀ m. PlayT m => Int -> Text -> Text -> m ()
vary' = varyFull False

vary :: ∀ m. PlayT m => Text -> Text -> m ()
vary name variant = do
    dur <- TurnBased.getDur . Skill.channel <$> P.skill
    unless (dur == 1) $ varyFull True dur name variant

varyLoadout :: ∀ m. PlayT m
            => ( Int  -- ^ Base offset added to the first 'Variant' slot
               , Int  -- ^ Base offset added to the second 'Variant' slot
               , Int  -- ^ Base offset added to the third 'Variant' slot
               , Bool -- ^ Whether to affect the fourth 'Variant' slot at all
               )
            -> Int  -- ^ Counter added to all 'Variant' slots
            -> m () -- ^ Recalculates the 'Variant's of a target 'Ninja'
varyLoadout (a, b, c, affectsFourth) i = do
    unsafeVary False 0 0 $ i + a
    unsafeVary False 0 1 $ i + b
    unsafeVary False 0 2 $ i + c
    when affectsFourth $ unsafeVary False 0 3 i

varyNext :: ∀ m. PlayT m => Text -> m ()
varyNext name = do
    target <- P.target
    maybeS <- List.findIndex (any match) .
              toList . Character.skills . Ninja.character <$> P.nTarget
    case maybeS of
        Nothing -> return ()
        Just s  -> P.modify $ Game.adjust target \n ->
            n { Ninja.variants = Seq.adjust' adj s $ Ninja.variants n }
  where
    match = (== toCaseFold name) . toCaseFold . Skill.name
    adj vs@(x:|xs)
      | variant <= 0 = vs
      | otherwise    = x { Variant.variant = 1 + variant } :| xs
      where
        variant = Variant.variant x

copyAll :: ∀ m. PlayT m => Int -> m ()
copyAll dur = do
    source  <- P.source
    target  <- P.target
    nTarget <- P.nTarget
    let copy skill = Just $ Copy.Copy { Copy.dur   = dur'
                                      , Copy.skill = copying skill target
                                      }
    P.modify $ Game.adjust source \n ->
        n { Ninja.copies = fromList $ copy <$> Adjust.skills nTarget }
  where
    dur'
      | dur < 0   = incr $ sync dur
      | otherwise = sync dur
    copying skill target = skill
        { Skill.copying = Copy.Deep (Copy.root skill target) $ sync dur - 1 }

copyLast :: ∀ m. PlayT m => Int -> Int -> m ()
copyLast dur s = do
    skill      <- P.skill
    user       <- P.user
    target     <- P.target
    mLastSkill <- Ninja.lastSkill <$> P.nTarget
    case mLastSkill of
        Nothing -> return ()
        Just lastSkill -> P.modify $
            Execute.copy False Copy.Shallow target lastSkill
            (user, Skill.name skill, s, sync dur)

teacher :: ∀ m. PlayT m => (Maybe Copy -> Seq (Maybe Copy) -> Seq (Maybe Copy))
        -> Int -> (Slot -> Int -> Copying) -> Int -> m ()
teacher f dur cop s = do
    source    <- P.source
    target    <- P.target
    nSource   <- P.nSource
    let skill  = (Adjust.skill (Left s) nSource)
                 { Skill.copying = cop source $ sync dur - 1 }
        copied = Just $ Copy.Copy { Copy.skill = skill
                                  , Copy.dur   = dur'
                                  }
    P.modify $ Game.adjust target \n ->
        n { Ninja.copies = f copied $ Ninja.copies n }
  where
    dur'
      | dur < 0   = incr $ sync dur
      | otherwise = sync dur

teach :: ∀ m. PlayT m => Int -> (Slot -> Int -> Copying) -> Int -> m ()
teach = teacher $ const . replicate 4

teachOne :: ∀ m. PlayT m
         => Int -> Int -> (Slot -> Int -> Copying) -> Int -> m ()
teachOne dur s' = teacher (Seq.update s') dur
