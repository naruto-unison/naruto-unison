{-# LANGUAGE ImpredicativeTypes    #-}
module Action.Status
  ( apply, apply', applyWith
  , tag, tag'
  , flag, flag'
  , hide, hide'
  , bomb, bomb', bombWith, bombWith'
  , addStack, addStacks, addStacks'
  , setFace
  , refresh, hasten, prolong
  , cure, cureAll, cureBane, cureStun, purge, remove, removeStack, removeStacks
  , snapshot
  , commandeer
  ) where

import ClassyPrelude.Yesod
import           Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.List as List

import           Core.Util ((∈), (∉), incr, sync)
import qualified Class.Play as P
import           Class.Play (Play(..), PlayT)
import           Class.Random (RandomT)
import qualified Model.Channel as Channel
import           Model.Channel (Channeling(..))
import           Model.Class (Class(..))
import qualified Model.Copy as Copy
import qualified Model.Effect as Effect
import           Model.Effect (Effect(..))
import qualified Model.Face as Face
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Model.Skill as Skill
import qualified Model.Status as Status
import           Model.Status (Bomb)
import qualified Engine.Adjust as Adjust
import qualified Engine.Effects as Effects
import qualified Action.Channel as ActionChannel

refresh :: ∀ m. PlayT m => Text -> m ()
refresh = P.fromSource . Ninja.refresh

hasten :: ∀ m. PlayT m => Int -> Text -> m ()
hasten dur name = P.source >>= P.toTarget . Ninja.hasten (sync dur) name

prolong :: ∀ m. PlayT m => Int -> Text -> m ()
prolong dur name = do
    source  <- P.source
    copying <- Skill.copying <$> P.skill
    P.toTarget $ Ninja.prolong (Copy.maxDur copying $ sync dur) name source

setFace :: ∀ m. PlayT m => Int -> m ()
setFace dur = do
    skill <- P.skill
    let copying = Skill.copying skill
    case copying of
        Copy.NotCopied -> do
            source <- P.source
            let face = Face.Face { Face.icon   = Skill.name skill
                                 , Face.source = source
                                 , Face.dur    = sync dur
                                 }
            P.toTarget \n -> n { Ninja.face = face : Ninja.face n }
        _ -> return ()

applyFull :: ∀ m. (PlayT m, RandomT m)
          => [Class] -> Bool -> [(Bomb, Play ())] -> Text -> Int -> [Effect]
          -> m ()
applyFull classes bounced bombs name unthrottled fs = void $ runMaybeT do
        skill       <- P.skill
        source      <- P.source
        user        <- P.user
        target      <- P.target
        nSource     <- P.nSource
        nUser       <- P.nUser
        nTarget     <- P.nTarget
        let already  = Ninja.has name source nTarget
            isSingle = name == Skill.name skill && Single ∈ classes
            throttle = Effects.throttle fs nSource
            self     = source == user && user == target
            noremove = self && any (not . Effect.helpful) fs
                       || unthrottled == 0
                       || unthrottled == 1 && Skill.channel skill /= Instant
                       || null fs && Bane ∉ Skill.classes skill
            extra    = fst <$> filter snd
                       [ (Soulbound,   any bind fs)
                       , (Unremovable, noremove)
                       ]
            classes' = List.delete Resource . List.nub $
                       extra ++ classes ++ Skill.classes skill
            silenced = Ninja.is Silence nUser
            filt
              | silenced && bounced = const []
              | silenced = filter isDmg . Adjust.effects nTarget
              | bounced  = filter (not . isDmg) . Adjust.effects nTarget
              | otherwise = id
            dur
              | Direct ∈ classes = unthrottled
              | unthrottled >= 0 = max 0 $ unthrottled - throttle
              | otherwise        = min 0 $ unthrottled + throttle
            newSt = Status.new source dur skill
            st    = newSt
                  { Status.name    = Skill.defaultName name skill
                  , Status.user    = user
                  , Status.effects = filt fs
                  , Status.classes = List.delete Resource . List.nub $
                                     extra ++ classes ++ Skill.classes skill
                  , Status.bombs   = guard (Status.dur newSt <= incr (sync dur))
                                     >> bombs
                  }
            prolong' = mapMaybe $
                       Ninja.prolong' (Status.dur st) name (Status.root st)
        guard . not $ dur == 0 && unthrottled /= 0 && not (null fs)
                   || null fs && Shifted ∈ classes
                   || already && (bounced || isSingle)
        if already && Extending ∈ classes' then
            P.modify $ Game.adjust target \n ->
                n { Ninja.statuses = prolong' $ Ninja.statuses n }
        else do
            guard $ null fs || not (null $ Status.effects st)
            P.modify . Game.adjust target $ Ninja.addStatus st
            lift . ActionChannel.doDisrupts $ any (∈ Status.effects st) .
                   (Stun <$>) . Skill.classes . Channel.skill
            when (bounced && not self) do
                let bounce t = P.withTarget t $
                               applyFull [] True (Status.bombs st) name dur fs
                lift . traverse_ bounce . List.delete source $
                       Effects.share nTarget
  where
    bind Redirect{}   = True
    bind _            = False
    isDmg (Afflict x) = x > 0
    isDmg _           = False

applyWith' :: ∀ m. (PlayT m, RandomT m) => [Class] -> Text -> Int -> [Effect] -> m ()
applyWith' classes = applyFull classes False []
apply' :: ∀ m. (PlayT m, RandomT m) => Text -> Int -> [Effect] -> m ()
apply' = applyWith' []
applyWith :: ∀ m. (PlayT m, RandomT m) => [Class] -> Int -> [Effect] -> m ()
applyWith classes = applyWith' classes ""
-- | Adds a 'Status' to the target.
apply :: ∀ m. (PlayT m, RandomT m) => Int -> [Effect] -> m ()
apply = apply' ""

addStacks' :: ∀ m. PlayT m => Int -> Text -> Int -> m ()
addStacks' _    _ 0   = return ()
addStacks' dur name i = do
    skill  <- P.skill
    source <- P.source
    user   <- P.user
    target <- P.target
    let st  = Status.new source dur skill
    P.modify . Game.adjust target $ Ninja.addStatus
        st { Status.name   = name
           , Status.amount = i
           , Status.user   = user
           , Status.classes = Unremovable : Status.classes st
           }

-- 'apply' replicated.
addStacks :: ∀ m. PlayT m => Text -> Int -> m ()
addStacks = addStacks' 0
addStack :: ∀ m. PlayT m => m ()
addStack = do
    name <- Skill.name <$> P.skill
    addStacks name 1

bombWith' :: ∀ m. (PlayT m, RandomT m)
          => [Class] -> Text -> Int -> [Effect] -> [(Bomb, Play ())]
          -> m ()
bombWith' classes name dur fs bombs =
    applyFull classes False bombs name dur fs
bombWith :: ∀ m. (PlayT m, RandomT m)
         => [Class] -> Int -> [Effect] -> [(Bomb, Play ())] -> m ()
bombWith classes = bombWith' classes ""
bomb' :: ∀ m. (PlayT m, RandomT m)
      => Text -> Int -> [Effect] -> [(Bomb, Play ())] -> m ()
bomb' = bombWith' []
-- Applies a 'Status' that causes a 'Play ()' when it ends,
-- either by expiring or by being cured.
bomb :: ∀ m. (PlayT m, RandomT m)
     => Int -> [Effect] -> [(Bomb, Play ())] -> m ()
bomb = bomb' ""

flag' :: ∀ m. (PlayT m, RandomT m) => Text -> m ()
flag' name = applyWith' [Hidden, Unremovable, Nonstacking] name (-1) []
-- Applies a hidden 'Status' with no effects that immediately expires.
flag :: ∀ m. (PlayT m, RandomT m) => m ()
flag = flag' ""

tag' :: ∀ m. (PlayT m, RandomT m) => Text -> Int -> m ()
tag' name dur = applyWith' [Unremovable, Nonstacking] name dur []
-- Applies a 'Status' with no effects, used as a marker for other 'Skill's.
tag :: ∀ m. (PlayT m, RandomT m) => Int -> m ()
tag = tag' ""

hide' :: ∀ m. (PlayT m, RandomT m) => Text -> Int -> [Effect] -> m ()
hide' = applyWith' [Unremovable, Hidden]
-- Applies a 'Hidden' 'Status'.
hide :: ∀ m. (PlayT m, RandomT m) => Int -> [Effect] -> m ()
hide = hide' ""

-- | 'N.cure'
cure :: ∀ m. PlayT m => (Effect -> Bool) -> m ()
cure match = P.toTarget $ Ninja.cure match

cureAll :: ∀ m. PlayT m => m ()
cureAll = cure $ const True

-- | 'N.cureBane'
cureBane :: ∀ m. PlayT m => m ()
cureBane = P.toTarget Ninja.cureBane

cureStun :: ∀ m. PlayT m => m ()
cureStun = cure cured
  where
    cured Stun{} = True
    cured _      = False

-- | 'N.purge'
purge :: ∀ m. PlayT m => m ()
purge = P.toTarget Ninja.purge

-- | 'N.clear'
remove :: ∀ m. PlayT m => Text -> m ()
remove = P.fromSource . Ninja.clear

-- | 'N.removeStack'
removeStack :: ∀ m. PlayT m => Text -> m ()
removeStack = P.toTarget . Ninja.removeStack

-- | 'N.removeStacks'
removeStacks :: ∀ m. PlayT m => Text -> Int -> m ()
removeStacks name = P.fromSource . Ninja.removeStacks name

snapshot :: ∀ m.PlayT m => Int -> m ()
snapshot dur = do
    skill   <- P.skill
    source  <- P.source
    user    <- P.user
    target  <- P.target
    nTarget <- P.nTarget
    let st   = Status.new source dur skill
    P.modify . Game.adjust target $ Ninja.addStatus
        st { Status.user    = user
           , Status.effects = [Snapshot nTarget]
           , Status.classes = Nonstacking : Unremovable : Status.classes st
           }

-- | Steals all of the target's beneficial effects.
commandeer :: ∀ m. PlayT m => m ()
commandeer = do
    user    <- P.user
    target  <- P.target
    nUser   <- P.nUser
    nTarget <- P.nTarget
    P.modify $ Game.adjust user \n ->
        n { Ninja.defense  = Ninja.defense nTarget ++ Ninja.defense n
          , Ninja.barrier  = []
          , Ninja.statuses = mapMaybe gainHelpful (Ninja.statuses nTarget)
                             ++ Ninja.statuses n
          }
    P.modify $ Game.adjust target \n ->
        n { Ninja.defense  = []
          , Ninja.barrier  = Ninja.barrier nUser
          , Ninja.statuses = mapMaybe loseHelpful $ Ninja.statuses n
         }
  where
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
    lose ef = Effect.helpful ef && not (Effect.sticky ef)
