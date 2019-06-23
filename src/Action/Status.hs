{-# LANGUAGE ImpredicativeTypes #-}
-- | Actions that characters can use to affect 'Status.Status'es.
module Action.Status
  ( -- * Applying statuses
    apply, apply', applyWith
  , tag, tag'
  , flag, flag'
  , hide, hide'
  , addStack, addStacks, addStacks'
    -- * Applying bombs
  , bomb, bomb', bombWith, bombWith'
  -- * Adjusting statuses
  , refresh, prolong, hasten
  -- * Removing statuses
  , cure, cureAll, cureBane, cureStun, purge, remove, removeStack, removeStacks
  -- * Specialized
  , setFace
  , snapshot
  , commandeer
  ) where

import ClassyPrelude

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List (nub)

import           Core.Util ((∈), (∉), enumerate, intersects)
import qualified Class.Play as P
import           Class.Play (Play(..), MonadPlay)
import           Class.Random (MonadRandom)
import qualified Model.Channel as Channel
import           Model.Channel (Channeling(..))
import           Model.Class (Class(..))
import qualified Model.Copy as Copy
import qualified Model.Duration as Duration
import           Model.Duration (Duration(..), Turns, incr, sync)
import qualified Model.Effect as Effect
import           Model.Effect (Effect(..))
import qualified Model.Face as Face
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Model.Skill as Skill
import qualified Model.Status as Status
import           Model.Status (Bomb)
import           Model.Trap (Trigger(..))
import qualified Engine.Adjust as Adjust
import qualified Engine.Effects as Effects
import qualified Action.Channel as ActionChannel

-- | Refreshes the 'Status.dur' of 'Ninja.statuses' with matching 'Status.name'
-- to 'Status.maxDur'. Uses 'Ninja.refresh' internally.
refresh :: ∀ m. MonadPlay m => Text -> m ()
refresh = P.fromSource . Ninja.refresh

-- | Increases the 'Status.dur' of 'Ninja.statuses' with matching 'Status.name'.
-- Uses 'Ninja.prolong' internally.
prolong :: ∀ m. MonadPlay m => Turns -> Text -> m ()
prolong (Duration -> dur) name = do
    user    <- P.user
    copying <- Skill.copying <$> P.skill
    P.toTarget $ Ninja.prolong (Copy.maxDur copying $ sync dur) name user

-- | Reduces the 'Status.dur' of 'Ninja.statuses' with matching 'Status.name'.
-- Uses 'Ninja.prolong' internally.
hasten :: ∀ m. MonadPlay m => Turns -> Text -> m ()
hasten (Duration -> dur) name =
    P.user >>= P.toTarget . Ninja.prolong (negate $ sync dur) name

-- | Adds a 'Face.Face' to the 'Ninja.face' of a 'Ninja.Ninja', changing their
-- in-game icon.
setFace :: ∀ m. MonadPlay m => Turns -> m ()
setFace (Duration -> dur) = do
    skill <- P.skill
    let copying = Skill.copying skill
    case copying of
        Copy.NotCopied -> do
            user <- P.user
            let face = Face.Face { Face.icon   = Skill.name skill
                                 , Face.user = user
                                 , Face.dur    = sync dur
                                 }
            P.toTarget \n -> n { Ninja.face = face : Ninja.face n }
        _ -> return ()

-- | Adds a 'Status.Status' to 'Ninja.statuses'.
apply :: ∀ m. (MonadPlay m, MonadRandom m) => Turns -> [Effect] -> m ()
apply = apply' ""
-- | 'apply' with a 'Status.name'.
apply' :: ∀ m. (MonadPlay m, MonadRandom m) => Text -> Turns -> [Effect] -> m ()
apply' = applyWith' []
-- | 'apply' with extra 'Status.classes'.
applyWith :: ∀ m. (MonadPlay m, MonadRandom m) => [Class] -> Turns -> [Effect]
          -> m ()
applyWith classes = applyWith' classes ""
-- | 'applyWith' with a 'Status.name'.
applyWith' :: ∀ m. (MonadPlay m, MonadRandom m) => [Class] -> Text -> Turns
           -> [Effect] -> m ()
applyWith' classes = applyFull classes False []

-- | Adds a simple 'Status.Status' with no 'Status.effects' or 'Status.dur' to
-- 'Ninja.statuses'. Stacks are unremovable.
addStack :: ∀ m. MonadPlay m => m ()
addStack = do
    name <- Skill.name <$> P.skill
    addStacks name 1

-- | 'addStack' with a 'Status.name' and 'Status.amount'.
addStacks :: ∀ m. MonadPlay m => Text -> Int -> m ()
addStacks = addStacks' 0

-- | 'addStack' with a 'Status.dur', 'Status.name', and 'Status.amount'.
addStacks' :: ∀ m. MonadPlay m => Turns -> Text -> Int -> m ()
addStacks' _    _ 0   = return ()
addStacks' (Duration -> dur) name i = do
    skill  <- P.skill
    user   <- P.user
    target <- P.target
    let st  = Status.new user dur skill
    P.modify . Game.adjust target $ Ninja.addStatus
        st { Status.name   = name
           , Status.amount = i
           , Status.user   = user
           , Status.classes = Unremovable : Status.classes st
           }

-- | Adds a hidden 'Status.Status' with no effects that immediately expires.
flag :: ∀ m. (MonadPlay m, MonadRandom m) => m ()
flag = flag' ""
-- | 'flag' with a 'Status.name'.
flag' :: ∀ m. (MonadPlay m, MonadRandom m) => Text -> m ()
flag' name = applyWith' [Hidden, Unremovable, Nonstacking] name (-1) []

-- | Applies a 'Status.Status' with no effects, used as a marker for other
-- 'Skill.Skill's.
tag :: ∀ m. (MonadPlay m, MonadRandom m) => Turns -> m ()
tag = tag' ""
-- | 'tag' with a 'Status.name'.
tag' :: ∀ m. (MonadPlay m, MonadRandom m) => Text -> Turns -> m ()
tag' name dur = applyWith' [Unremovable, Nonstacking] name dur []

-- | Applies a 'Hidden' and 'Unremovable' 'Status.Status'.
hide :: ∀ m. (MonadPlay m, MonadRandom m) => Turns -> [Effect] -> m ()
hide = hide' ""
-- | 'hide' with a 'Status.name'.
hide' :: ∀ m. (MonadPlay m, MonadRandom m) => Text -> Turns -> [Effect] -> m ()
hide' = applyWith' [Unremovable, Hidden]

-- Adds a 'Status.Status' with 'Status.bombs' to 'Ninja.statuses'.
-- Bombs apply an effect when the 'Status.Status' ends. If the 'Bomb' type is
-- 'Status.Expire', the bomb activates when the 'Status.Status' naturally reaches the
-- end of its 'Status.dur'. If the 'Bomb' type is 'Status.Remove', the bomb
-- activates when the 'Status.Status' is removd before naturally reaching the end of
-- its 'Status.dur'. If the 'Bomb' type is 'Status.Done', the bomb activates
-- in both situations.
bomb :: ∀ m. (MonadPlay m, MonadRandom m) => Turns -> [Effect] -> [(Bomb, Play ())]
     -> m ()
bomb = bomb' ""
-- | 'bomb' with a 'Status.name'.
bomb' :: ∀ m. (MonadPlay m, MonadRandom m) => Text -> Turns -> [Effect]
      -> [(Bomb, Play ())] -> m ()
bomb' = bombWith' []
-- | 'bomb' with extra 'Status.classes'.
bombWith :: ∀ m. (MonadPlay m, MonadRandom m) => [Class] -> Turns -> [Effect]
         -> [(Bomb, Play ())] -> m ()
bombWith classes = bombWith' classes ""
-- | 'bombWith' with a 'Status.name'.
bombWith' :: ∀ m. (MonadPlay m, MonadRandom m) => [Class] -> Text -> Turns
          -> [Effect] -> [(Bomb, Play ())] -> m ()
bombWith' classes name dur fs bombs =
    applyFull classes False bombs name dur fs

immuneEffects :: [Effect]
immuneEffects = (Invulnerable <$> enumerate) ++ (Invincible <$> enumerate)
stunEffects :: [Effect]
stunEffects = Stun <$> enumerate

-- | Status engine.
applyFull :: ∀ m. (MonadPlay m, MonadRandom m) => [Class] -> Bool
          -> [(Bomb, Play ())] -> Text -> Turns -> [Effect] -> m ()
applyFull classes bounced bombs name turns@(Duration -> unthrottled) fs =
    void $ runMaybeT do
        skill       <- P.skill
        user        <- P.user
        target      <- P.target
        nUser       <- P.nUser
        nTarget     <- P.nTarget
        dur         <- MaybeT . return $ Duration.throttle
                       (Effects.throttle fs nUser) unthrottled
        let already  = Ninja.has name user nTarget
            isSingle = name == Skill.name skill && Single ∈ classes
            self     = user == user && user == target
            noremove = self && any (not . Effect.helpful) fs
                       || turns == 0
                       || turns == 1 && Skill.channel skill /= Instant
                       || null fs && Bane ∉ Skill.classes skill
            extra    = fst <$> filter snd
                       [ (Soulbound,   any bind fs)
                       , (Unremovable, noremove)
                       ]
            classes' = delete Resource . nub $
                       extra ++ classes ++ Skill.classes skill
            silenced = Ninja.is Silence nUser
            filt
              | silenced && bounced = const []
              | silenced            = filter isDmg
              | bounced             = filter (not . isDmg)
              | otherwise           = id
            newSt = Status.new user dur skill
            st    = newSt
                  { Status.name    = Skill.defaultName name skill
                  , Status.user    = user
                  , Status.effects = filt $ Adjust.apply nTarget fs
                  , Status.classes = delete Resource . nub $
                                     extra ++ classes ++ Skill.classes skill
                  , Status.bombs   = guard (Status.dur newSt <= incr (sync dur))
                                     >> bombs
                  }
            prolong' = mapMaybe $
                       Ninja.prolong' (Status.dur st) name (Status.source st)
        guard . not $ already && (bounced || isSingle)
        if already && Extending ∈ classes' then
            P.modify $ Game.adjust target \n ->
                n { Ninja.statuses = prolong' $ Ninja.statuses n }
        else do
            guard $ null fs || not (null $ Status.effects st)
            let
              onImmune n
                | Status.effects st `intersects` immuneEffects =
                    n { Ninja.triggers = insertSet OnImmune $ Ninja.triggers n }
                | otherwise = n
            P.modify . Game.adjust target $ onImmune . Ninja.addStatus st
            when (Status.effects st `intersects` stunEffects) do
                P.trigger user OnStun
                P.trigger target OnStunned

            lift . ActionChannel.onInterrupts $ any (∈ Status.effects st) .
                   (Stun <$>) . Skill.classes . Channel.skill
            when (bounced && not self) do
                let bounce t = P.withTarget t $
                               applyFull [] True (Status.bombs st) name
                               turns fs
                lift . traverse_ bounce . delete user $
                       Effects.share nTarget
  where
    bind Redirect{}   = True
    bind _            = False
    isDmg (Afflict x) = x > 0
    isDmg _           = False

-- | Removes non-'Effect.helpful' effects in 'Ninja.statuses' that match a
-- predicate. Uses 'Ninja.cure' internally.
cure :: ∀ m. MonadPlay m => (Effect -> Bool) -> m ()
cure match = P.toTarget $ Ninja.cure match

-- | Removes all non-'Effect.helpful' 'effects in 'Ninja.statuses'.
-- Uses 'Ninja.cure' internally.
cureAll :: ∀ m. MonadPlay m => m ()
cureAll = cure $ const True

-- | Removes all 'Ninja.statuses' with 'Bane' in their 'Status.classes'.
-- Uses 'Ninja.cureBane' internally.
cureBane :: ∀ m. MonadPlay m => m ()
cureBane = P.toTarget Ninja.cureBane

-- | Cures all 'Stun' effects from 'Ninja.statuses'.
-- Uses 'Ninja.cure' internally.
cureStun :: ∀ m. MonadPlay m => m ()
cureStun = cure cured
  where
    cured Stun{} = True
    cured _      = False

-- | Cures all 'Effect.helpful' effects from 'Ninja.statuses'.
-- Uses 'Ninja.purge' internally.
purge :: ∀ m. MonadPlay m => m ()
purge = P.toTarget Ninja.purge

-- | Removes all 'Status.Status'es with matching 'Status.name' and whose
-- 'Status.user' is the one performing the action.
-- Uses 'Ninja.clear' internally.
remove :: ∀ m. MonadPlay m => Text -> m ()
remove = P.fromSource . Ninja.clear

-- | Decreases the 'Status.amount' of a 'Status.Status' with matching 'Status.name' by
-- 1, removing it if it reaches 0. Uses 'Ninja.removeStack' internally.
removeStack :: ∀ m. MonadPlay m => Text -> m ()
removeStack = P.toTarget . Ninja.removeStack

-- | Decreases the 'Status.amount' of a 'Status.Status' with matching 'Status.name' and
-- whose 'Status.user is the one performing the action by some amount,
-- removing it if it reaches 0. Uses 'Ninja.removeStack' internally.
removeStacks :: ∀ m. MonadPlay m => Text -> Int -> m ()
removeStacks name = P.fromSource . Ninja.removeStacks name

-- | Saves the target's state to their 'Ninja.statuses' in a 'Snapshot'.
snapshot :: ∀ m. MonadPlay m => Duration -> m ()
snapshot dur = do
    skill   <- P.skill
    user    <- P.user
    target  <- P.target
    nTarget <- P.nTarget
    let st   = Status.new user dur skill
    P.modify . Game.adjust target $ Ninja.addStatus
        st { Status.user    = user
           , Status.effects = [Snapshot nTarget]
           , Status.classes = Nonstacking : Unremovable : Status.classes st
           }

-- | Steals all of the target's 'Effect.helpful' 'Effect's.
commandeer :: ∀ m. MonadPlay m => m ()
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
