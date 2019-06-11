module Engine.Trigger
  ( counter
  , parry
  , reapply
  , redirect
  , reflect
  , replace
  , death
  , snareTrap
  , swap
  ) where

import ClassyPrelude.Yesod hiding (Status, redirect, replace, swap)

import           Core.Util ((∈), (∉), intersects)
import qualified Class.Play as P
import           Class.Play (GameT, Play)
import           Class.Random (RandomT)
import           Model.Class (Class(..))
import           Model.Effect (Effect(..))
import qualified Model.Game as Game
import qualified Model.Channel as Channel
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import           Model.Slot (Slot)
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import qualified Model.Status as Status
import           Model.Status (Status)
import qualified Model.Trap as Trap
import           Model.Trap (Trigger(..))
import qualified Engine.Traps as Traps

-- | Trigger a 'Replace'.
-- Returns ('Status.source', 'Status.name', 'Effects.replaceTo', 'Effects.replaceDuration').
replace :: [Class] -> Ninja -> Bool -> [(Slot, Text, Int, Int)]
replace classes n harm = mapMaybe ifCopy allStatuses
  where
    allStatuses = filter (any matches . Status.effects) $ Ninja.statuses n
    matches (Replace _ cla _ noharm) = (harm || noharm) && cla ∈ classes
    matches _                        = False
    ifCopy st = [(Status.source st, Status.name st, to, dur)
                  | Replace dur _ to _ <- find matches $ Status.effects st]

-- | Trigger a 'Counter'.
counter :: [Class] -> Ninja -> Ninja -> Maybe Ninja
counter classes n nt
  | nocounter = Just nt
  | isJust . find (any matchN . Status.effects) $ Ninja.statuses nt = Just nt
  | otherwise = Ninja.drop match nt
  where
    nocounter = Uncounterable ∉ classes
                && any ((OnCounterAll ==) . Trap.trigger) (Ninja.traps n)
    matchN (CounterAll Uncounterable) = True
    matchN (CounterAll cla)           = cla ∈ classes
                                        && Uncounterable ∉ classes
    matchN _                          = False
    match (Counter Uncounterable)     = True
    match (Counter cla)               = cla ∈ classes
                                        && Uncounterable ∉ classes
    match _ = False

-- | Trigger a 'Parry'.
parry :: Skill -> Ninja -> Maybe (Ninja, Status, Play ())
parry skill n =
    [(n', st, a) | st <- find (any matchN . Status.effects) $ Ninja.statuses n
                 , ParryAll _ a <- find matchN $ Status.effects st]
    <|> [(n'', st, a) | (n'', Parry _ a, st) <- Ninja.take match n]
  where
    classes = Skill.classes skill
    n'      = n { Ninja.parrying = skill : Ninja.parrying n }
    matchN (ParryAll Uncounterable _) = True
    matchN (ParryAll cla _)           = cla ∈ classes
                                        && Uncounterable ∉ classes
    matchN _                          = False
    match (Parry Uncounterable _)     = True
    match (Parry cla _)               = cla ∈ classes
                                        && Uncounterable ∉ classes
    match _ = False

-- | Trigger a 'Reflect'.
reflect :: [Class] -> Ninja -> Ninja -> Maybe Ninja
reflect classes n nt
  | [Mental, Unreflectable] `intersects` classes                      = Nothing
  | any ((ReflectAll ∈) . Status.effects) $ Ninja.statuses nt = Just nt
  | any ((OnReflectAll ==) . Trap.trigger) $ Ninja.traps n         = Just nt
  | otherwise = Ninja.drop (Reflect ==) nt

-- | Trigger a 'SnareTrap'.
snareTrap :: Skill -> Ninja -> Maybe (Ninja, Int)
snareTrap skill n = [(n', a) | (n', SnareTrap _ a, _) <- Ninja.take match n]
  where
    match (SnareTrap cla _) = cla ∈ Skill.classes skill
    match _                 = False

reflectable :: ([Effect] -> Bool) -> [Class] -> Ninja -> Maybe Status
reflectable matches classes
  | Unreflectable ∈ classes = const Nothing
  | otherwise                    = find (matches . Status.effects) .
                                   Ninja.statuses

-- | Trigger a 'Reapply'.
reapply :: [Class] -> Ninja -> Maybe Slot
reapply classes = (Status.user <$>) . reflectable (Reapply ∈) classes

-- | Trigger a 'Redirect'.
redirect :: [Class] -> Ninja -> Maybe Slot
redirect classes = (Status.user <$>) . reflectable (any match) classes
  where
    match (Redirect cla) = cla ∈ classes
    match _              = False

-- | Trigger a 'Swap'.
swap :: [Class] -> Ninja -> Maybe Status
swap classes = reflectable (any match) classes
  where
    match (Swap cla) = cla ∈ classes
    match _          = False


-- | If the 'nHealth' of a 'Ninja' reaches 0,
-- they are either resurrected by triggering 'OnRes'
-- or they die and trigger 'OnDeath'.
-- If they die, their 'Soulbound' effects are canceled.
death :: ∀ m. (GameT m, RandomT m) => Slot -> m ()
death slot = do
    game   <- P.game
    let n   = Game.ninja slot game
        res = Traps.get (not $ Ninja.is Plague n) OnRes n
        die = Traps.get True OnDeath n
    if | Ninja.health n > 0 -> return ()
       | not $ null res     -> do
            P.modify $ Game.adjust slot \nt ->
                nt { Ninja.health = 1
                   , Ninja.traps  = filter ((OnRes /=) . Trap.trigger) $
                                    Ninja.traps nt
                   }
            Traps.direct slot res
       | otherwise -> do
            P.modify $ Game.adjust slot \nt ->
                nt { Ninja.traps = filter ((OnDeath /=) . Trap.trigger) $
                                  Ninja.traps nt }
            Traps.direct slot die
            P.modify . Game.alter $ (unres <$>)
  where
    unres n = n
        { Ninja.statuses = [st | st <- Ninja.statuses n
                               , slot /= Status.source st
                                 && slot /= Status.user st
                                 || Soulbound ∉ Status.classes st]
        , Ninja.traps    = [trap | trap <- Ninja.traps n
                                 , slot /= Trap.source trap
                                   || Soulbound ∉ Trap.classes trap]
        , Ninja.channels = filter ((slot /=) . Channel.target) $
                           Ninja.channels n
        }
