{-# OPTIONS_HADDOCK prune #-}

module Mission.Hooks.Action
  ( ActionHook
  , check
  , cure
  , damage, damageDuringStacks, damageWithStacks
  , defend
  , demolish
  , heal
  , kill, killAffected, killDuring
  , interrupt
  , use, useDuring, useDuringStacks
  ) where

import ClassyPrelude

import           Class.Parity (allied)
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import           Mission.Hooks.Util (boolean, hasFrom, toID)

type ActionHook = Text  -- ^ Skill name.
               -> Ninja -- ^ User.
               -> Ninja -- ^ Target before action.
               -> Ninja -- ^ Target after action.
               -> Int

-- | 1 if the condition holds true, otherwise 0.
check :: (Ninja -> Ninja -> Ninja -> Bool) -> ActionHook
check f _ x y z = boolean $ f x y z

-- | 1 if the user cured the target, otherwise 0.
cure :: ActionHook
cure _ user target target' = boolean
    $ allied user target
    && N.numHelpful target' > N.numHelpful target

-- | Damage received by the target after an action.
damage :: ActionHook
damage _ user target target'
  | allied user target = 0
  | otherwise          = max 0 $ N.health target - N.health target'

-- | Damage received by the target after an action while the user has some
-- number of stacks of a @Status@.
damageDuringStacks :: Text -> ActionHook
damageDuringStacks name _ user@Ninja{slot} target target'
  | allied user target                  = 0
  | N.health target' >= N.health target = 0
  | otherwise = N.numStacks (toID name slot) user

-- | Damage received by the target after an action while the target has some
-- number of stacks of a @Status@.
damageWithStacks :: Text -> ActionHook
damageWithStacks name _ user@Ninja{slot} target target'
  | allied user target                  = 0
  | N.health target' >= N.health target = 0
  | otherwise = N.numStacks (toID name slot) target

-- | 'N.defense' added to the target after an action.
defend :: ActionHook
defend name Ninja{slot} target target'
  | N.alive target = max 0 addedDefense
  | otherwise      = 0
  where
    getDefense   = N.defenseAmount $ toID name slot
    addedDefense = getDefense target' - getDefense target

-- | 'N.defense' destroyed after an action.
demolish :: ActionHook
demolish _ user target target'
  | allied user target = 0
  | otherwise          = max 0 $ N.totalDefense target - N.totalDefense target'

-- | Healing received by a target after an action.
heal :: ActionHook
heal _ user target target'
  | not (N.alive target) || not (allied user target) = 0
  | otherwise = max 0 $ N.health target' - N.health target

-- | 1 if the target died after an action, otherwise 0.
kill :: ActionHook
kill _ user target target' = boolean
    $ not (allied user target)
    && N.alive target
    && not (N.alive target')

-- | 1 if the target died after an action while affected by a @Status@,
-- otherwise 0.
killAffected :: Text -> ActionHook
killAffected name _ user target target' = boolean
    $ not (allied user target)
    && N.alive target
    && not (N.alive target')
    && hasFrom user name target

-- | 1 if the target died after an action while the user had a @Status@,
-- otherwise 0.
killDuring :: Text -> ActionHook
killDuring name _ user@Ninja{slot} target target' = boolean
    $ not (allied user target)
    && N.alive target
    && not (N.alive target')
    && N.has (toID name slot) user

-- | Number of target's 'N.channels' canceled due to an action.
interrupt :: ActionHook
interrupt _ user target target'
  | allied user target = 0
  | otherwise          = max 0 interrupted
  where
    numChannels Ninja{channels} = length channels
    interrupted = numChannels target - numChannels target'

-- Always 1.
use :: ActionHook
use _ _ _ _ = 1

-- | 1 if the action was used while the user was affected by a @Status@,
-- otherwise 0.
useDuring :: Text -> ActionHook
useDuring name _ user@Ninja{slot} _ _ = boolean
                                      $ N.has (toID name slot) user

-- | Number of user's stacks of a @Status@ after an action.
useDuringStacks :: Text -> ActionHook
useDuringStacks name _ user@Ninja{slot} _ _ = N.numStacks (toID name slot) user
