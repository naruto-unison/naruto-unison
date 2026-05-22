module Game.Model.Barrier (Barrier(..), new) where

import ClassyPrelude

import           Game.Model.Duration (Duration)
import           Game.Model.Internal (Barrier(..), Context(Context), Runnable(To), IntRunConstraint, RunConstraint)
import qualified Game.Model.Internal.Context as Context
import qualified Game.Model.Internal.Skill as Skill

-- | Adds a 'Barrier' with an effect that occurs when its duration
-- 'Barrier.finish'es, which is passed as an argument the 'Barrier.amount' of
-- barrier remaining, and an effect that occurs each turn 'Barrier.while' it
-- exists.
new :: Context
    -> Duration
    -> IntRunConstraint () -- ^ Applied at end with amount remaining.
    -> RunConstraint () -- ^ Applied every turn.
    -> Int -- ^ Initial amount.
    -> Barrier
new Context{skill, target, user} dur finish while amount = Barrier
    { user
    , name   = Skill.name skill
    , finish = \i -> To (saved False) $ finish i
    , while  = To (saved True) while
    , amount
    , dur
    }
  where
    saved continues = Context
        { new = False
        , continues
        , skill
        , target
        , user
        }
