module Model.Barrier (Barrier(..), new) where

import ClassyPrelude

import qualified Model.Context as Context
import           Model.Context (Context)
import           Model.Runnable (Runnable(To), RunConstraint)
import           Model.Internal (Barrier(..))
import qualified Model.Skill as Skill

-- | Adds a 'Barrier' with an effect that occurs when its duration
-- 'Barrier.finish'es, which is passed as an argument the 'Barrier.amount' of
-- barrier remaining, and an effect that occurs each turn 'Barrier.while' it
-- exists.
new :: Context
    -> Int -- ^ Duration.
    -> (Int -> RunConstraint ()) -- ^ Applied at end with amount remaining.
    -> RunConstraint () -- ^ Applied every turn.
    -> Int -- ^ Initial amount.
    -> Barrier
new context dur finish while amount = Barrier
    { user   = Context.user context
    , name   = Skill.name skill
    , finish = \i -> To saved $ finish i
    , while  = To saved while
    , amount = amount
    , dur    = dur
    }
  where
    saved = context { Context.new = False }
    skill = Context.skill context
