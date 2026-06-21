{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Reanimation
    ( reanimations
    , reanimate, reanimationStatusName
    , removeReanimation
    , reserveReanimation
    , clearReanimationReserves
    ) where

import ClassyPrelude

import           Control.Monad.Trans.Maybe (MaybeT(..), hoistMaybe)
import           Data.Enum.Set (EnumSet)
import qualified Data.Sequence as Seq

import           Class.Random (MonadRandom)
import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Class.Play (MonadGame, MonadPlay)
import qualified Class.Play as P
import qualified Class.Random as R
import qualified Game.Action as Action
import           Game.Action.Channel (cancelChannel)
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Model.Character as Character
import           Game.Model.Character (Category(..))
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import           Game.Model.Duration (Duration(..))
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Game as Game
import qualified Game.Model.ID as ID
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import           Game.Model.Runnable (RunConstraint, Runnable(To))
import           Game.Model.Skill (Skill(Skill), Target(..))
import qualified Game.Model.Skill as Skill
import           Game.Model.Status (Bomb(..))
import           Game.Model.Status (Status(Status))
import qualified Game.Model.Status as Status
import           Util ((∈), (!!), (?), lazyMapFromKeyed)

import qualified Game.Characters.Reanimated.Adults
import qualified Game.Characters.Reanimated.Kage
import qualified Game.Characters.Reanimated.Organizations

reanimationStatusName :: Text
reanimationStatusName = "$reanimate"

chooseReanimation :: ∀ p m. (Parity p, MonadGame m, MonadRandom m)
               => p -> Maybe Text -> m (Maybe Text)
chooseReanimation user mcurrent = runMaybeT do
    dna <- Parity.getOf user . Game.dna <$> P.game
    guard . not $ null dna
    i <- R.range (0, length dna - 1)
    let ident = dna !! i
    P.alterGame \g ->
        g { Game.dna = Parity.modifyOf user (adjustDNA i) g.dna }
    skill <- hoistMaybe $ reanimationMap ? ident
    return skill.name
  where
    adjustDNA i = case mcurrent of
        Just current -> Seq.update i current
        Nothing      -> Seq.deleteAt i

hiddenClasses :: EnumSet Class
hiddenClasses = [Hidden, Nonstacking, Unremovable, Atemporal]

reanimate :: ∀ m. MonadPlay m => Duration -> m ()
reanimate dur = P.uncopied do
    Context{user, skill} <- P.context
    P.modify user $ Ninjas.addStatus (Status.new user -1 skill)
        { Status.name    = "$reanimateTimer"
        , Status.classes = hiddenClasses
        , Status.bombs   = [ To Done $ doReanimation dur ]
        }

getCurrent :: Ninja -> Maybe Skill
getCurrent n = do
    skill@Skill{charges} <- find ((Reanimation ∈) . Skill.classes) n.skills
    guard $ charges == 0 || case n.charges ? Skill.key skill of
        Just charge -> charge < charges
        Nothing     -> True
    return skill

doReanimation :: ∀ m. MonadPlay m => Duration -> m ()
doReanimation dur = do
    Context{user, skill} <- P.context
    statusID <- P.createID reanimationStatusName
    mcurrent <- getCurrent <$> P.nUser
    reanimation <- chooseReanimation user $ identFromSkill <$> mcurrent
    case reanimation of
        Nothing -> do
            P.modify user $ Ninjas.clear statusID
            cancelChannel skill.name
        Just alternate ->
            P.modify user
                $ Ninjas.processSkills
                . Ninjas.addStatus (Status.new user dur skill)
                    { Status.name    = reanimationStatusName
                    , Status.classes = hiddenClasses
                    , Status.effects = [Alternate skill.name alternate]
                    }

removeReanimation :: ∀ m. MonadPlay m => m ()
removeReanimation = P.toUserFromUser Ninjas.clear reanimationStatusName

reserveReanimation :: ∀ m. MonadPlay m => m ()
reserveReanimation = void $ runMaybeT do
    current <- MaybeT $ getCurrent <$> P.nUser
    Context{user, skill} <- P.context
    removeReanimation
    let skillID = ID.from skill
    P.modify user $ Ninjas.addStatus (Status.new user Permanent current)
        { Status.classes = Reanimation `insertSet` skill.classes
        , Status.bombs   = [ To Done $ P.modify user $ Ninjas.recharge skillID
                                        | Skill.hasCharges skill ]
        }

doReserves :: ∀ m. MonadPlay m => m ()
doReserves = do
    effects <- getReserves <$> P.nUser
    unless (null effects) do
        P.toUser clearReserves
        Action.runTargeted effects

getReserves :: Ninja -> [Runnable Target]
getReserves Ninja{statuses} =
    [ effect | Status { classes = ((Reanimation ∈) -> True)
                      , skill   = Skill{effects = _:effects}
                      } <- statuses,
               effect <- effects ]

clearReanimationReserves :: RunConstraint ()
clearReanimationReserves = P.toUser clearReserves

clearReserves :: Ninja -> Ninja
clearReserves n = Ninjas.spendCharges skills n { N.statuses = nays }
  where
    (yays, nays) = partition ((Reanimation ∈) . Status.classes) n.statuses
    skills       = filter Skill.hasCharges $ Status.skill <$> yays

identFromSkill :: Skill -> Text
identFromSkill Skill{name} = Character.identFrom Reanimated
                           $ takeWhile (/= ':') name

toReanimation :: Skill -> Skill
toReanimation skill = Skill.withExtraClasses
    skill { Skill.classes = Reanimation `insertSet` skill.classes
          , Skill.effects = before : skill.effects
          }
  where
    before = To Self doReserves

reanimations :: Vector Skill
reanimations = fromList reanimationList
{-# NOINLINE reanimations #-}

reanimationMap :: HashMap Text Skill
reanimationMap = lazyMapFromKeyed (identFromSkill, id) reanimationList
{-# NOINLINE reanimationMap #-}

reanimationList :: [Skill]
reanimationList = toReanimation
    <$> Game.Characters.Reanimated.Kage.reanimations
     ++ Game.Characters.Reanimated.Adults.reanimations
     ++ Game.Characters.Reanimated.Organizations.reanimations
