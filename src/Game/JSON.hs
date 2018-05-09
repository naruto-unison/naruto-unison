-- | 'ToJSON' instances that require functions from 'Game.Functions'.
module Game.JSON () where 

import qualified Data.Sequence as S

import GHC.List      (errorEmptyList)
import Data.Foldable
import Data.List
import Yesod         ((.=), ToJSON, toJSON, object)

import Calculus
import Core.Unicode
import Game.Structure
import Game.Functions

-- | List must be non-empty.
reduceStatus ∷ [Status] → Status
reduceStatus []              = errorEmptyList "reduceStatus"
reduceStatus [statuses]      = statuses
reduceStatus (st : statuses) = st { statusL = f $ statusL st }
  where f = (☩ (" (" ☩ tshow (1 + length statuses) ☩ ")"))

groupStatuses ∷ Status → Status → Bool
groupStatuses = f2all [eqs statusSrc, eqs statusC, eqs statusL, eqs statusDur]

reduceStatuses ∷ Ninja → [Status]
reduceStatuses Ninja{..} = uncurry (⧺) ∘ partition ((nId ≠) ∘ statusSrc)
                         ∘ uncurry (⧺)
                         ∘ do2 True (map reduceStatus ∘ groupBy groupStatuses)
                         ∘ partition ((Multi ∈) ∘ statusClasses)
                         $ filter ((Hidden ∉) ∘ statusClasses) nStatuses

instance ToJSON Ninja where
    toJSON n@Ninja{..} = object
        [ "nId"        .= nId
        , "nHealth"    .= nHealth
        , "nDefense"   .= nDefense
        , "nBarrier"   .= nBarrier
        , "nChannels"  .= nChannels
        , "nCharges"   .= nCharges
        , "nFace"      .= nFace
        , "nCopied"    .= nCopied
        , "nParrying"  .= nParrying
        , "nVariants"  .= nVariants
        , "nTags"      .= nTags
        , "nTraps"     .= S.filter ((Hidden ∉) ∘ trapClasses) nTraps
        , "nName"      .= characterName nCharacter
        , "nStatuses"  .= reduceStatuses n
        , "nCooldowns" .= getCds n
        , "nSkills"    .= map usable' (getSkills n)
        ]
      where usable' skill@Skill{..} = skill { require = fulfill require }
            fulfill req@(HasI _ _) | matchRequire req nId n = Usable
                                   | otherwise              = Unusable
            fulfill a = a

instance ToJSON Game where
    toJSON Game{..}  = object
        [ "gameChakra"  .= gameChakra
        , "gameNinjas"  .= gameNinjas
        , "gamePlaying" .= gamePlaying
        , "gameVictor"  .= gameVictor
        , "gameTargets" .= gameTargets
        ] 
      where ns          = toList gameNinjas 
            gameTargets = do
              n ← ns
              return $ do
                skill ← getSkills n
                return ∘ (skillTargets skill (nId n) ∩) ∘ map nId 
                        $ filter (targetable skill n n) ns
