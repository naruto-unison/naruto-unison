-- | 'ToJSON' instances that require functions from 'Game.Functions'.
module Game.JSON () where 

import StandardLibrary
import qualified Data.List as List

import Calculus
import Game.Structure
import Game.Functions

reduceStatus :: NonEmpty Status -> Status
reduceStatus (statuses:|[]) = statuses
reduceStatus (st:|statuses) = st { statusL = f $ statusL st }
  where 
    f = (++ (" (" ++ tshow (1 + length statuses) ++ ")"))

groupStatuses :: Status -> Status -> Bool
groupStatuses = andOn [eqs statusSrc, eqs statusC, eqs statusL, eqs statusDur]

reduceStatuses :: Ninja -> [Status]
reduceStatuses Ninja{..} = 
    uncurry (++) . partition ((nId /=) . statusSrc) . uncurry (++) .
    do2 True (map reduceStatus . groupBy groupStatuses) .
    partition ((Multi `elem`) . statusClasses) $
    filter ((Hidden `notElem`) . statusClasses) nStatuses

instance ToJSON Ninja where
    toJSON n@Ninja{..} = object
        [ "nId"        .= nId
        , "nHealth"    .= nHealth
        , "nName"      .= characterName nCharacter
        , "nDefense"   .= nDefense
        , "nBarrier"   .= nBarrier
        , "nStatuses"  .= reduceStatuses n
        , "nCharges"   .= nCharges
        , "nCooldowns" .= getCds n
        , "nVariants"  .= nVariants
        , "nCopied"    .= nCopied
        , "nChannels"  .= nChannels
        , "nTraps"     .= filter ((Hidden `notElem`) . trapClasses) nTraps
        , "nFace"      .= nFace
        , "nParrying"  .= nParrying
        , "nTags"      .= nTags
        , "nLastSkill" .= nLastSkill
        , "nTargeted"  .= nTargeted
        , "nSkills"    .= (usable' <$> getSkills n)
        ]
      where 
        usable' skill@Skill{..} = skill { require = fulfill require }
        fulfill req@(HasI _ _) 
          | matchRequire req nId n = Usable
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
      where 
        ns              = toList gameNinjas 
        gameTargets     = do
            n <- toList gameNinjas
            return $ do
                skill@Skill{..} <- getSkills n
                return $ (List.intersect $ skillTargets skill (nId n)) 
                         [ nId | nt@Ninja {..} <- ns, targetable skill n n nt ]
            