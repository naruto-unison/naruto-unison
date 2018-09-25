-- | 'ToJSON' instances that require functions from 'Game.Functions'.
module Game.JSON () where 

import StandardLibrary
import qualified Data.List as List

import Game.Structure
import Game.Functions

instance ToJSON Ninja where
    toJSON n@Ninja{..} = object
        [ "nId"        .= nId
        , "nHealth"    .= nHealth
        , "nName"      .= characterName nCharacter
        , "nDefense"   .= nDefense
        , "nBarrier"   .= nBarrier
        , "nStatuses"  .= filter ((Hidden `notElem`) . statusClasses) nStatuses
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
        , "nSkills"    .= (usable' <$> getSkills n)
        ]
      where 
        usable' skill@Skill{..} = skill { require = fulfill require }
        fulfill req@HasI{}
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
        ns          = toList gameNinjas 
        gameTargets :: [[[Slot]]]
        gameTargets = do
            n <- toList gameNinjas
            return $ do
                skill@Skill{..} <- getSkills n
                return $ (List.intersect $ skillTargets skill (nId n)) 
                         [ nId | nt@Ninja {..} <- ns, targetable skill n n nt ]
            