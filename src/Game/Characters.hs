-- | Collection of all 'Character's.
module Game.Characters (cs, cs', reanimatedBy) where

import StandardLibrary

import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as Text

import Calculus
import Game.Structure

import Game.Characters.Original.Exams
import Game.Characters.Original.Flashbacks
import Game.Characters.Original.Kids
import Game.Characters.Original.Leaders
import Game.Characters.Original.Organizations
import Game.Characters.Original.Teachers
import Game.Characters.Original.Versions

import Game.Characters.Shippuden.Adults
import Game.Characters.Shippuden.Akatsuki
import Game.Characters.Shippuden.Family
import Game.Characters.Shippuden.Kids
import Game.Characters.Shippuden.Organizations
import Game.Characters.Shippuden.Versions

import Game.Characters.Reanimated

-- | Database of 'Character's using 'characterName's + 'Group's as keys.
cs :: HashMap Text Character
cs = Map.fromList $ (tshow &&& id) <$> cs'

-- | Ordered database of 'Character's.
cs' :: [Character]
cs' = addClasses <$> original ++ shippuden ++ reanimated
  where
    original   = ($ Original) 
                 <$> kidCs ++ examCs ++ teacherCs ++ organizationCs ++ leaderCs 
                  ++ versionCs ++ flashbackCs
    shippuden  = ($ Shippuden)
                 <$> kidCsS ++ adultCsS ++ familyCsS ++ organizationCsS 
                  ++ akatsukiCsS ++ versionCsS
    reanimated = ($ Reanimated) <$> reanimatedCs

addClasses :: Character -> Character
addClasses c@Character{..} = c { characterSkills = doSkills <$> characterSkills }

doSkills :: NonEmpty Skill -> NonEmpty Skill
doSkills (x:|xs) = doSkill x :| (doSkill . vari <$> xs)
  where 
    vari skill = skill { varicd = varicd skill || cd x /= cd skill || diff skill }
    diff skill = label x `notElem` [label skill, tInit $ label x]
    
doSkill :: Skill -> Skill
doSkill skill@Skill{..} = skill { classes = go classes }
  where
    go = nub . (All :) . (Mental `notElem` classes ? (NonMental :))
    --Game{..}  = mockSkill skill

reanimatedBy :: Character -> [Character]
reanimatedBy c = filter match reanimated
  where
    reanimated = addClasses . ($ Reanimated) <$> reanimatedCs
    userIs     = (`Text.isInfixOf` characterName c)
    match 
      | userIs "Kabuto"     = Text.isInfixOf "by Kabuto" . characterBio
      | userIs "Orochimaru" = Text.isInfixOf "By Orochimaru" . characterBio
      | otherwise           = const True

{-
mockSkill :: Skill -> Game
mockSkill skill@Skill{..} = foldl mock mockGame $ snd <$> (start ++ effects)
  where 
    mock g f = f skill mockSlot mockSlot g mockSlot

-- | Used in testing.
mockGame :: Game
mockGame = mocked { gameMock   = True 
                  , gameNinjas = gameNinjas mocked <&> \n -> n { nHealth = 50 }
                  }
  where 
    mocked = newGame mockPlayer mockPlayer $ replicate 6 mockCharacter
    mockCharacter = Character "" "" ((newSkill:|[]):|[]) []
    mockPlayer    = fromJust $ case keyFromValues [PersistInt64 0] of
        Right key -> Just key
        _         -> Nothing
-}
