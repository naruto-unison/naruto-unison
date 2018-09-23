-- | Collection of all 'Character's.
module Game.Characters (cs, cs') where

import StandardLibrary

import qualified Data.HashMap.Strict as Map

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

-- | Database of 'Character's using 'characterName's as keys.
cs :: HashMap Text Character
cs = Map.fromList $ cs' <&> \c -> (characterName c, c)

-- | Ordered database of 'Character's.
cs' :: [Character]
cs' = addClasses 
    <$> kidCs ++ examCs ++ teacherCs ++ organizationCs ++ leaderCs ++ versionCs 
    ++ flashbackCs ++ (mark "S" <$> s) ++ (mark "R" <$> reanimatedCsS)
  where
    s = kidCsS ++ adultCsS ++ familyCsS ++ organizationCsS ++ akatsukiCsS 
      ++ versionCsS
    mark m c = c { characterName = characterName c ++ " (" ++ m ++ ")" }

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
    unRemove  = (channel /= Instant || Multi `elem` classes) ? (Unremovable :)
    nonMental = (Mental `notElem` classes) ? (NonMental :)
    go        = nub . (All :) . unRemove . nonMental
               -- . any (not . null . nTraps) gameNinjas ? (Trapping :)
               -- . any ((> 50) . nHealth)    gameNinjas ? (Healing :)
    --Game{..}  = mockSkill skill
    
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
    mocked = newGame (replicate 6 mockCharacter) mockPlayer mockPlayer
    mockCharacter = Character "" "" ((newSkill:|[]):|[]) []
    mockPlayer    = fromJust $ case keyFromValues [PersistInt64 0] of
        Right key -> Just key
        _         -> Nothing
-}
