-- | Collection of all 'Character's.
module Game.Characters (cs, cs') where

import Data.HashMap.Strict (fromList, HashMap)
import Data.List
import Data.Text           (Text)

import Calculus
import Core.Unicode
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
cs ∷ HashMap Text Character
cs = fromList $ map (\c → (characterName c, c)) cs'

-- | Ordered database of 'Character's.
cs' ∷ [Character]
cs' = map addClasses
    $ kidCs ⧺ examCs ⧺ teacherCs ⧺ organizationCs ⧺ leaderCs ⧺ versionCs 
    ⧺ flashbackCs ⧺ map (mark "S") s ⧺ map (mark "R") reanimatedCsS
  where 
  s = kidCsS ⧺ adultCsS ⧺ familyCsS ⧺ organizationCsS ⧺ akatsukiCsS ⧺ versionCsS

mark ∷ Text → Character → Character
mark m c = c { characterName = characterName c ☩ " (" ☩ m ☩ ")" }

addClasses ∷ Character → Character
addClasses c@Character{..} = c { characterSkills = map doSkills characterSkills }

doSkills ∷ [Skill] → [Skill]
doSkills []               = []
doSkills [skill]          = [doSkill skill]
doSkills (skill : skills) = doSkill skill : map (doSkill ∘ v) skills
  where v skill' = skill' 
            { varicd = varicd skill' ∨ cd skill ≠ cd skill' 
                     ∨ (label skill ∉ [label skill', tInit (label skill)])
            }

doSkill ∷ Skill → Skill
doSkill skill@Skill{..} = skill { classes = g classes }
  where g         = nub ∘ (All :) ∘ unRemove ∘ nonMental
        unRemove  = classify Unremovable $ channel ≠ Instant ∨ Multi ∈ classes
        nonMental = classify NonMental   $ Mental ∉ classes
        classify cla True  = (cla :)
        classify _   False = id

