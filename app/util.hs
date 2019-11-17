import ClassyPrelude

import qualified Game.Model.Character as Character
import qualified Game.Model.Skill as Skill
import           Game.Model.Skill (Skill)
import qualified Game.Characters as Characters

findSkills :: (Skill -> Bool) -> Text
findSkills f = toStrict . builderToLazy . mconcat $ drop 1 do
    character <- Characters.list
    let skills = [toBuilder $ Skill.name skill
                     | skill <- toList . join $ Character.skills character
                     , f skill]
    guard . not $ null skills
    ["\n", toBuilder $ Character.ident character, ": "]
      ++ intersperse ", " skills

not4Skills :: [Text]
not4Skills = Character.format
             <$> filter ((/= 4) . length . Character.skills) Characters.list
