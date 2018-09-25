module Database.Functions where

import StandardLibrary
import Data.String.CodeUnits as CodeUnits

import Database.Structure

par :: Int -> Int
par = flip mod 2

allSlots :: Array Int
allSlots = 0 .. 5
teamSlots :: Array Int
teamSlots = filter ((0 == _) <<< par) allSlots

allied :: Slot -> Slot -> Boolean
allied x y = par x == par y

unχ :: Chakras -> Array String
unχ (Chakras x) = replicate x.blood "blood"
               <> replicate x.gen   "gen"
               <> replicate x.nin   "nin"
               <> replicate x.tai   "tai"
               <> replicate x.rand  "rand"

χSum :: Chakras -> Int
χSum (Chakras x) = x.blood + x.gen + x.nin + x.tai

lacks :: Chakras -> Boolean
lacks (Chakras x) = 
    x.blood < 0 || x.gen < 0 || x.nin < 0 || x.tai < 0 || x.rand < 0

living :: Int -> Game -> Int
living p (Game g) = sum $ health <$> g.gameNinjas
  where 
    health (Ninja n)
      | par n.nId == p = min 1 n.nHealth 
      | otherwise      = 0

removable :: Boolean -> SkillEffect -> Boolean
removable onAlly (SkillEffect ef) = not ef.effectSticky 
                                 && onAlly /= ef.effectHelpful

shorten :: String -> String
shorten = memoize $ 
          CodeUnits.fromCharArray <<< map shorten' <<<
          filter (_ `notElem` [' ','-',':','(',')','®','.','/','?', '\'']) <<< 
          CodeUnits.toCharArray
  where shorten' 'ō' = 'o'
        shorten' 'Ō' = 'O'
        shorten' 'ū' = 'u'
        shorten' 'Ū' = 'U'
        shorten' 'ä' = 'a'
        shorten' a   = a

skillDur :: Skill -> String
skillDur (Skill s) = case s.channel of
    Instant     -> "Instant"
    Passive     -> "Instant"
    (Action 0)  -> "Action"
    (Control 0) -> "Control"
    (Ongoing 0) -> "Ongoing"
    (Action x)  -> "Action " <> show x
    (Control x) -> "Control " <> show x
    (Ongoing x) -> "Ongoing " <> show x

skillIcon :: Skill -> String
skillIcon (Skill s) = shorten s.label <> if s.skPic then "*" else ""

skillRoot :: Skill -> Int -> Int
skillRoot (Skill s) nId = case s.copying of
    NotCopied   -> nId
    Shallow a _ -> a
    Deep a    _ -> a


skillTarget :: Int -> Skill -> Array Int
skillTarget = memoize go
  where
    go c (Skill s) = case unit of
        _| enemy && ally  -> allSlots
        _| enemy && xally -> delete c allSlots
        _| enemy         -> (_ + 1 - par c) <$> teamSlots
        _| ally          -> (_ + par c) <$> teamSlots
        _| xally         -> delete c $ (_ + par c) <$> teamSlots
        _| otherwise     -> [c]
      where targets = untuple <$> (s.start <> s.effects)
            enemy   = Enemy `elem` targets
            ally    = Ally  `elem` targets
            xally   = XAlly `elem` targets

filterClasses :: Boolean -> Array String -> Array String
filterClasses = memoize \hideMore -> 
    (_ \\ hideMore ? (moreHidden <> _) $ hiddenClasses)

userLevel :: User -> Int
userLevel (User u) = u.xp / 1000

userXP :: User -> Int
userXP (User u) = u.xp / 1000

userRank :: User -> String
userRank (User u)
  | u.privilege == Normal = fromMaybe "Hokage" $ ranks !! (u.xp / 5000)
  | otherwise             = show u.privilege 
  
lMatch :: Skill -> Skill -> Boolean
lMatch = eq `on` (_.label <<< unwrap)

mergeSkills :: Character -> Ninja -> Character
mergeSkills (Character c) (Ninja n) =
    Character c { characterSkills = zipWith f c.characterSkills n.nSkills }
  where 
    f cSkills nSkill = f' <$> cSkills
      where f' cSkill | lMatch cSkill nSkill = nSkill
                      | otherwise            = cSkill

hiddenClasses :: Array String
hiddenClasses = 
    [ "NonMental"
    , "Bloodline"
    , "Genjutsu"
    , "Ninjutsu"
    , "Taijutsu"
    , "Random"
    , "Necromancy" 

    , "All"
    , "Affliction"
    , "NonAffliction" 
    , "NonMental"
    , "Nonstacking" 
    , "Multi"
    , "Extending" 
    , "Hidden"
    , "Shifted" 
    , "Unshifted"
    , "Direct" 
    , "BaseTrap"
    , "Healing"
    ]

moreHidden :: Array String
moreHidden = 
    [ "Single"
    , "Bypassing"
    , "Uncounterable"
    , "Unreflectable"
    ]

ranks :: Array String
ranks = [ "Academy Student"
         , "Genin"
         , "Chūnin"
         , "Missing-Nin"
         , "Anbu"
         , "Jōnin"
         , "Sannin"
         , "Jinchūriki"
         , "Akatsuki"
         , "Kage"
         , "Hokage"
         ]
