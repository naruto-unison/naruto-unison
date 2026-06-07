{-# LANGUAGE OverloadedLists #-}

module Handler.Play.War
  ( War(..)
  , match
  , opponent
  , today
  ) where

import ClassyPrelude hiding (tails)

import           Data.Aeson (ToJSON)
import           Data.Enum.Set (EnumSet)
import           Data.Time.LocalTime (LocalTime(LocalTime), getCurrentTimeZone, utcToLocalTime)
import           Data.List (tails)
import qualified System.Random as Random

import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character
import           Game.Model.Group (Group(..))
import           Util ((!!), intersects)

vsAll :: ∀ a. [a] -> [(a, a)]
vsAll xs = [(x,y) | x:ys <- tails xs, y <- ys]

vsEach :: [[Group]] -> [(EnumSet Group, EnumSet Group)]
vsEach xs = vsAll . (singleton <$>) =<< xs

-- | Represented as an unboxed Vector not because it is direly
-- performance-critical, but because unboxed Vectors have a neat trick of
-- converting vectors of pairs into pairs of vectors, so why not?
wars :: UVector (EnumSet Group, EnumSet Group)
wars = fromList
    $ replicate 6
        ([AlliedForces], [Akatsuki, Kabuto])
    ++ replicate 4
        ([LeafVillage], deleteSet LeafVillage [CloudVillage .. StoneVillage ])
    ++ replicate 4
        ([Eleven], [Orochimaru])
    ++ replicate 2
        ([Jinchuriki, Sage], [Anbu, Rogue])
    ++ vsAll [ [Akimichi, Nara, Yamanaka], [Sarutobi, Senju]
             , [SandClan], [Uchiha], [Uzumaki]
             ]
    ++ vsAll [[Genin], [Chunin], [Jonin], [Anbu, Sannin, Kage]]
    ++ vsEach
    [ delete LeafVillage [CloudVillage .. StoneVillage]
    , [Earth .. Yin]
    , [BloodlineUser .. TaijutsuUser]
    , delete AlliedForces [Akatsuki .. SevenSwordsmen]
    ]
{-# NOINLINE wars #-}

participant :: EnumSet Group -> Character -> Bool
participant war Character{groups} = war `intersects` groups

-- | You ever wonder why we're here?
data War = Red | Blue
           deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic)

instance Parity War where
    even Red  = True
    even Blue = False

instance ToJSON War

opponent :: War -> War
opponent Red  = Blue
opponent Blue = Red

-- | Matches both sides of a battle to sides in the war.
-- Matching fails if both teams can participate in both sides, or if either team
-- cannnot participate in either side.
match :: [Character] -> [Character] -> (EnumSet Group, EnumSet Group)
      -> Maybe War
match pTeam vsTeam war
  | team Red  pTeam && team Blue vsTeam = Just Red
  | team Blue pTeam && team Red  vsTeam = Just Blue
  | otherwise                         = Nothing
  where
    allOn side   = all . participant $ Parity.getOf side war
    team side xs = allOn side xs && not (allOn (opponent side) xs)

fromDay :: Day -> (EnumSet Group, EnumSet Group)
fromDay (ModifiedJulianDay day) = wars !! i
  where
    gen    = Random.mkStdGen $ fromInteger day + 1
    (i, _) = Random.randomR (0, length wars - 1) gen

-- | Obtains today's war as a pseudorandom choice seeded from the
-- 'localDay' of the current @LocalTime@.
today :: IO (EnumSet Group, EnumSet Group)
today = do
    LocalTime day _ <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
    return $ fromDay day
