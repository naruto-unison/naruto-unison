{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedLists #-}

module Handler.Play.War
  ( War(..)
  , match
  , opponent
  , today
  ) where

import ClassyPrelude

import           Data.Aeson (ToJSON)
import           Data.Enum.Set.Class (EnumSet)
import           Data.List (tails)
import qualified System.Random as Random

import           Game.Model.Character (Character)
import qualified Game.Model.Character as Character
import           Game.Model.Group (Group(..))
import           Util ((!!), intersects)

vsAll :: ∀ a. [a] -> [(a, a)]
vsAll xs = [(x,y) | x:ys <- tails xs, y <- ys]

vsEach :: [[Group]] -> [(EnumSet Group, EnumSet Group)]
vsEach xs = vs =<< xs
  where
    vs ys = vsAll $ singletonSet <$> ys

-- | Represented as an unboxed Vector not because it is direly
-- performance-critical, but because unboxed Vectors have a neat trick of
-- converting vectors of pairs into pairs of vectors, so why not?
wars :: UVector (EnumSet Group, EnumSet Group)
wars = fromList $
  replicate 6
      ([AlliedForces], [Akatsuki, Kabuto])
  ++ replicate 4
      ([LeafVillage], LeafVillage `deleteSet` [CloudVillage .. StoneVillage ])
  ++ replicate 4
      ([Eleven], [Orochimaru])
  ++ replicate 2
      ([Jinchuriki, Sage], [Anbu, Rogue])
  ++ vsAll [ [Akimichi, Nara, Yamanaka], [Sarutobi, Senju]
           , [SandClan], [Uchiha], [Uzumaki]
           ]
  ++ vsAll [[Genin], [Chunin], [Jonin], [Anbu, Sannin, Kage]]
  ++ vsEach
  [ LeafVillage `delete` [CloudVillage .. StoneVillage]
  , [Earth .. Yin]
  , [BloodlineUser .. TaijutsuUser]
  , AlliedForces `delete` [Akatsuki .. SevenSwordsmen]
  , Sage `delete` [Rogue .. TeamLeader]
  ]
{-# NOINLINE wars #-}

participant :: EnumSet Group -> Character -> Bool
participant war char = war `intersects` Character.groups char

-- | You ever wonder why we're here?
data War = Red | Blue
           deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic, ToJSON)

opponent :: War -> War
opponent Red  = Blue
opponent Blue = Red

-- | Matches both sides of a battle to sides in the war.
-- Matching fails if both teams can participate in both sides, or if either team
-- cannnot participate in either side.
match :: [Character] -> [Character] -> (EnumSet Group, EnumSet Group)
      -> Maybe War
match pTeam vsTeam (red, blue)
  | pRed && not pBlue && vsBlue  = Just Red
  | vsRed && not vsBlue && pBlue = Just Blue
  | pBlue && not pRed && vsRed   = Just Blue
  | vsBlue && not vsRed && pBlue = Just Red
  | otherwise                    = Nothing
  where
    pRed   = all (participant red)  pTeam
    pBlue  = all (participant blue) pTeam
    vsRed  = all (participant red)  vsTeam
    vsBlue = all (participant blue) vsTeam

-- TODO Is there a better way of doing this than using System.Random? Again,
-- performance isn't really the concern, but this function is the only reason
-- the project depends on the "random" package, so it could otherwise be taken
-- out.
-- | Obtains today's war. Intended to be paired with 'getCurrentTime'.
today :: UTCTime -> (EnumSet Group, EnumSet Group)
today (UTCTime (ModifiedJulianDay day) _) = wars !! i
  where
    (i, _) = Random.randomR (0, length wars) . Random.mkStdGen . (+ 1) $ fromInteger day
