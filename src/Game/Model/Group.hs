module Game.Model.Group (Group(..)) where

import ClassyPrelude

import Data.Aeson (ToJSON(..))
import Data.Enum.Set.Class (AsEnumSet(..))
import Text.Julius (ToJavascript(..))

import Class.Display (Display(..))

-- Never add @Male@ and @Female@! No battles of the sexes here, thanks.
-- | Used in Wars. Stored in 'Game.Model.Character.groups'.
data Group
    = CloudVillage
    | MistVillage
    | LeafVillage
    | RainVillage
    | SandVillage
    | SoundVillage
    | StoneVillage

    | Akatsuki
    | AlliedForces
    | Eleven
    | Orochimaru
    | SevenSwordsmen

    | Genin
    | Chunin
    | Jonin
    | Anbu
    | Sannin
    | Kage

    | Jinchuriki
    | Kabuto
    | Rogue
    | Sage
    | Sensor
    | SRank
    | TeamLeader

    | Aburame
    | Akimichi
    | Hozuki
    | Hyuga
    | Inuzuka
    | Kamizuru
    | Nara
    | SandClan
    | Sarutobi
    | Senju
    | Uchiha
    | Uzumaki
    | Yamanaka

    | Earth
    | Fire
    | Lightning
    | Water
    | Wind
    | Yang
    | Yin

    | BloodlineUser
    | GenjutsuUser
    | NinjutsuUser
    | TaijutsuUser
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

instance AsEnumSet Group where
    type EnumSetRep Group = Word64

instance ToJavascript Group where
    toJavascript = toJavascript . display'

instance ToJSON Group where
    toJSON = toJSON . display'

instance Display Group where
    display CloudVillage = "Hidden Cloud Village"
    display LeafVillage = "Hidden Leaf Village"
    display MistVillage = "Hidden Mist Village"
    display RainVillage = "Hidden Rain Village"
    display SandVillage = "Hidden Sand Village"
    display SoundVillage = "Hidden Sound Village"
    display StoneVillage = "Hidden Stone Village"

    display Akatsuki = "Akatsuki"
    display AlliedForces = "Allied Shinobi Forces"
    display Eleven = "Hidden Leaf 11"
    display Orochimaru = "Orochimaru's Faction"
    display SevenSwordsmen = "Seven Swordsmen of the Mist"

    display Genin = "Genin"
    display Chunin = "Chūnin"
    display Jonin = "Jōnin"
    display Anbu = "Anbu"
    display Sannin = "Sannin"
    display Kage = "Kage"

    display Jinchuriki = "Jinchūriki"
    display Kabuto = "Reanimated by Kabuto"
    display Rogue = "Rogue Operatives"
    display Sage = "Sages"
    display Sensor = "Sensors"
    display SRank = "S-Ranked"
    display TeamLeader = "Team Leaders"

    display Aburame = "Aburame Clan"
    display Akimichi = "Akimichi Clan"
    display Hozuki = "Hōzuki Clan"
    display Hyuga = "Hyūga Clan"
    display Inuzuka = "Inuzuka Clan"
    display Kamizuru = "Kamizuru Clan"
    display Nara = "Nara Clan"
    display SandClan = "Kazekage Clan"
    display Sarutobi = "Sarutobi Clan"
    display Senju = "Senju Clan"
    display Uchiha = "Uchiha Clan"
    display Uzumaki = "Uzumaki Clan"
    display Yamanaka = "Yamanaka Clan"

    display Earth = "Earth Style Users"
    display Fire = "Fire Style Users"
    display Lightning = "Lightning Style Users"
    display Water = "Water Style Users"
    display Wind = "Wind Style Users"
    display Yang = "Yang Style Users"
    display Yin = "Yin Style Users"

    display BloodlineUser = "Bloodline Users"
    display GenjutsuUser = "Genjutsu Users"
    display NinjutsuUser = "Ninjutsu Users"
    display TaijutsuUser = "Taijutsu Users"
