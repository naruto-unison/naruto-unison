module Database.Structure 
  ( Slot, Class
  , Act(..), actCost
  , Barrier(..)
  , Bomb
  , Chakras(..), intχ, χf
  , Channel(..), Channeling(..), ChannelTag(..), channelingDur
  , Character(..)
  , Copied(..), Copying(..)
  , Defense(..)
  , SkillEffect(..)
  , Face(..)
  , Game(..)
  , GameInfo(..)
  , Ninja(..)
  , Requirement(..)
  , Skill(..)
  , Status(..)
  , Target(..)
  , Trap(..)
  , TrapType(..)
  , Privilege(..)
  , User(..)
  , Variant(..)
  , Untuple(..), untuple
  ) where

import StandardLibrary
import Foreign      as Foreign
import Generic      as G
import Data.Newtype as Newtype
import Data.String  as String

newtype Untuple a = Untuple a
instance _decodeUntuple_ :: G.Decode a => G.Decode (Untuple a) where
    decode x = do
        arr <- Foreign.readArray x
        case head arr of
            Nothing -> Foreign.fail $ Foreign.ForeignError "Empty array"
            Just head' -> Untuple <$> G.decode head'

untuple :: ∀ a. Untuple a -> a
untuple (Untuple x) = x

newtype Act = Act { actC     :: Int
                  , actS     :: Int
                  , actT     :: Int
                  , actSkill :: Skill
                  , actTs    :: Array Int
                  }
instance eqAct :: Eq Act where
  eq (Act x) (Act y) = x.actC == y.actC && x.actS == y.actS && x.actT == y.actT
actCost :: Act -> Chakras
actCost (Act {actSkill: Skill x}) = x.cost

newtype Barrier = Barrier { barrierAmount :: Int 
                          , barrierSrc    :: Slot
                          , barrierL      :: String
                          , barrierDur    :: Int
                          }

data Bomb = Done | Expire | Remove

newtype Chakras = Chakras { blood :: Int
                          , gen   :: Int
                          , nin   :: Int
                          , tai   :: Int
                          , rand  :: Int
                          }
derive instance eqChakras :: Eq Chakras

fChakras :: (Int -> Int -> Int) -> Chakras -> Chakras -> Chakras
fChakras f (Chakras x) (Chakras y) = Chakras { blood: f x.blood y.blood 
                                             , gen:   f x.gen   y.gen
                                             , nin:   f x.nin   y.nin
                                             , tai:   f x.tai   y.tai
                                             , rand:  f x.rand  y.rand
                                             }

intχ :: Int -> Chakras
intχ x = Chakras { blood: x, gen: x, nin: x, tai: x, rand: x }

χf :: ({ blood :: Int, gen :: Int, nin :: Int, tai :: Int, rand :: Int } 
   -> { blood :: Int, gen :: Int, nin :: Int, tai :: Int, rand :: Int })
   -> Chakras
χf = flip (Newtype.over Chakras) zero

instance _semiringChakras_ :: Semiring Chakras where
    zero = intχ 0
    one  = intχ 0
    add = fChakras (+)
    mul = fChakras (*)
instance _ringChakras_ :: Ring Chakras where
    sub = fChakras (-)

newtype Channel = Channel { channelRoot  :: Slot 
                          , channelSkill :: Skill
                          , channelT     :: Slot
                          , channelDur   :: Channeling
                          }

data Channeling = Instant
                | Passive
                | Action  Int
                | Control Int
                | Ongoing Int

channelingDur :: Channeling -> Int
channelingDur Instant     = 1
channelingDur Passive     = 1
channelingDur (Action a)  = a
channelingDur (Control a) = a
channelingDur (Ongoing a) = a

newtype ChannelTag = ChannelTag { tagRoot  :: Slot
                                , tagSrc   :: Slot
                                , tagSkill :: Skill
                                , tagGhost :: Boolean
                                , tagDur   :: Int
                                }

newtype Character = Character { characterName   :: String
                              , characterBio    :: String
                              , characterSkills :: Array (Array Skill)
                              }
instance _showCharacter_ :: Show Character where
    show (Character c) = c.characterName
instance _eqCharacter_ :: Eq Character where
    eq = eq `on` show
instance _ordCharacter_ :: Ord Character where
    compare = comparing show

newtype Copied = Copied { copiedSkill :: Skill 
                        , copiedDur   :: Int
                        }

data Copying = Shallow Slot Int | Deep Slot Int | NotCopied

type Class = String

newtype Defense = Defense { defenseAmount :: Int 
                          , defenseSrc    :: Slot
                          , defenseL      :: String
                          , defenseDur    :: Int
                          }

newtype SkillEffect = SkillEffect { effectDesc    :: String 
                                  , effectHelpful :: Boolean
                                  , effectSticky  :: Boolean
                                  , effectTrap    :: Boolean
                                  }

newtype Face = Face { faceIcon :: String
                    , faceSrc  :: Slot
                    , faceDur  :: Int
                    }

newtype Game = Game { gameChakra  :: Array Chakras
                    , gameNinjas  :: Array Ninja
                    , gamePlaying :: Int
                    , gameVictor  :: Maybe Int
                    , gameTargets :: Array (Array (Array Slot))
                    }

newtype GameInfo = GameInfo { gameVsUser     :: User
                            , gamePar        :: Int
                            , gameLeft       :: Int
                            , gameGame       :: Game
                            , gameCharacters :: Array Character
                            }

newtype Ninja = Ninja { nId        :: Slot
                      , nHealth    :: Int
                      , nName      :: String
                      , nDefense   :: Array Defense
                      , nBarrier   :: Array Barrier
                      , nStatuses  :: Array Status
                      , nCharges   :: Array Int
                      , nCooldowns :: Array Int
                      , nVariants  :: Array (Array Variant)
                      , nCopied    :: Array (Maybe Copied)
                      , nChannels  :: Array Channel
                      , nTraps     :: Array Trap
                      , nFace      :: Array Face
                      , nParrying  :: Array Skill
                      , nTags      :: Array ChannelTag
                      , nLastSkill :: Maybe Skill
                      , nTargeted  :: Boolean
                      , nSkills    :: Array Skill
                      }

data Requirement 
    = Usable
    | Unusable
    | HasI Int String
    | HasU String

newtype Skill = Skill { label   :: String
                      , desc    :: String
                      , classes :: Array Class
                      , cost    :: Chakras
                      , require :: Requirement
                      , cd      :: Int
                      , varicd  :: Boolean
                      , charges :: Int
                      , channel :: Channeling
                      , start   :: Array (Untuple Target)
                      , effects :: Array (Untuple Target)
                      , disrupt :: Array (Untuple Target)
                      , copying :: Copying
                      , skPic   :: Boolean
                      }
instance _showSkill_ :: Show Skill where
    show (Skill s) = s.label

type Slot = Int

newtype Status = Status { statusL       :: String 
                        , statusRoot    :: Slot
                        , statusSrc     :: Slot
                        , statusC       :: Slot
                        , statusSkill   :: Skill
                        , statusEfs     :: Array SkillEffect
                        , statusClasses :: Array Class
                        , statusBombs   :: Array (Untuple Target)
                        , statusMaxDur  :: Int
                        , statusDur     :: Int
                        }

data Target 
    = Self
    | Ally   
    | Allies 
    | RAlly  
    | XAlly  
    | XAllies 
    | Enemy   
    | Enemies 
    | REnemy  
    | XEnemies 
    | Everyone 
    | Specific Slot

newtype Trap = Trap { trapType    :: TrapType
                    , trapTrigger :: String
                    , trapL       :: String
                    , trapSrc     :: Slot
                    , trapClasses :: Array Class
                    , trapTrack   :: Int
                    , trapDesc    :: String
                    , trapDur     :: Int
                    }

data TrapType = TrapTo | TrapFrom | TrapPer

data Privilege = Normal | Moderator | Admin
instance _showPrivilege_ :: Show Privilege where 
    show = String.drop 1 <<< String.dropWhile (_ /= period) <<< G.genericShow
      where
        period = String.codePointFromChar '.'

newtype User = User { name       :: String
                    , avatar     :: String
                    , clan       :: Maybe String
                    , xp         :: Int
                    , wins       :: Int
                    , losses     :: Int
                    , streak     :: Int
                    , background :: Maybe String
                    , privilege  :: Privilege
                    , condense   :: Boolean
                    }

newtype Variant = Variant { variantV   :: Int 
                          , variantVCD :: Boolean
                          , variantL   :: String
                          , variantDur :: Int
                          }


-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

opts :: G.Options
opts = G.defaultOptions { unwrapSingleConstructors = true }

derive instance _0_ :: G.Generic Barrier _
instance _1_ :: G.Decode Barrier where
    decode = G.genericDecode opts
derive instance _3_ :: G.Newtype Barrier _

derive instance _10_ :: G.Generic Bomb _
instance _11_ :: G.Decode Bomb where
    decode = G.genericDecode opts

derive instance _20_ :: G.Generic Chakras _
instance _21_ :: G.Decode Chakras where
    decode = G.genericDecode opts
derive instance _22_ :: Eq Chakras
derive instance _23_ :: G.Newtype Chakras _

derive instance _30_ :: G.Generic Channel _
instance _31_ :: G.Decode Channel where
    decode = G.genericDecode opts
derive instance _33_ :: G.Newtype Channel _

derive instance _40_ :: G.Generic Channeling _
instance _41_ :: G.Decode Channeling where
    decode = G.genericDecode opts

derive instance _50_ :: G.Generic ChannelTag _
instance _51_ :: G.Decode ChannelTag where
    decode = G.genericDecode opts
derive instance _53_ :: G.Newtype ChannelTag _

derive instance _60_ :: G.Generic Character _
instance _61_ :: G.Decode Character where
    decode = G.genericDecode opts
derive instance _62_ :: G.Newtype Character _

derive instance _70_ :: G.Generic Copied _
instance _71_ :: G.Decode Copied where
    decode = G.genericDecode opts
derive instance _72_ :: G.Newtype Copied _
    
derive instance _80_ :: G.Generic Copying _
instance _81_ :: G.Decode Copying where
    decode = G.genericDecode opts

derive instance _90_ :: G.Generic Defense _
instance _91_ :: G.Decode Defense where
    decode = G.genericDecode opts
derive instance _93_ :: G.Newtype Defense _
    
derive instance _100_ :: G.Generic SkillEffect _
instance _101_ :: G.Decode SkillEffect where
    decode = G.genericDecode opts
derive instance _103_ :: G.Newtype SkillEffect _
    
derive instance _110_ :: G.Generic Face _
instance _111_ :: G.Decode Face where
    decode = G.genericDecode opts
derive instance _113_ :: G.Newtype Face _

derive instance _120_ :: G.Generic Game _
instance _121_ :: G.Decode Game where
    decode = G.genericDecode opts
derive instance _123_ :: G.Newtype Game _

derive instance _130_ :: G.Generic GameInfo _
instance _131_ :: G.Decode GameInfo where
    decode = G.genericDecode opts
derive instance _133_ :: G.Newtype GameInfo _

derive instance _140_ :: G.Generic Ninja _
instance _141_ :: G.Decode Ninja where
    decode = G.genericDecode opts
derive instance _143_ :: G.Newtype Ninja _
    
derive instance _150_ :: G.Generic Requirement _
instance _151_ :: G.Decode Requirement where
    decode = G.genericDecode opts
derive instance _152_ :: Eq Requirement
    
derive instance _160_ :: G.Generic Skill _
instance _161_ :: G.Decode Skill where
    decode = G.genericDecode opts
derive instance _163_ :: G.Newtype Skill _

derive instance _170_ :: G.Generic Target _
instance _171_ :: G.Decode Target where
    decode = G.genericDecode opts
derive instance _172_ :: Eq Target

derive instance _180_ :: G.Generic Trap _
instance _181_ :: G.Decode Trap where
    decode = G.genericDecode opts
derive instance _183_ :: G.Newtype Trap _

derive instance _190_ :: G.Generic TrapType _
instance _191_ :: G.Decode TrapType where
    decode = G.genericDecode opts

derive instance _200_ :: G.Generic Privilege _
instance _201_ :: G.Decode Privilege where
    decode = G.genericDecode opts
derive instance _202_ :: Eq Privilege

derive instance _210_ :: G.Generic User _
instance _211_ :: G.Decode User where
    decode = G.genericDecode opts
derive instance _213_ :: G.Newtype User _
    
derive instance _220_ :: G.Generic Variant _
instance _221_ :: G.Decode Variant where
    decode = G.genericDecode opts
derive instance _223_ :: G.Newtype Variant _

derive instance _230_ :: G.Generic Status _
instance _231_ :: G.Decode Status where
    decode = G.genericDecode opts
derive instance _233_ :: G.Newtype Status _

derive instance _243_ :: G.Newtype User _

derive instance _253_ :: G.Newtype Act _

derive instance _260_ :: G.Generic Character _
