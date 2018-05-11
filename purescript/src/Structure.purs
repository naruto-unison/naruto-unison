module Structure where

import Prelude

import Data.Argonaut.Core          (Json)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Either                 (Either)   
import Data.Generic                (class Generic, gShow)
import Data.Maybe                  (Maybe)
import Data.Tuple                  (Tuple)

import Operators

type Dummy a = Array (Tuple a (Maybe a))

newtype Act = Act { actC     ∷ Int
                  , actS     ∷ Int
                  , actT     ∷ Int
                  , actSkill ∷ Skill
                  , actTs    ∷ Array Int
                  }
instance eqAct ∷ Eq Act where
  eq (Act a) (Act b) = a.actC ≡ b.actC ∧ a.actS ≡ b.actS ∧ a.actT ≡ b.actT
actC_ ∷ Act → Int
actC_ (Act {actC}) = actC
actCost ∷ Act → Chakras
actCost (Act {actSkill: Skill {cost}}) = cost

newtype Barrier = Barrier { barrierAmount ∷ Int 
                          , barrierSrc    ∷ Slot
                          , barrierL      ∷ String
                          , barrierDur    ∷ Int
                          }
derive instance genericBarrier ∷ Generic Barrier

data Bomb = Done | Expire | Remove
derive instance genericBomb ∷ Generic Bomb

type ChakraRecord = { blood ∷ Int
                    , gen   ∷ Int
                    , nin   ∷ Int
                    , tai   ∷ Int
                    , rand  ∷ Int
                    }

newtype Chakras = Chakras ChakraRecord
derive instance genericChakras ∷ Generic Chakras
derive instance eqChakras ∷ Eq Chakras
χØ ∷ Chakras
χØ = Chakras { blood: 0, gen: 0, nin: 0, tai: 0, rand: 0 }

newtype Channel = Channel { channelRoot  ∷ Slot 
                          , channelSkill ∷ Skill
                          , channelT     ∷ Slot
                          , channelDur   ∷ Channeling
                          }
derive instance genericChannel ∷ Generic Channel
data Channeling = Instant
                | Passive
                | Action  Int
                | Control Int
                | Ongoing Int
derive instance genericChanneling ∷ Generic Channeling

channelingDur ∷ Channeling → Int
channelingDur Instant     = 1
channelingDur Passive     = 1
channelingDur (Action a)  = a
channelingDur (Control a) = a
channelingDur (Ongoing a) = a

newtype ChannelTag = ChannelTag { tagRoot  ∷ Slot
                                , tagSrc   ∷ Slot
                                , tagSkill ∷ Skill
                                , tagGhost ∷ Boolean
                                , tagDur   ∷ Int
                                }
derive instance genericChannelTag ∷ Generic ChannelTag

newtype Character = Character { characterName   ∷ String
                              , characterBio    ∷ String
                              , characterSkills ∷ Array (Array Skill)
                              }
derive instance genericCharacter ∷ Generic Character
instance eqCharacter ∷ Eq Character where
  eq (Character a) (Character b) = eq a.characterName b.characterName
characterName_ ∷ Character → String
characterName_ (Character {characterName}) = characterName

newtype Copied = Copied { copiedSkill ∷ Skill 
                        , copiedDur   ∷ Int
                        }
derive instance genericCopied ∷ Generic Copied

data Copying = Shallow Slot Int | Deep Slot Int | NotCopied
derive instance genericCopying ∷ Generic Copying

type Class = String

newtype Defense = Defense { defenseAmount ∷ Int 
                          , defenseSrc    ∷ Slot
                          , defenseL      ∷ String
                          , defenseDur    ∷ Int
                          }
derive instance genericDefense ∷ Generic Defense

newtype Effect = Effect { effectDesc    ∷ String 
                        , effectHelpful ∷ Boolean
                        , effectSticky  ∷ Boolean
                        , effectTrap    ∷ Boolean
                        }
derive instance genericEffect ∷ Generic Effect

newtype Face = Face { faceIcon ∷ String
                    , faceSrc  ∷ Slot
                    , faceDur  ∷ Int
                    }
derive instance genericFace ∷ Generic Face

newtype Game = Game { gameChakra  ∷ Array Chakras
                    , gameNinjas  ∷ Array Ninja
                    , gamePlaying ∷ Int
                    , gameVictor  ∷ Maybe Int
                    , gameTargets ∷ Array (Array (Array Slot))
                    }
derive instance genericGame ∷ Generic Game

newtype GameInfo = GameInfo { gameVsUser     ∷ User
                            , gamePar        ∷ Int
                            , gameLeft       ∷ Int
                            , gameGame       ∷ Game
                            , gameCharacters ∷ Array Character
                            }
derive instance genericGameInfo ∷ Generic GameInfo

decodeGames ∷ Json → Either String (Array Game)
decodeGames = decodeJson

newtype Ninja = Ninja { nId        ∷ Slot
                      , nHealth    ∷ Int
                      , nDefense   ∷ Array Defense
                      , nBarrier   ∷ Array Barrier
                      , nChannels  ∷ Array Channel
                      , nCharges   ∷ Array Int
                      , nFace      ∷ Array Face
                      , nCopied    ∷ Array (Maybe Copied)
                      , nParrying  ∷ Array Skill
                      , nVariants  ∷ Array (Array Variant)
                      , nTags      ∷ Array ChannelTag
                      , nTraps     ∷ Array Trap
                      , nName      ∷ String
                      , nStatuses  ∷ Array Status
                      , nCooldowns ∷ Array Int
                      , nSkills    ∷ Array Skill
                      }
derive instance genericNinja ∷ Generic Ninja

data Requirement = Usable
                 | Unusable
                 | HasI Int String
                 | HasU String
derive instance genericRequirement ∷ Generic Requirement
derive instance eqRequirement ∷ Eq Requirement

newtype Skill = Skill { label   ∷ String
                      , desc    ∷ String
                      , classes ∷ Array Class
                      , cost    ∷ Chakras
                      , require ∷ Requirement
                      , cd      ∷ Int
                      , varicd  ∷ Boolean
                      , charges ∷ Int
                      , channel ∷ Channeling
                      , start   ∷ Dummy Target
                      , effects ∷ Dummy Target
                      , disrupt ∷ Dummy Target
                      , copying ∷ Copying
                      , skPic   ∷ Boolean
                      }
derive instance genericSkill ∷ Generic Skill
label_ ∷ Skill → String
label_ (Skill {label}) = label

type Slot = Int

newtype Status = Status { statusL       ∷ String 
                        , statusRoot    ∷ Slot
                        , statusSrc     ∷ Slot
                        , statusC       ∷ Slot
                        , statusSkill   ∷ Skill
                        , statusEfs     ∷ Array Effect
                        , statusClasses ∷ Array Class
                        , statusBombs   ∷ Dummy Bomb
                        , statusMaxDur  ∷ Int
                        , statusDur     ∷ Int
                        }
derive instance genericStatus ∷ Generic Status

data Target = Self
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
derive instance genericTarget ∷ Generic Target
derive instance eqTarget ∷ Eq Target

newtype Trap = Trap { trapType    ∷ TrapType
                    , trapTrigger ∷ String
                    , trapL       ∷ String
                    , trapSrc     ∷ Slot
                    , trapClasses ∷ Array Class
                    , trapTrack   ∷ Int
                    , trapDesc    ∷ String
                    , trapDur     ∷ Int
                    }
derive instance genericTrap ∷ Generic Trap

data TrapType = TrapTo | TrapFrom | TrapPer
derive instance genericTrapType ∷ Generic TrapType

data Privilege = Normal | Moderator | Admin
derive instance genericPrivilege ∷ Generic Privilege
derive instance eqPrivilege ∷ Eq Privilege
instance showPrivilege ∷ Show Privilege where show = gShow

newtype User = User { name       ∷ String
                    , avatar     ∷ String
                    , clan       ∷ Maybe String
                    , xp         ∷ Int
                    , wins       ∷ Int
                    , losses     ∷ Int
                    , streak     ∷ Int
                    , background ∷ Maybe String
                    , privilege  ∷ Privilege
                    }
derive instance genericUser ∷ Generic User

avatar_ ∷ User → String
avatar_ (User {avatar}) = avatar

newtype Variant = Variant { variantV   ∷ Int 
                          , variantVCD ∷ Boolean
                          , variantL   ∷ String
                          , variantDur ∷ Int
                          }
derive instance genericVariant ∷ Generic Variant
