module Flags exposing (Flags, decode)

import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)

{-}
import Json.Encode as E
import List.Extra  as List
import Maybe.Extra as Maybe
import Set         as Set
import Date exposing (Date)
import Dict exposing (Dict)
import Time

import StandardLibrary       exposing (..)
import Database              exposing (..)
import Database.Base         exposing (..)
import Database.CraftEssence exposing (..)
import Database.Servant      exposing (..)
import MyServant             exposing (..)
import Persist.Preferences   exposing (..)

import Class.Show as Show
-}
type alias Flags =
    { characters : List Bool
    , user       : Maybe Bool
    }

type alias Slot   = Int
type alias Class  = String
type alias Effect = String

decode : Json.Decode.Decoder Flags
decode =
    Json.Decode.map2 Flags
    (Json.Decode.field "characters" <| Json.Decode.list Json.Decode.bool)
    (Json.Decode.field "user" <| Json.Decode.maybe Json.Decode.bool)


type alias Barrier  =
   { amount: Int
   , source: Int
   , name: String
   , dur: Int
   }

jsonDecBarrier : Json.Decode.Decoder ( Barrier )
jsonDecBarrier =
   Json.Decode.succeed (\pamount psource pname pdur -> {amount = pamount, source = psource, name = pname, dur = pdur})
   |> required "amount" (Json.Decode.int)
   |> required "source" (Json.Decode.int)
   |> required "name" (Json.Decode.string)
   |> required "dur" (Json.Decode.int)

jsonEncBarrier : Barrier -> Value
jsonEncBarrier  val =
   Json.Encode.object
   [ ("amount", Json.Encode.int val.amount)
   , ("source", Json.Encode.int val.source)
   , ("name", Json.Encode.string val.name)
   , ("dur", Json.Encode.int val.dur)
   ]



type Bomb  =
    Done 
    | Expire 
    | Remove 

jsonDecBomb : Json.Decode.Decoder ( Bomb )
jsonDecBomb = 
    let jsonDecDictBomb = Dict.fromList [("Done", Done), ("Expire", Expire), ("Remove", Remove)]
    in  decodeSumUnaries "Bomb" jsonDecDictBomb

jsonEncBomb : Bomb -> Value
jsonEncBomb  val =
    case val of
        Done -> Json.Encode.string "Done"
        Expire -> Json.Encode.string "Expire"
        Remove -> Json.Encode.string "Remove"



type Category  =
    Original 
    | Shippuden 
    | Reanimated 

jsonDecCategory : Json.Decode.Decoder ( Category )
jsonDecCategory = 
    let jsonDecDictCategory = Dict.fromList [("Original", Original), ("Shippuden", Shippuden), ("Reanimated", Reanimated)]
    in  decodeSumUnaries "Category" jsonDecDictCategory

jsonEncCategory : Category -> Value
jsonEncCategory  val =
    case val of
        Original -> Json.Encode.string "Original"
        Shippuden -> Json.Encode.string "Shippuden"
        Reanimated -> Json.Encode.string "Reanimated"



type alias Chakras  =
   { blood: Int
   , gen: Int
   , nin: Int
   , tai: Int
   , rand: Int
   }

jsonDecChakras : Json.Decode.Decoder ( Chakras )
jsonDecChakras =
   Json.Decode.succeed (\pblood pgen pnin ptai prand -> {blood = pblood, gen = pgen, nin = pnin, tai = ptai, rand = prand})
   |> required "blood" (Json.Decode.int)
   |> required "gen" (Json.Decode.int)
   |> required "nin" (Json.Decode.int)
   |> required "tai" (Json.Decode.int)
   |> required "rand" (Json.Decode.int)

jsonEncChakras : Chakras -> Value
jsonEncChakras  val =
   Json.Encode.object
   [ ("blood", Json.Encode.int val.blood)
   , ("gen", Json.Encode.int val.gen)
   , ("nin", Json.Encode.int val.nin)
   , ("tai", Json.Encode.int val.tai)
   , ("rand", Json.Encode.int val.rand)
   ]



type alias Channel  =
   { root: Int
   , skill: Skill
   , target: Int
   , dur: Channeling
   }

jsonDecChannel : Json.Decode.Decoder ( Channel )
jsonDecChannel =
   Json.Decode.succeed (\proot pskill ptarget pdur -> {root = proot, skill = pskill, target = ptarget, dur = pdur})
   |> required "root" (Json.Decode.int)
   |> required "skill" (jsonDecSkill)
   |> required "target" (Json.Decode.int)
   |> required "dur" (jsonDecChanneling)

jsonEncChannel : Channel -> Value
jsonEncChannel  val =
   Json.Encode.object
   [ ("root", Json.Encode.int val.root)
   , ("skill", jsonEncSkill val.skill)
   , ("target", Json.Encode.int val.target)
   , ("dur", jsonEncChanneling val.dur)
   ]



type Channeling  =
    Instant 
    | Passive 
    | Action Int
    | Control Int
    | Ongoing Int

jsonDecChanneling : Json.Decode.Decoder ( Channeling )
jsonDecChanneling =
    let jsonDecDictChanneling = Dict.fromList
            [ ("Instant", Json.Decode.lazy (\_ -> Json.Decode.succeed Instant))
            , ("Passive", Json.Decode.lazy (\_ -> Json.Decode.succeed Passive))
            , ("Action", Json.Decode.lazy (\_ -> Json.Decode.map Action (Json.Decode.int)))
            , ("Control", Json.Decode.lazy (\_ -> Json.Decode.map Control (Json.Decode.int)))
            , ("Ongoing", Json.Decode.lazy (\_ -> Json.Decode.map Ongoing (Json.Decode.int)))
            ]
    in  decodeSumObjectWithSingleField  "Channeling" jsonDecDictChanneling

jsonEncChanneling : Channeling -> Value
jsonEncChanneling  val =
    let keyval v = case v of
                    Instant  -> ("Instant", encodeValue (Json.Encode.list identity []))
                    Passive  -> ("Passive", encodeValue (Json.Encode.list identity []))
                    Action v1 -> ("Action", encodeValue (Json.Encode.int v1))
                    Control v1 -> ("Control", encodeValue (Json.Encode.int v1))
                    Ongoing v1 -> ("Ongoing", encodeValue (Json.Encode.int v1))
    in encodeSumObjectWithSingleField keyval val



type alias ChannelTag  =
   { root: Int
   , source: Int
   , skill: Skill
   , ghost: Bool
   , dur: Int
   }

jsonDecChannelTag : Json.Decode.Decoder ( ChannelTag )
jsonDecChannelTag =
   Json.Decode.succeed (\proot psource pskill pghost pdur -> {root = proot, source = psource, skill = pskill, ghost = pghost, dur = pdur})
   |> required "root" (Json.Decode.int)
   |> required "source" (Json.Decode.int)
   |> required "skill" (jsonDecSkill)
   |> required "ghost" (Json.Decode.bool)
   |> required "dur" (Json.Decode.int)

jsonEncChannelTag : ChannelTag -> Value
jsonEncChannelTag  val =
   Json.Encode.object
   [ ("root", Json.Encode.int val.root)
   , ("source", Json.Encode.int val.source)
   , ("skill", jsonEncSkill val.skill)
   , ("ghost", Json.Encode.bool val.ghost)
   , ("dur", Json.Encode.int val.dur)
   ]



type alias Character  =
   { name: String
   , bio: String
   , skills: (List (List Skill))
   , category: Category
   }

jsonDecCharacter : Json.Decode.Decoder ( Character )
jsonDecCharacter =
   Json.Decode.succeed (\pname pbio pskills pcategory -> {name = pname, bio = pbio, skills = pskills, category = pcategory})
   |> required "name" (Json.Decode.string)
   |> required "bio" (Json.Decode.string)
   |> required "skills" (Json.Decode.list (Json.Decode.list (jsonDecSkill)))
   |> required "category" (jsonDecCategory)

jsonEncCharacter : Character -> Value
jsonEncCharacter  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("bio", Json.Encode.string val.bio)
   , ("skills", (Json.Encode.list (Json.Encode.list jsonEncSkill)) val.skills)
   , ("category", jsonEncCategory val.category)
   ]



type alias Context  =
   { skill: Skill
   , source: Int
   , user: Int
   , target: Int
   }

jsonDecContext : Json.Decode.Decoder ( Context )
jsonDecContext =
   Json.Decode.succeed (\pskill psource puser ptarget -> {skill = pskill, source = psource, user = puser, target = ptarget})
   |> required "skill" (jsonDecSkill)
   |> required "source" (Json.Decode.int)
   |> required "user" (Json.Decode.int)
   |> required "target" (Json.Decode.int)

jsonEncContext : Context -> Value
jsonEncContext  val =
   Json.Encode.object
   [ ("skill", jsonEncSkill val.skill)
   , ("source", Json.Encode.int val.source)
   , ("user", Json.Encode.int val.user)
   , ("target", Json.Encode.int val.target)
   ]



type alias Copy  =
   { skill: Skill
   , dur: Int
   }

jsonDecCopy : Json.Decode.Decoder ( Copy )
jsonDecCopy =
   Json.Decode.succeed (\pskill pdur -> {skill = pskill, dur = pdur})
   |> required "skill" (jsonDecSkill)
   |> required "dur" (Json.Decode.int)

jsonEncCopy : Copy -> Value
jsonEncCopy  val =
   Json.Encode.object
   [ ("skill", jsonEncSkill val.skill)
   , ("dur", Json.Encode.int val.dur)
   ]



type Copying  =
    Shallow Int Int
    | Deep Int Int
    | NotCopied 

jsonDecCopying : Json.Decode.Decoder ( Copying )
jsonDecCopying =
    let jsonDecDictCopying = Dict.fromList
            [ ("Shallow", Json.Decode.lazy (\_ -> Json.Decode.map2 Shallow (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.int))))
            , ("Deep", Json.Decode.lazy (\_ -> Json.Decode.map2 Deep (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.int))))
            , ("NotCopied", Json.Decode.lazy (\_ -> Json.Decode.succeed NotCopied))
            ]
    in  decodeSumObjectWithSingleField  "Copying" jsonDecDictCopying

jsonEncCopying : Copying -> Value
jsonEncCopying  val =
    let keyval v = case v of
                    Shallow v1 v2 -> ("Shallow", encodeValue (Json.Encode.list identity [Json.Encode.int v1, Json.Encode.int v2]))
                    Deep v1 v2 -> ("Deep", encodeValue (Json.Encode.list identity [Json.Encode.int v1, Json.Encode.int v2]))
                    NotCopied  -> ("NotCopied", encodeValue (Json.Encode.list identity []))
    in encodeSumObjectWithSingleField keyval val



type alias Defense  =
   { amount: Int
   , source: Int
   , name: String
   , dur: Int
   }

jsonDecDefense : Json.Decode.Decoder ( Defense )
jsonDecDefense =
   Json.Decode.succeed (\pamount psource pname pdur -> {amount = pamount, source = psource, name = pname, dur = pdur})
   |> required "amount" (Json.Decode.int)
   |> required "source" (Json.Decode.int)
   |> required "name" (Json.Decode.string)
   |> required "dur" (Json.Decode.int)

jsonEncDefense : Defense -> Value
jsonEncDefense  val =
   Json.Encode.object
   [ ("amount", Json.Encode.int val.amount)
   , ("source", Json.Encode.int val.source)
   , ("name", Json.Encode.string val.name)
   , ("dur", Json.Encode.int val.dur)
   ]



type Direction  =
    To 
    | From 
    | Per 

jsonDecDirection : Json.Decode.Decoder ( Direction )
jsonDecDirection = 
    let jsonDecDictDirection = Dict.fromList [("To", To), ("From", From), ("Per", Per)]
    in  decodeSumUnaries "Direction" jsonDecDictDirection

jsonEncDirection : Direction -> Value
jsonEncDirection  val =
    case val of
        To -> Json.Encode.string "To"
        From -> Json.Encode.string "From"
        Per -> Json.Encode.string "Per"



type alias Face  =
   { icon: String
   , source: Int
   , dur: Int
   }

jsonDecFace : Json.Decode.Decoder ( Face )
jsonDecFace =
   Json.Decode.succeed (\picon psource pdur -> {icon = picon, source = psource, dur = pdur})
   |> required "icon" (Json.Decode.string)
   |> required "source" (Json.Decode.int)
   |> required "dur" (Json.Decode.int)

jsonEncFace : Face -> Value
jsonEncFace  val =
   Json.Encode.object
   [ ("icon", Json.Encode.string val.icon)
   , ("source", Json.Encode.int val.source)
   , ("dur", Json.Encode.int val.dur)
   ]



type Requirement  =
    Usable 
    | Unusable 
    | HasI Int String
    | HasU String

jsonDecRequirement : Json.Decode.Decoder ( Requirement )
jsonDecRequirement =
    let jsonDecDictRequirement = Dict.fromList
            [ ("Usable", Json.Decode.lazy (\_ -> Json.Decode.succeed Usable))
            , ("Unusable", Json.Decode.lazy (\_ -> Json.Decode.succeed Unusable))
            , ("HasI", Json.Decode.lazy (\_ -> Json.Decode.map2 HasI (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.string))))
            , ("HasU", Json.Decode.lazy (\_ -> Json.Decode.map HasU (Json.Decode.string)))
            ]
    in  decodeSumObjectWithSingleField  "Requirement" jsonDecDictRequirement

jsonEncRequirement : Requirement -> Value
jsonEncRequirement  val =
    let keyval v = case v of
                    Usable  -> ("Usable", encodeValue (Json.Encode.list identity []))
                    Unusable  -> ("Unusable", encodeValue (Json.Encode.list identity []))
                    HasI v1 v2 -> ("HasI", encodeValue (Json.Encode.list identity [Json.Encode.int v1, Json.Encode.string v2]))
                    HasU v1 -> ("HasU", encodeValue (Json.Encode.string v1))
    in encodeSumObjectWithSingleField keyval val



type alias Skill  =
   { name: String
   , desc: String
   , require: Requirement
   , classes: (List String)
   , cost: Chakras
   , cooldown: Int
   , varicd: Bool
   , charges: Int
   , channel: Channeling
   , start: (List (Target, (Maybe Bool)))
   , effects: (List (Target, (Maybe Bool)))
   , interrupt: (List (Target, (Maybe Bool)))
   , copying: Copying
   , pic: Bool
   }

jsonDecSkill : Json.Decode.Decoder ( Skill )
jsonDecSkill =
   Json.Decode.succeed (\pname pdesc prequire pclasses pcost pcooldown pvaricd pcharges pchannel pstart peffects pinterrupt pcopying ppic -> {name = pname, desc = pdesc, require = prequire, classes = pclasses, cost = pcost, cooldown = pcooldown, varicd = pvaricd, charges = pcharges, channel = pchannel, start = pstart, effects = peffects, interrupt = pinterrupt, copying = pcopying, pic = ppic})
   |> required "name" (Json.Decode.string)
   |> required "desc" (Json.Decode.string)
   |> required "require" (jsonDecRequirement)
   |> required "classes" (Json.Decode.list (Json.Decode.string))
   |> required "cost" (jsonDecChakras)
   |> required "cooldown" (Json.Decode.int)
   |> required "varicd" (Json.Decode.bool)
   |> required "charges" (Json.Decode.int)
   |> required "channel" (jsonDecChanneling)
   |> required "start" (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecTarget)) (Json.Decode.index 1 (Json.Decode.maybe (Json.Decode.bool)))))
   |> required "effects" (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecTarget)) (Json.Decode.index 1 (Json.Decode.maybe (Json.Decode.bool)))))
   |> required "interrupt" (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecTarget)) (Json.Decode.index 1 (Json.Decode.maybe (Json.Decode.bool)))))
   |> required "copying" (jsonDecCopying)
   |> required "pic" (Json.Decode.bool)

jsonEncSkill : Skill -> Value
jsonEncSkill  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("desc", Json.Encode.string val.desc)
   , ("require", jsonEncRequirement val.require)
   , ("classes", (Json.Encode.list Json.Encode.string) val.classes)
   , ("cost", jsonEncChakras val.cost)
   , ("cooldown", Json.Encode.int val.cooldown)
   , ("varicd", Json.Encode.bool val.varicd)
   , ("charges", Json.Encode.int val.charges)
   , ("channel", jsonEncChanneling val.channel)
   , ("start", (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncTarget) t1,((maybeEncode (Json.Encode.bool))) t2])) val.start)
   , ("effects", (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncTarget) t1,((maybeEncode (Json.Encode.bool))) t2])) val.effects)
   , ("interrupt", (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncTarget) t1,((maybeEncode (Json.Encode.bool))) t2])) val.interrupt)
   , ("copying", jsonEncCopying val.copying)
   , ("pic", Json.Encode.bool val.pic)
   ]



type alias Status  =
   { amount: Int
   , name: String
   , root: Int
   , source: Int
   , user: Int
   , skill: Skill
   , effects: (List String)
   , classes: (List String)
   , bombs: (List (Bomb, (Maybe Bool)))
   , maxDur: Int
   , dur: Int
   }

jsonDecStatus : Json.Decode.Decoder ( Status )
jsonDecStatus =
   Json.Decode.succeed (\pamount pname proot psource puser pskill peffects pclasses pbombs pmaxDur pdur -> {amount = pamount, name = pname, root = proot, source = psource, user = puser, skill = pskill, effects = peffects, classes = pclasses, bombs = pbombs, maxDur = pmaxDur, dur = pdur})
   |> required "amount" (Json.Decode.int)
   |> required "name" (Json.Decode.string)
   |> required "root" (Json.Decode.int)
   |> required "source" (Json.Decode.int)
   |> required "user" (Json.Decode.int)
   |> required "skill" (jsonDecSkill)
   |> required "effects" (Json.Decode.list (Json.Decode.string))
   |> required "classes" (Json.Decode.list (Json.Decode.string))
   |> required "bombs" (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecBomb)) (Json.Decode.index 1 (Json.Decode.maybe (Json.Decode.bool)))))
   |> required "maxDur" (Json.Decode.int)
   |> required "dur" (Json.Decode.int)

jsonEncStatus : Status -> Value
jsonEncStatus  val =
   Json.Encode.object
   [ ("amount", Json.Encode.int val.amount)
   , ("name", Json.Encode.string val.name)
   , ("root", Json.Encode.int val.root)
   , ("source", Json.Encode.int val.source)
   , ("user", Json.Encode.int val.user)
   , ("skill", jsonEncSkill val.skill)
   , ("effects", (Json.Encode.list Json.Encode.string) val.effects)
   , ("classes", (Json.Encode.list Json.Encode.string) val.classes)
   , ("bombs", (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncBomb) t1,((maybeEncode (Json.Encode.bool))) t2])) val.bombs)
   , ("maxDur", Json.Encode.int val.maxDur)
   , ("dur", Json.Encode.int val.dur)
   ]



type Target  =
    Self 
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
    | Specific Int

jsonDecTarget : Json.Decode.Decoder ( Target )
jsonDecTarget =
    let jsonDecDictTarget = Dict.fromList
            [ ("Self", Json.Decode.lazy (\_ -> Json.Decode.succeed Self))
            , ("Ally", Json.Decode.lazy (\_ -> Json.Decode.succeed Ally))
            , ("Allies", Json.Decode.lazy (\_ -> Json.Decode.succeed Allies))
            , ("RAlly", Json.Decode.lazy (\_ -> Json.Decode.succeed RAlly))
            , ("XAlly", Json.Decode.lazy (\_ -> Json.Decode.succeed XAlly))
            , ("XAllies", Json.Decode.lazy (\_ -> Json.Decode.succeed XAllies))
            , ("Enemy", Json.Decode.lazy (\_ -> Json.Decode.succeed Enemy))
            , ("Enemies", Json.Decode.lazy (\_ -> Json.Decode.succeed Enemies))
            , ("REnemy", Json.Decode.lazy (\_ -> Json.Decode.succeed REnemy))
            , ("XEnemies", Json.Decode.lazy (\_ -> Json.Decode.succeed XEnemies))
            , ("Everyone", Json.Decode.lazy (\_ -> Json.Decode.succeed Everyone))
            , ("Specific", Json.Decode.lazy (\_ -> Json.Decode.map Specific (Json.Decode.int)))
            ]
    in  decodeSumObjectWithSingleField  "Target" jsonDecDictTarget

jsonEncTarget : Target -> Value
jsonEncTarget  val =
    let keyval v = case v of
                    Self  -> ("Self", encodeValue (Json.Encode.list identity []))
                    Ally  -> ("Ally", encodeValue (Json.Encode.list identity []))
                    Allies  -> ("Allies", encodeValue (Json.Encode.list identity []))
                    RAlly  -> ("RAlly", encodeValue (Json.Encode.list identity []))
                    XAlly  -> ("XAlly", encodeValue (Json.Encode.list identity []))
                    XAllies  -> ("XAllies", encodeValue (Json.Encode.list identity []))
                    Enemy  -> ("Enemy", encodeValue (Json.Encode.list identity []))
                    Enemies  -> ("Enemies", encodeValue (Json.Encode.list identity []))
                    REnemy  -> ("REnemy", encodeValue (Json.Encode.list identity []))
                    XEnemies  -> ("XEnemies", encodeValue (Json.Encode.list identity []))
                    Everyone  -> ("Everyone", encodeValue (Json.Encode.list identity []))
                    Specific v1 -> ("Specific", encodeValue (Json.Encode.int v1))
    in encodeSumObjectWithSingleField keyval val



type alias Trap  =
   { direction: Direction
   , trigger: Trigger
   , name: String
   , desc: String
   , source: Int
   , classes: (List String)
   , tracker: Int
   , dur: Int
   }

jsonDecTrap : Json.Decode.Decoder ( Trap )
jsonDecTrap =
   Json.Decode.succeed (\pdirection ptrigger pname pdesc psource pclasses ptracker pdur -> {direction = pdirection, trigger = ptrigger, name = pname, desc = pdesc, source = psource, classes = pclasses, tracker = ptracker, dur = pdur})
   |> required "direction" (jsonDecDirection)
   |> required "trigger" (jsonDecTrigger)
   |> required "name" (Json.Decode.string)
   |> required "desc" (Json.Decode.string)
   |> required "source" (Json.Decode.int)
   |> required "classes" (Json.Decode.list (Json.Decode.string))
   |> required "tracker" (Json.Decode.int)
   |> required "dur" (Json.Decode.int)

jsonEncTrap : Trap -> Value
jsonEncTrap  val =
   Json.Encode.object
   [ ("direction", jsonEncDirection val.direction)
   , ("trigger", jsonEncTrigger val.trigger)
   , ("name", Json.Encode.string val.name)
   , ("desc", Json.Encode.string val.desc)
   , ("source", Json.Encode.int val.source)
   , ("classes", (Json.Encode.list Json.Encode.string) val.classes)
   , ("tracker", Json.Encode.int val.tracker)
   , ("dur", Json.Encode.int val.dur)
   ]



type Trigger  =
    OnAction String
    | OnNoAction 
    | OnBreak String
    | OnChakra 
    | OnCounter String
    | OnCounterAll 
    | OnDamage 
    | OnDamaged String
    | OnDeath 
    | OnHarm 
    | OnNoHarm 
    | OnHarmed String
    | OnHealed 
    | PerHealed 
    | OnHelped 
    | OnImmune 
    | OnReflectAll 
    | OnRes 
    | OnStun 
    | OnStunned 
    | PerDamage 
    | PerDamaged 
    | TrackDamage 
    | TrackDamaged 

jsonDecTrigger : Json.Decode.Decoder ( Trigger )
jsonDecTrigger =
    let jsonDecDictTrigger = Dict.fromList
            [ ("OnAction", Json.Decode.lazy (\_ -> Json.Decode.map OnAction (Json.Decode.string)))
            , ("OnNoAction", Json.Decode.lazy (\_ -> Json.Decode.succeed OnNoAction))
            , ("OnBreak", Json.Decode.lazy (\_ -> Json.Decode.map OnBreak (Json.Decode.string)))
            , ("OnChakra", Json.Decode.lazy (\_ -> Json.Decode.succeed OnChakra))
            , ("OnCounter", Json.Decode.lazy (\_ -> Json.Decode.map OnCounter (Json.Decode.string)))
            , ("OnCounterAll", Json.Decode.lazy (\_ -> Json.Decode.succeed OnCounterAll))
            , ("OnDamage", Json.Decode.lazy (\_ -> Json.Decode.succeed OnDamage))
            , ("OnDamaged", Json.Decode.lazy (\_ -> Json.Decode.map OnDamaged (Json.Decode.string)))
            , ("OnDeath", Json.Decode.lazy (\_ -> Json.Decode.succeed OnDeath))
            , ("OnHarm", Json.Decode.lazy (\_ -> Json.Decode.succeed OnHarm))
            , ("OnNoHarm", Json.Decode.lazy (\_ -> Json.Decode.succeed OnNoHarm))
            , ("OnHarmed", Json.Decode.lazy (\_ -> Json.Decode.map OnHarmed (Json.Decode.string)))
            , ("OnHealed", Json.Decode.lazy (\_ -> Json.Decode.succeed OnHealed))
            , ("PerHealed", Json.Decode.lazy (\_ -> Json.Decode.succeed PerHealed))
            , ("OnHelped", Json.Decode.lazy (\_ -> Json.Decode.succeed OnHelped))
            , ("OnImmune", Json.Decode.lazy (\_ -> Json.Decode.succeed OnImmune))
            , ("OnReflectAll", Json.Decode.lazy (\_ -> Json.Decode.succeed OnReflectAll))
            , ("OnRes", Json.Decode.lazy (\_ -> Json.Decode.succeed OnRes))
            , ("OnStun", Json.Decode.lazy (\_ -> Json.Decode.succeed OnStun))
            , ("OnStunned", Json.Decode.lazy (\_ -> Json.Decode.succeed OnStunned))
            , ("PerDamage", Json.Decode.lazy (\_ -> Json.Decode.succeed PerDamage))
            , ("PerDamaged", Json.Decode.lazy (\_ -> Json.Decode.succeed PerDamaged))
            , ("TrackDamage", Json.Decode.lazy (\_ -> Json.Decode.succeed TrackDamage))
            , ("TrackDamaged", Json.Decode.lazy (\_ -> Json.Decode.succeed TrackDamaged))
            ]
    in  decodeSumObjectWithSingleField  "Trigger" jsonDecDictTrigger

jsonEncTrigger : Trigger -> Value
jsonEncTrigger  val =
    let keyval v = case v of
                    OnAction v1 -> ("OnAction", encodeValue (Json.Encode.string v1))
                    OnNoAction  -> ("OnNoAction", encodeValue (Json.Encode.list identity []))
                    OnBreak v1 -> ("OnBreak", encodeValue (Json.Encode.string v1))
                    OnChakra  -> ("OnChakra", encodeValue (Json.Encode.list identity []))
                    OnCounter v1 -> ("OnCounter", encodeValue (Json.Encode.string v1))
                    OnCounterAll  -> ("OnCounterAll", encodeValue (Json.Encode.list identity []))
                    OnDamage  -> ("OnDamage", encodeValue (Json.Encode.list identity []))
                    OnDamaged v1 -> ("OnDamaged", encodeValue (Json.Encode.string v1))
                    OnDeath  -> ("OnDeath", encodeValue (Json.Encode.list identity []))
                    OnHarm  -> ("OnHarm", encodeValue (Json.Encode.list identity []))
                    OnNoHarm  -> ("OnNoHarm", encodeValue (Json.Encode.list identity []))
                    OnHarmed v1 -> ("OnHarmed", encodeValue (Json.Encode.string v1))
                    OnHealed  -> ("OnHealed", encodeValue (Json.Encode.list identity []))
                    PerHealed  -> ("PerHealed", encodeValue (Json.Encode.list identity []))
                    OnHelped  -> ("OnHelped", encodeValue (Json.Encode.list identity []))
                    OnImmune  -> ("OnImmune", encodeValue (Json.Encode.list identity []))
                    OnReflectAll  -> ("OnReflectAll", encodeValue (Json.Encode.list identity []))
                    OnRes  -> ("OnRes", encodeValue (Json.Encode.list identity []))
                    OnStun  -> ("OnStun", encodeValue (Json.Encode.list identity []))
                    OnStunned  -> ("OnStunned", encodeValue (Json.Encode.list identity []))
                    PerDamage  -> ("PerDamage", encodeValue (Json.Encode.list identity []))
                    PerDamaged  -> ("PerDamaged", encodeValue (Json.Encode.list identity []))
                    TrackDamage  -> ("TrackDamage", encodeValue (Json.Encode.list identity []))
                    TrackDamaged  -> ("TrackDamaged", encodeValue (Json.Encode.list identity []))
    in encodeSumObjectWithSingleField keyval val



type alias Variant  =
   { variant: Int
   , ownCd: Bool
   , name: String
   , fromSkill: Bool
   , dur: Int
   }

jsonDecVariant : Json.Decode.Decoder ( Variant )
jsonDecVariant =
   Json.Decode.succeed (\pvariant pownCd pname pfromSkill pdur -> {variant = pvariant, ownCd = pownCd, name = pname, fromSkill = pfromSkill, dur = pdur})
   |> required "variant" (Json.Decode.int)
   |> required "ownCd" (Json.Decode.bool)
   |> required "name" (Json.Decode.string)
   |> required "fromSkill" (Json.Decode.bool)
   |> required "dur" (Json.Decode.int)

jsonEncVariant : Variant -> Value
jsonEncVariant  val =
   Json.Encode.object
   [ ("variant", Json.Encode.int val.variant)
   , ("ownCd", Json.Encode.bool val.ownCd)
   , ("name", Json.Encode.string val.name)
   , ("fromSkill", Json.Encode.bool val.fromSkill)
   , ("dur", Json.Encode.int val.dur)
   ]


