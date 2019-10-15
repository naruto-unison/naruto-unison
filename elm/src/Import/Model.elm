module Import.Model exposing (..)

import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (ObjectEncoding, encodeObject, encodeValue, decodeSumObjectWithSingleField, encodeSumObjectWithSingleField, decodeSumTwoElemArray, encodeSumTwoElementArray, encodeSumTaggedObject, decodeSumUnaries, decodeSumNullaries, decodeSumNullaryOrSingleField, decodeMap, encodeMap, jsonEncDict, jsonDecDict, encodeSet, decodeSet, maybeEncode, encodeSumUntagged, required, custom, fnullable, tuple2, tuple3)
import Dict exposing (Dict)
import Set exposing (Set)

import Import.Decode exposing (decodeSumTaggedObject)

type alias Barrier  =
   { amount: Int
   , user: Int
   , name: String
   , dur: Int
   }

jsonDecBarrier : Json.Decode.Decoder ( Barrier )
jsonDecBarrier =
   Json.Decode.succeed (\pamount puser pname pdur -> {amount = pamount, user = puser, name = pname, dur = pdur})
   |> required "amount" (Json.Decode.int)
   |> required "user" (Json.Decode.int)
   |> required "name" (Json.Decode.string)
   |> required "dur" (Json.Decode.int)

jsonEncBarrier : Barrier -> Value
jsonEncBarrier  val =
   Json.Encode.object
   [ ("amount", Json.Encode.int val.amount)
   , ("user", Json.Encode.int val.user)
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
   { skill: Skill
   , target: Int
   , dur: Channeling
   }

jsonDecChannel : Json.Decode.Decoder ( Channel )
jsonDecChannel =
   Json.Decode.succeed (\pskill ptarget pdur -> {skill = pskill, target = ptarget, dur = pdur})
   |> required "skill" (jsonDecSkill)
   |> required "target" (Json.Decode.int)
   |> required "dur" (jsonDecChanneling)

jsonEncChannel : Channel -> Value
jsonEncChannel  val =
   Json.Encode.object
   [ ("skill", jsonEncSkill val.skill)
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
        jsonDecObjectSetChanneling = Set.fromList []
    in  decodeSumTaggedObject "Channeling" "tag" "contents" jsonDecDictChanneling jsonDecObjectSetChanneling

jsonEncChanneling : Channeling -> Value
jsonEncChanneling  val =
    let keyval v = case v of
                    Instant  -> ("Instant", encodeValue (Json.Encode.list identity []))
                    Passive  -> ("Passive", encodeValue (Json.Encode.list identity []))
                    Action v1 -> ("Action", encodeValue (Json.Encode.int v1))
                    Control v1 -> ("Control", encodeValue (Json.Encode.int v1))
                    Ongoing v1 -> ("Ongoing", encodeValue (Json.Encode.int v1))
    in encodeSumTaggedObject "tag" "contents" keyval val



type alias Character  =
   { name: String
   , bio: String
   , skills: (List (List Skill))
   , price: Int
   , category: Category
   }

jsonDecCharacter : Json.Decode.Decoder ( Character )
jsonDecCharacter =
   Json.Decode.succeed (\pname pbio pskills pprice pcategory -> {name = pname, bio = pbio, skills = pskills, price = pprice, category = pcategory})
   |> required "name" (Json.Decode.string)
   |> required "bio" (Json.Decode.string)
   |> required "skills" (Json.Decode.list (Json.Decode.list (jsonDecSkill)))
   |> required "price" (Json.Decode.int)
   |> required "category" (jsonDecCategory)

jsonEncCharacter : Character -> Value
jsonEncCharacter  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("bio", Json.Encode.string val.bio)
   , ("skills", (Json.Encode.list (Json.Encode.list jsonEncSkill)) val.skills)
   , ("price", Json.Encode.int val.price)
   , ("category", jsonEncCategory val.category)
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
        jsonDecObjectSetCopying = Set.fromList []
    in  decodeSumTaggedObject "Copying" "tag" "contents" jsonDecDictCopying jsonDecObjectSetCopying

jsonEncCopying : Copying -> Value
jsonEncCopying  val =
    let keyval v = case v of
                    Shallow v1 v2 -> ("Shallow", encodeValue (Json.Encode.list identity [Json.Encode.int v1, Json.Encode.int v2]))
                    Deep v1 v2 -> ("Deep", encodeValue (Json.Encode.list identity [Json.Encode.int v1, Json.Encode.int v2]))
                    NotCopied  -> ("NotCopied", encodeValue (Json.Encode.list identity []))
    in encodeSumTaggedObject "tag" "contents" keyval val



type alias Defense  =
   { amount: Int
   , user: Int
   , name: String
   , dur: Int
   }

jsonDecDefense : Json.Decode.Decoder ( Defense )
jsonDecDefense =
   Json.Decode.succeed (\pamount puser pname pdur -> {amount = pamount, user = puser, name = pname, dur = pdur})
   |> required "amount" (Json.Decode.int)
   |> required "user" (Json.Decode.int)
   |> required "name" (Json.Decode.string)
   |> required "dur" (Json.Decode.int)

jsonEncDefense : Defense -> Value
jsonEncDefense  val =
   Json.Encode.object
   [ ("amount", Json.Encode.int val.amount)
   , ("user", Json.Encode.int val.user)
   , ("name", Json.Encode.string val.name)
   , ("dur", Json.Encode.int val.dur)
   ]



type Direction  =
    Toward
    | From
    | Per

jsonDecDirection : Json.Decode.Decoder ( Direction )
jsonDecDirection =
    let jsonDecDictDirection = Dict.fromList [("Toward", Toward), ("From", From), ("Per", Per)]
    in  decodeSumUnaries "Direction" jsonDecDictDirection

jsonEncDirection : Direction -> Value
jsonEncDirection  val =
    case val of
        Toward -> Json.Encode.string "Toward"
        From -> Json.Encode.string "From"
        Per -> Json.Encode.string "Per"



type alias Effect  =
   { desc: String
   , helpful: Bool
   , sticky: Bool
   , visible: Bool
   , trap: Bool
   }

jsonDecEffect : Json.Decode.Decoder ( Effect )
jsonDecEffect =
   Json.Decode.succeed (\pdesc phelpful psticky pvisible ptrap -> {desc = pdesc, helpful = phelpful, sticky = psticky, visible = pvisible, trap = ptrap})
   |> required "desc" (Json.Decode.string)
   |> required "helpful" (Json.Decode.bool)
   |> required "sticky" (Json.Decode.bool)
   |> required "visible" (Json.Decode.bool)
   |> required "trap" (Json.Decode.bool)

jsonEncEffect : Effect -> Value
jsonEncEffect  val =
   Json.Encode.object
   [ ("desc", Json.Encode.string val.desc)
   , ("helpful", Json.Encode.bool val.helpful)
   , ("sticky", Json.Encode.bool val.sticky)
   , ("visible", Json.Encode.bool val.visible)
   , ("trap", Json.Encode.bool val.trap)
   ]



type alias Face  =
   { icon: String
   , user: Int
   }

jsonDecFace : Json.Decode.Decoder ( Face )
jsonDecFace =
   Json.Decode.succeed (\picon puser -> {icon = picon, user = puser})
   |> required "icon" (Json.Decode.string)
   |> required "user" (Json.Decode.int)

jsonEncFace : Face -> Value
jsonEncFace  val =
   Json.Encode.object
   [ ("icon", Json.Encode.string val.icon)
   , ("user", Json.Encode.int val.user)
   ]



type Failure  =
    AlreadyQueued
    | Canceled
    | InvalidTeam
    | Locked
    | NotFound

jsonDecFailure : Json.Decode.Decoder ( Failure )
jsonDecFailure =
    let jsonDecDictFailure = Dict.fromList [("AlreadyQueued", AlreadyQueued), ("Canceled", Canceled), ("InvalidTeam", InvalidTeam), ("Locked", Locked), ("NotFound", NotFound)]
    in  decodeSumUnaries "Failure" jsonDecDictFailure

jsonEncFailure : Failure -> Value
jsonEncFailure  val =
    case val of
        AlreadyQueued -> Json.Encode.string "AlreadyQueued"
        Canceled -> Json.Encode.string "Canceled"
        InvalidTeam -> Json.Encode.string "InvalidTeam"
        Locked -> Json.Encode.string "Locked"
        NotFound -> Json.Encode.string "NotFound"



type alias GameInfo  =
   { opponent: User
   , turn: Turn
   , player: Player
   }

jsonDecGameInfo : Json.Decode.Decoder ( GameInfo )
jsonDecGameInfo =
   Json.Decode.succeed (\popponent pturn pplayer -> {opponent = popponent, turn = pturn, player = pplayer})
   |> required "opponent" (jsonDecUser)
   |> required "turn" (jsonDecTurn)
   |> required "player" (jsonDecPlayer)

jsonEncGameInfo : GameInfo -> Value
jsonEncGameInfo  val =
   Json.Encode.object
   [ ("opponent", jsonEncUser val.opponent)
   , ("turn", jsonEncTurn val.turn)
   , ("player", jsonEncPlayer val.player)
   ]



type Message  =
    Fail Failure
    | Info GameInfo
    | Ping
    | Play Turn
    | Reward (List (String, Int))

jsonDecMessage : Json.Decode.Decoder ( Message )
jsonDecMessage =
    let jsonDecDictMessage = Dict.fromList
            [ ("Fail", Json.Decode.lazy (\_ -> Json.Decode.map Fail (jsonDecFailure)))
            , ("Info", Json.Decode.lazy (\_ -> Json.Decode.map Info (jsonDecGameInfo)))
            , ("Ping", Json.Decode.lazy (\_ -> Json.Decode.succeed Ping))
            , ("Play", Json.Decode.lazy (\_ -> Json.Decode.map Play (jsonDecTurn)))
            , ("Reward", Json.Decode.lazy (\_ -> Json.Decode.map Reward (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (Json.Decode.int))))))
            ]
        jsonDecObjectSetMessage = Set.fromList []
    in  decodeSumTaggedObject "Message" "tag" "contents" jsonDecDictMessage jsonDecObjectSetMessage

jsonEncMessage : Message -> Value
jsonEncMessage  val =
    let keyval v = case v of
                    Fail v1 -> ("Fail", encodeValue (jsonEncFailure v1))
                    Info v1 -> ("Info", encodeValue (jsonEncGameInfo v1))
                    Ping  -> ("Ping", encodeValue (Json.Encode.list identity []))
                    Play v1 -> ("Play", encodeValue (jsonEncTurn v1))
                    Reward v1 -> ("Reward", encodeValue ((Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(Json.Encode.string) t1,(Json.Encode.int) t2])) v1))
    in encodeSumTaggedObject "tag" "contents" keyval val



type alias Ninja  =
   { slot: Int
   , character: String
   , health: Int
   , defense: (List Defense)
   , barrier: (List Barrier)
   , statuses: (List Status)
   , charges: (List Int)
   , cooldowns: (List Int)
   , copies: (List (Maybe Copy))
   , channels: (List Channel)
   , traps: (List Trap)
   , face: (Maybe Face)
   , lastSkill: (Maybe Skill)
   , skills: (List Skill)
   }

jsonDecNinja : Json.Decode.Decoder ( Ninja )
jsonDecNinja =
   Json.Decode.succeed (\pslot pcharacter phealth pdefense pbarrier pstatuses pcharges pcooldowns pcopies pchannels ptraps pface plastSkill pskills -> {slot = pslot, character = pcharacter, health = phealth, defense = pdefense, barrier = pbarrier, statuses = pstatuses, charges = pcharges, cooldowns = pcooldowns, copies = pcopies, channels = pchannels, traps = ptraps, face = pface, lastSkill = plastSkill, skills = pskills})
   |> required "slot" (Json.Decode.int)
   |> required "character" (Json.Decode.string)
   |> required "health" (Json.Decode.int)
   |> required "defense" (Json.Decode.list (jsonDecDefense))
   |> required "barrier" (Json.Decode.list (jsonDecBarrier))
   |> required "statuses" (Json.Decode.list (jsonDecStatus))
   |> required "charges" (Json.Decode.list (Json.Decode.int))
   |> required "cooldowns" (Json.Decode.list (Json.Decode.int))
   |> required "copies" (Json.Decode.list (Json.Decode.maybe (jsonDecCopy)))
   |> required "channels" (Json.Decode.list (jsonDecChannel))
   |> required "traps" (Json.Decode.list (jsonDecTrap))
   |> fnullable "face" (jsonDecFace)
   |> fnullable "lastSkill" (jsonDecSkill)
   |> required "skills" (Json.Decode.list (jsonDecSkill))

jsonEncNinja : Ninja -> Value
jsonEncNinja  val =
   Json.Encode.object
   [ ("slot", Json.Encode.int val.slot)
   , ("character", Json.Encode.string val.character)
   , ("health", Json.Encode.int val.health)
   , ("defense", (Json.Encode.list jsonEncDefense) val.defense)
   , ("barrier", (Json.Encode.list jsonEncBarrier) val.barrier)
   , ("statuses", (Json.Encode.list jsonEncStatus) val.statuses)
   , ("charges", (Json.Encode.list Json.Encode.int) val.charges)
   , ("cooldowns", (Json.Encode.list Json.Encode.int) val.cooldowns)
   , ("copies", (Json.Encode.list (maybeEncode (jsonEncCopy))) val.copies)
   , ("channels", (Json.Encode.list jsonEncChannel) val.channels)
   , ("traps", (Json.Encode.list jsonEncTrap) val.traps)
   , ("face", (maybeEncode (jsonEncFace)) val.face)
   , ("lastSkill", (maybeEncode (jsonEncSkill)) val.lastSkill)
   , ("skills", (Json.Encode.list jsonEncSkill) val.skills)
   ]



type alias ObjectiveProgress  =
   { character: (Maybe String)
   , desc: String
   , goal: Int
   , progress: Int
   }

jsonDecObjectiveProgress : Json.Decode.Decoder ( ObjectiveProgress )
jsonDecObjectiveProgress =
   Json.Decode.succeed (\pcharacter pdesc pgoal pprogress -> {character = pcharacter, desc = pdesc, goal = pgoal, progress = pprogress})
   |> fnullable "character" (Json.Decode.string)
   |> required "desc" (Json.Decode.string)
   |> required "goal" (Json.Decode.int)
   |> required "progress" (Json.Decode.int)

jsonEncObjectiveProgress : ObjectiveProgress -> Value
jsonEncObjectiveProgress  val =
   Json.Encode.object
   [ ("character", (maybeEncode (Json.Encode.string)) val.character)
   , ("desc", Json.Encode.string val.desc)
   , ("goal", Json.Encode.int val.goal)
   , ("progress", Json.Encode.int val.progress)
   ]



type Player  =
    A
    | B

jsonDecPlayer : Json.Decode.Decoder ( Player )
jsonDecPlayer =
    let jsonDecDictPlayer = Dict.fromList [("A", A), ("B", B)]
    in  decodeSumUnaries "Player" jsonDecDictPlayer

jsonEncPlayer : Player -> Value
jsonEncPlayer  val =
    case val of
        A -> Json.Encode.string "A"
        B -> Json.Encode.string "B"



type Privilege  =
    Normal
    | Moderator
    | Admin

jsonDecPrivilege : Json.Decode.Decoder ( Privilege )
jsonDecPrivilege =
    let jsonDecDictPrivilege = Dict.fromList [("Normal", Normal), ("Moderator", Moderator), ("Admin", Admin)]
    in  decodeSumUnaries "Privilege" jsonDecDictPrivilege

jsonEncPrivilege : Privilege -> Value
jsonEncPrivilege  val =
    case val of
        Normal -> Json.Encode.string "Normal"
        Moderator -> Json.Encode.string "Moderator"
        Admin -> Json.Encode.string "Admin"



type Requirement  =
    Usable
    | Unusable
    | HasI Int String
    | HasU Int String
    | DefenseI Int String

jsonDecRequirement : Json.Decode.Decoder ( Requirement )
jsonDecRequirement =
    let jsonDecDictRequirement = Dict.fromList
            [ ("Usable", Json.Decode.lazy (\_ -> Json.Decode.succeed Usable))
            , ("Unusable", Json.Decode.lazy (\_ -> Json.Decode.succeed Unusable))
            , ("HasI", Json.Decode.lazy (\_ -> Json.Decode.map2 HasI (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.string))))
            , ("HasU", Json.Decode.lazy (\_ -> Json.Decode.map2 HasU (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.string))))
            , ("DefenseI", Json.Decode.lazy (\_ -> Json.Decode.map2 DefenseI (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.string))))
            ]
        jsonDecObjectSetRequirement = Set.fromList []
    in  decodeSumTaggedObject "Requirement" "tag" "contents" jsonDecDictRequirement jsonDecObjectSetRequirement

jsonEncRequirement : Requirement -> Value
jsonEncRequirement  val =
    let keyval v = case v of
                    Usable  -> ("Usable", encodeValue (Json.Encode.list identity []))
                    Unusable  -> ("Unusable", encodeValue (Json.Encode.list identity []))
                    HasI v1 v2 -> ("HasI", encodeValue (Json.Encode.list identity [Json.Encode.int v1, Json.Encode.string v2]))
                    HasU v1 v2 -> ("HasU", encodeValue (Json.Encode.list identity [Json.Encode.int v1, Json.Encode.string v2]))
                    DefenseI v1 v2 -> ("DefenseI", encodeValue (Json.Encode.list identity [Json.Encode.int v1, Json.Encode.string v2]))
    in encodeSumTaggedObject "tag" "contents" keyval val



type alias Skill  =
   { name: String
   , desc: String
   , require: Requirement
   , classes: (Set String)
   , cost: Chakras
   , cooldown: Int
   , varicd: Bool
   , charges: Int
   , dur: Channeling
   , start: (List Target)
   , effects: (List Target)
   , stunned: (List Target)
   , interrupt: (List Target)
   , copying: Copying
   }

jsonDecSkill : Json.Decode.Decoder ( Skill )
jsonDecSkill =
   Json.Decode.succeed (\pname pdesc prequire pclasses pcost pcooldown pvaricd pcharges pdur pstart peffects pstunned pinterrupt pcopying -> {name = pname, desc = pdesc, require = prequire, classes = pclasses, cost = pcost, cooldown = pcooldown, varicd = pvaricd, charges = pcharges, dur = pdur, start = pstart, effects = peffects, stunned = pstunned, interrupt = pinterrupt, copying = pcopying})
   |> required "name" (Json.Decode.string)
   |> required "desc" (Json.Decode.string)
   |> required "require" (jsonDecRequirement)
   |> required "classes" (decodeSet (Json.Decode.string))
   |> required "cost" (jsonDecChakras)
   |> required "cooldown" (Json.Decode.int)
   |> required "varicd" (Json.Decode.bool)
   |> required "charges" (Json.Decode.int)
   |> required "dur" (jsonDecChanneling)
   |> required "start" (Json.Decode.list (jsonDecTarget))
   |> required "effects" (Json.Decode.list (jsonDecTarget))
   |> required "stunned" (Json.Decode.list (jsonDecTarget))
   |> required "interrupt" (Json.Decode.list (jsonDecTarget))
   |> required "copying" (jsonDecCopying)

jsonEncSkill : Skill -> Value
jsonEncSkill  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("desc", Json.Encode.string val.desc)
   , ("require", jsonEncRequirement val.require)
   , ("classes", (encodeSet Json.Encode.string) val.classes)
   , ("cost", jsonEncChakras val.cost)
   , ("cooldown", Json.Encode.int val.cooldown)
   , ("varicd", Json.Encode.bool val.varicd)
   , ("charges", Json.Encode.int val.charges)
   , ("dur", jsonEncChanneling val.dur)
   , ("start", (Json.Encode.list jsonEncTarget) val.start)
   , ("effects", (Json.Encode.list jsonEncTarget) val.effects)
   , ("stunned", (Json.Encode.list jsonEncTarget) val.stunned)
   , ("interrupt", (Json.Encode.list jsonEncTarget) val.interrupt)
   , ("copying", jsonEncCopying val.copying)
   ]



type alias Status  =
   { amount: Int
   , name: String
   , user: Int
   , skill: Skill
   , effects: (List Effect)
   , classes: (Set String)
   , bombs: (List Bomb)
   , maxDur: Int
   , dur: Int
   }

jsonDecStatus : Json.Decode.Decoder ( Status )
jsonDecStatus =
   Json.Decode.succeed (\pamount pname puser pskill peffects pclasses pbombs pmaxDur pdur -> {amount = pamount, name = pname, user = puser, skill = pskill, effects = peffects, classes = pclasses, bombs = pbombs, maxDur = pmaxDur, dur = pdur})
   |> required "amount" (Json.Decode.int)
   |> required "name" (Json.Decode.string)
   |> required "user" (Json.Decode.int)
   |> required "skill" (jsonDecSkill)
   |> required "effects" (Json.Decode.list (jsonDecEffect))
   |> required "classes" (decodeSet (Json.Decode.string))
   |> required "bombs" (Json.Decode.list (jsonDecBomb))
   |> required "maxDur" (Json.Decode.int)
   |> required "dur" (Json.Decode.int)

jsonEncStatus : Status -> Value
jsonEncStatus  val =
   Json.Encode.object
   [ ("amount", Json.Encode.int val.amount)
   , ("name", Json.Encode.string val.name)
   , ("user", Json.Encode.int val.user)
   , ("skill", jsonEncSkill val.skill)
   , ("effects", (Json.Encode.list jsonEncEffect) val.effects)
   , ("classes", (encodeSet Json.Encode.string) val.classes)
   , ("bombs", (Json.Encode.list jsonEncBomb) val.bombs)
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

jsonDecTarget : Json.Decode.Decoder ( Target )
jsonDecTarget =
    let jsonDecDictTarget = Dict.fromList [("Self", Self), ("Ally", Ally), ("Allies", Allies), ("RAlly", RAlly), ("XAlly", XAlly), ("XAllies", XAllies), ("Enemy", Enemy), ("Enemies", Enemies), ("REnemy", REnemy), ("XEnemies", XEnemies), ("Everyone", Everyone)]
    in  decodeSumUnaries "Target" jsonDecDictTarget

jsonEncTarget : Target -> Value
jsonEncTarget  val =
    case val of
        Self -> Json.Encode.string "Self"
        Ally -> Json.Encode.string "Ally"
        Allies -> Json.Encode.string "Allies"
        RAlly -> Json.Encode.string "RAlly"
        XAlly -> Json.Encode.string "XAlly"
        XAllies -> Json.Encode.string "XAllies"
        Enemy -> Json.Encode.string "Enemy"
        Enemies -> Json.Encode.string "Enemies"
        REnemy -> Json.Encode.string "REnemy"
        XEnemies -> Json.Encode.string "XEnemies"
        Everyone -> Json.Encode.string "Everyone"



type alias Trap  =
   { direction: Direction
   , trigger: String
   , name: String
   , skill: Skill
   , user: Int
   , classes: (Set String)
   , tracker: Int
   , dur: Int
   }

jsonDecTrap : Json.Decode.Decoder ( Trap )
jsonDecTrap =
   Json.Decode.succeed (\pdirection ptrigger pname pskill puser pclasses ptracker pdur -> {direction = pdirection, trigger = ptrigger, name = pname, skill = pskill, user = puser, classes = pclasses, tracker = ptracker, dur = pdur})
   |> required "direction" (jsonDecDirection)
   |> required "trigger" (Json.Decode.string)
   |> required "name" (Json.Decode.string)
   |> required "skill" (jsonDecSkill)
   |> required "user" (Json.Decode.int)
   |> required "classes" (decodeSet (Json.Decode.string))
   |> required "tracker" (Json.Decode.int)
   |> required "dur" (Json.Decode.int)

jsonEncTrap : Trap -> Value
jsonEncTrap  val =
   Json.Encode.object
   [ ("direction", jsonEncDirection val.direction)
   , ("trigger", Json.Encode.string val.trigger)
   , ("name", Json.Encode.string val.name)
   , ("skill", jsonEncSkill val.skill)
   , ("user", Json.Encode.int val.user)
   , ("classes", (encodeSet Json.Encode.string) val.classes)
   , ("tracker", Json.Encode.int val.tracker)
   , ("dur", Json.Encode.int val.dur)
   ]



type alias Turn  =
   { chakra: Chakras
   , playing: Player
   , victor: (List Player)
   , ninjas: (List Ninja)
   , targets: (List (List (List Int)))
   }

jsonDecTurn : Json.Decode.Decoder ( Turn )
jsonDecTurn =
   Json.Decode.succeed (\pchakra pplaying pvictor pninjas ptargets -> {chakra = pchakra, playing = pplaying, victor = pvictor, ninjas = pninjas, targets = ptargets})
   |> required "chakra" (jsonDecChakras)
   |> required "playing" (jsonDecPlayer)
   |> required "victor" (Json.Decode.list (jsonDecPlayer))
   |> required "ninjas" (Json.Decode.list (jsonDecNinja))
   |> required "targets" (Json.Decode.list (Json.Decode.list (Json.Decode.list (Json.Decode.int))))

jsonEncTurn : Turn -> Value
jsonEncTurn  val =
   Json.Encode.object
   [ ("chakra", jsonEncChakras val.chakra)
   , ("playing", jsonEncPlayer val.playing)
   , ("victor", (Json.Encode.list jsonEncPlayer) val.victor)
   , ("ninjas", (Json.Encode.list jsonEncNinja) val.ninjas)
   , ("targets", (Json.Encode.list (Json.Encode.list (Json.Encode.list Json.Encode.int))) val.targets)
   ]



type alias User  =
   { privilege: Privilege
   , name: String
   , avatar: String
   , background: (Maybe String)
   , xp: Int
   , wins: Int
   , losses: Int
   , streak: Int
   , record: Int
   , clan: (Maybe String)
   , muted: Bool
   , condense: Bool
   , dna: Int
   }

jsonDecUser : Json.Decode.Decoder ( User )
jsonDecUser =
   Json.Decode.succeed (\pprivilege pname pavatar pbackground pxp pwins plosses pstreak precord pclan pmuted pcondense pdna -> {privilege = pprivilege, name = pname, avatar = pavatar, background = pbackground, xp = pxp, wins = pwins, losses = plosses, streak = pstreak, record = precord, clan = pclan, muted = pmuted, condense = pcondense, dna = pdna})
   |> required "privilege" (jsonDecPrivilege)
   |> required "name" (Json.Decode.string)
   |> required "avatar" (Json.Decode.string)
   |> fnullable "background" (Json.Decode.string)
   |> required "xp" (Json.Decode.int)
   |> required "wins" (Json.Decode.int)
   |> required "losses" (Json.Decode.int)
   |> required "streak" (Json.Decode.int)
   |> required "record" (Json.Decode.int)
   |> fnullable "clan" (Json.Decode.string)
   |> required "muted" (Json.Decode.bool)
   |> required "condense" (Json.Decode.bool)
   |> required "dna" (Json.Decode.int)

jsonEncUser : User -> Value
jsonEncUser  val =
   Json.Encode.object
   [ ("privilege", jsonEncPrivilege val.privilege)
   , ("name", Json.Encode.string val.name)
   , ("avatar", Json.Encode.string val.avatar)
   , ("background", (maybeEncode (Json.Encode.string)) val.background)
   , ("xp", Json.Encode.int val.xp)
   , ("wins", Json.Encode.int val.wins)
   , ("losses", Json.Encode.int val.losses)
   , ("streak", Json.Encode.int val.streak)
   , ("record", Json.Encode.int val.record)
   , ("clan", (maybeEncode (Json.Encode.string)) val.clan)
   , ("muted", Json.Encode.bool val.muted)
   , ("condense", Json.Encode.bool val.condense)
   , ("dna", Json.Encode.int val.dna)
   ]

