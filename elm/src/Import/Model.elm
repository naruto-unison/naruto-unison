module Import.Model exposing (..)

import Dict exposing (Dict)
import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (..)
import Set exposing (Set)

visibleClasses : Set String
visibleClasses = Set.fromList ["Chakra", "Mental", "Physical", "Summon", "Melee", "Ranged", "Bypassing", "Invisible", "Soulbound", "Controlled", "Bane", "Necromancy", "Reanimation", "Uncounterable", "Unreflectable", "Unremovable"]

type Bomb  =
    Done
    | Expire

jsonDecBomb : Json.Decode.Decoder ( Bomb )
jsonDecBomb =
    let jsonDecDictBomb = Dict.fromList [("Done", Done), ("Expire", Expire)]
    in  decodeSumUnaries "Bomb" jsonDecDictBomb

jsonEncBomb : Bomb -> Value
jsonEncBomb  val =
    case val of
        Done -> Json.Encode.string "Done"
        Expire -> Json.Encode.string "Expire"



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
   Json.Decode.succeed Chakras
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
   , new: Bool
   , dur: Channeling
   }

jsonDecChannel : Json.Decode.Decoder ( Channel )
jsonDecChannel =
   Json.Decode.succeed Channel
   |> required "skill" (jsonDecSkill)
   |> required "target" (Json.Decode.int)
   |> required "new" (Json.Decode.bool)
   |> required "dur" (jsonDecChanneling)

jsonEncChannel : Channel -> Value
jsonEncChannel  val =
   Json.Encode.object
   [ ("skill", jsonEncSkill val.skill)
   , ("target", Json.Encode.int val.target)
   , ("new", Json.Encode.bool val.new)
   , ("dur", jsonEncChanneling val.dur)
   ]



type Channeling  =
    Instant
    | Passive
    | Action (Maybe Int)
    | Control (Maybe Int)
    | Ongoing (Maybe Int)

jsonDecChanneling : Json.Decode.Decoder ( Channeling )
jsonDecChanneling =
    let jsonDecDictChanneling = Dict.fromList
            [ ("Instant", Json.Decode.lazy (\_ -> Json.Decode.succeed Instant))
            , ("Passive", Json.Decode.lazy (\_ -> Json.Decode.succeed Passive))
            , ("Action", Json.Decode.lazy (\_ -> Json.Decode.map Action (Json.Decode.maybe (Json.Decode.int))))
            , ("Control", Json.Decode.lazy (\_ -> Json.Decode.map Control (Json.Decode.maybe (Json.Decode.int))))
            , ("Ongoing", Json.Decode.lazy (\_ -> Json.Decode.map Ongoing (Json.Decode.maybe (Json.Decode.int))))
            ]
        jsonDecObjectSetChanneling = Set.fromList ["Instant", "Passive"]
    in  decodeSumTaggedObject "Channeling" "tag" "contents" jsonDecDictChanneling jsonDecObjectSetChanneling

jsonEncChanneling : Channeling -> Value
jsonEncChanneling  val =
    let keyval v = case v of
                    Instant  -> ("Instant", encodeValue (Json.Encode.list identity []))
                    Passive  -> ("Passive", encodeValue (Json.Encode.list identity []))
                    Action v1 -> ("Action", encodeValue ((maybeEncode (Json.Encode.int)) v1))
                    Control v1 -> ("Control", encodeValue ((maybeEncode (Json.Encode.int)) v1))
                    Ongoing v1 -> ("Ongoing", encodeValue ((maybeEncode (Json.Encode.int)) v1))
    in encodeSumTaggedObject "tag" "contents" keyval val



type alias Character  =
   { name: String
   , price: Int
   , bio: String
   , groups: (Set String)
   , skills: (List (List Skill))
   , category: Category
   , ident: String
   }

jsonDecCharacter : Json.Decode.Decoder ( Character )
jsonDecCharacter =
   Json.Decode.succeed Character
   |> required "name" (Json.Decode.string)
   |> required "price" (Json.Decode.int)
   |> required "bio" (Json.Decode.string)
   |> required "groups" (decodeSet (Json.Decode.string))
   |> required "skills" (Json.Decode.list (Json.Decode.list (jsonDecSkill)))
   |> required "category" (jsonDecCategory)
   |> required "ident" (Json.Decode.string)

jsonEncCharacter : Character -> Value
jsonEncCharacter  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("price", Json.Encode.int val.price)
   , ("bio", Json.Encode.string val.bio)
   , ("groups", (encodeSet Json.Encode.string) val.groups)
   , ("skills", (Json.Encode.list (Json.Encode.list jsonEncSkill)) val.skills)
   , ("category", jsonEncCategory val.category)
   , ("ident", Json.Encode.string val.ident)
   ]



type alias Copy  =
   { skill: Skill
   , dur: (Maybe Int)
   }

jsonDecCopy : Json.Decode.Decoder ( Copy )
jsonDecCopy =
   Json.Decode.succeed Copy
   |> required "skill" (jsonDecSkill)
   |> fnullable "dur" (Json.Decode.int)

jsonEncCopy : Copy -> Value
jsonEncCopy  val =
   Json.Encode.object
   [ ("skill", jsonEncSkill val.skill)
   , ("dur", (maybeEncode (Json.Encode.int)) val.dur)
   ]



type alias Destructible  =
   { amount: Int
   , user: Int
   , skill: Skill
   , dur: (Maybe Int)
   , effects: (List Effect)
   }

jsonDecDestructible : Json.Decode.Decoder ( Destructible )
jsonDecDestructible =
   Json.Decode.succeed Destructible
   |> required "amount" (Json.Decode.int)
   |> required "user" (Json.Decode.int)
   |> required "skill" (jsonDecSkill)
   |> fnullable "dur" (Json.Decode.int)
   |> required "effects" (Json.Decode.list (jsonDecEffect))

jsonEncDestructible : Destructible -> Value
jsonEncDestructible  val =
   Json.Encode.object
   [ ("amount", Json.Encode.int val.amount)
   , ("user", Json.Encode.int val.user)
   , ("skill", jsonEncSkill val.skill)
   , ("dur", (maybeEncode (Json.Encode.int)) val.dur)
   , ("effects", (Json.Encode.list jsonEncEffect) val.effects)
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
   , slot: (Maybe Int)
   }

jsonDecEffect : Json.Decode.Decoder ( Effect )
jsonDecEffect =
   Json.Decode.succeed Effect
   |> required "desc" (Json.Decode.string)
   |> required "helpful" (Json.Decode.bool)
   |> required "sticky" (Json.Decode.bool)
   |> required "visible" (Json.Decode.bool)
   |> required "trap" (Json.Decode.bool)
   |> fnullable "slot" (Json.Decode.int)

jsonEncEffect : Effect -> Value
jsonEncEffect  val =
   Json.Encode.object
   [ ("desc", Json.Encode.string val.desc)
   , ("helpful", Json.Encode.bool val.helpful)
   , ("sticky", Json.Encode.bool val.sticky)
   , ("visible", Json.Encode.bool val.visible)
   , ("trap", Json.Encode.bool val.trap)
   , ("slot", (maybeEncode (Json.Encode.int)) val.slot)
   ]



type alias Face  =
   { icon: String
   , user: Int
   }

jsonDecFace : Json.Decode.Decoder ( Face )
jsonDecFace =
   Json.Decode.succeed Face
   |> required "icon" (Json.Decode.string)
   |> required "user" (Json.Decode.int)

jsonEncFace : Face -> Value
jsonEncFace  val =
   Json.Encode.object
   [ ("icon", Json.Encode.string val.icon)
   , ("user", Json.Encode.int val.user)
   ]



type alias GameInfo  =
   { opponent: User
   , turn: Turn
   , player: Player
   , war: (Maybe War)
   }

jsonDecGameInfo : Json.Decode.Decoder ( GameInfo )
jsonDecGameInfo =
   Json.Decode.succeed GameInfo
   |> required "opponent" (jsonDecUser)
   |> required "turn" (jsonDecTurn)
   |> required "player" (jsonDecPlayer)
   |> fnullable "war" (jsonDecWar)

jsonEncGameInfo : GameInfo -> Value
jsonEncGameInfo  val =
   Json.Encode.object
   [ ("opponent", jsonEncUser val.opponent)
   , ("turn", jsonEncTurn val.turn)
   , ("player", jsonEncPlayer val.player)
   , ("war", (maybeEncode (jsonEncWar)) val.war)
   ]



type GameMessage  =
    Play Turn
    | Rewards (List Reward)

jsonDecGameMessage : Json.Decode.Decoder ( GameMessage )
jsonDecGameMessage =
    let jsonDecDictGameMessage = Dict.fromList
            [ ("Play", Json.Decode.lazy (\_ -> Json.Decode.map Play (jsonDecTurn)))
            , ("Rewards", Json.Decode.lazy (\_ -> Json.Decode.map Rewards (Json.Decode.list (jsonDecReward))))
            ]
        jsonDecObjectSetGameMessage = Set.fromList []
    in  decodeSumTaggedObject "GameMessage" "tag" "contents" jsonDecDictGameMessage jsonDecObjectSetGameMessage

jsonEncGameMessage : GameMessage -> Value
jsonEncGameMessage  val =
    let keyval v = case v of
                    Play v1 -> ("Play", encodeValue (jsonEncTurn v1))
                    Rewards v1 -> ("Rewards", encodeValue ((Json.Encode.list jsonEncReward) v1))
    in encodeSumTaggedObject "tag" "contents" keyval val



type alias Ninja  =
   { slot: Int
   , character: String
   , health: Int
   , cooldowns: (Dict String Int)
   , charges: (Dict String Int)
   , defense: (List Destructible)
   , barrier: (List Destructible)
   , statuses: (List Status)
   , copies: (List (Maybe Copy))
   , channels: (List Channel)
   , traps: (List Trap)
   , face: (Maybe Face)
   , skills: (List Skill)
   }

jsonDecNinja : Json.Decode.Decoder ( Ninja )
jsonDecNinja =
   Json.Decode.succeed Ninja
   |> required "slot" (Json.Decode.int)
   |> required "character" (Json.Decode.string)
   |> required "health" (Json.Decode.int)
   |> required "cooldowns" (Json.Decode.dict (Json.Decode.int))
   |> required "charges" (Json.Decode.dict (Json.Decode.int))
   |> required "defense" (Json.Decode.list (jsonDecDestructible))
   |> required "barrier" (Json.Decode.list (jsonDecDestructible))
   |> required "statuses" (Json.Decode.list (jsonDecStatus))
   |> required "copies" (Json.Decode.list (Json.Decode.maybe (jsonDecCopy)))
   |> required "channels" (Json.Decode.list (jsonDecChannel))
   |> required "traps" (Json.Decode.list (jsonDecTrap))
   |> fnullable "face" (jsonDecFace)
   |> required "skills" (Json.Decode.list (jsonDecSkill))

jsonEncNinja : Ninja -> Value
jsonEncNinja  val =
   Json.Encode.object
   [ ("slot", Json.Encode.int val.slot)
   , ("character", Json.Encode.string val.character)
   , ("health", Json.Encode.int val.health)
   , ("cooldowns", (Json.Encode.dict identity (Json.Encode.int)) val.cooldowns)
   , ("charges", (Json.Encode.dict identity (Json.Encode.int)) val.charges)
   , ("defense", (Json.Encode.list jsonEncDestructible) val.defense)
   , ("barrier", (Json.Encode.list jsonEncDestructible) val.barrier)
   , ("statuses", (Json.Encode.list jsonEncStatus) val.statuses)
   , ("copies", (Json.Encode.list (maybeEncode (jsonEncCopy))) val.copies)
   , ("channels", (Json.Encode.list jsonEncChannel) val.channels)
   , ("traps", (Json.Encode.list jsonEncTrap) val.traps)
   , ("face", (maybeEncode (jsonEncFace)) val.face)
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
   Json.Decode.succeed ObjectiveProgress
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
    Guest
    | Normal
    | Moderator
    | Admin

jsonDecPrivilege : Json.Decode.Decoder ( Privilege )
jsonDecPrivilege =
    let jsonDecDictPrivilege = Dict.fromList [("Guest", Guest), ("Normal", Normal), ("Moderator", Moderator), ("Admin", Admin)]
    in  decodeSumUnaries "Privilege" jsonDecDictPrivilege

jsonEncPrivilege : Privilege -> Value
jsonEncPrivilege  val =
    case val of
        Guest -> Json.Encode.string "Guest"
        Normal -> Json.Encode.string "Normal"
        Moderator -> Json.Encode.string "Moderator"
        Admin -> Json.Encode.string "Admin"



type QueueFailure  =
    AlreadyQueued
    | Canceled
    | InvalidTeam String
    | Locked (List String)
    | NotFound
    | SocketError String

jsonDecQueueFailure : Json.Decode.Decoder ( QueueFailure )
jsonDecQueueFailure =
    let jsonDecDictQueueFailure = Dict.fromList
            [ ("AlreadyQueued", Json.Decode.lazy (\_ -> Json.Decode.succeed AlreadyQueued))
            , ("Canceled", Json.Decode.lazy (\_ -> Json.Decode.succeed Canceled))
            , ("InvalidTeam", Json.Decode.lazy (\_ -> Json.Decode.map InvalidTeam (Json.Decode.string)))
            , ("Locked", Json.Decode.lazy (\_ -> Json.Decode.map Locked (Json.Decode.list (Json.Decode.string))))
            , ("NotFound", Json.Decode.lazy (\_ -> Json.Decode.succeed NotFound))
            , ("SocketError", Json.Decode.lazy (\_ -> Json.Decode.map SocketError (Json.Decode.string)))
            ]
        jsonDecObjectSetQueueFailure = Set.fromList ["AlreadyQueued", "Canceled", "NotFound"]
    in  decodeSumTaggedObject "QueueFailure" "tag" "contents" jsonDecDictQueueFailure jsonDecObjectSetQueueFailure

jsonEncQueueFailure : QueueFailure -> Value
jsonEncQueueFailure  val =
    let keyval v = case v of
                    AlreadyQueued  -> ("AlreadyQueued", encodeValue (Json.Encode.list identity []))
                    Canceled  -> ("Canceled", encodeValue (Json.Encode.list identity []))
                    InvalidTeam v1 -> ("InvalidTeam", encodeValue (Json.Encode.string v1))
                    Locked v1 -> ("Locked", encodeValue ((Json.Encode.list Json.Encode.string) v1))
                    NotFound  -> ("NotFound", encodeValue (Json.Encode.list identity []))
                    SocketError v1 -> ("SocketError", encodeValue (Json.Encode.string v1))
    in encodeSumTaggedObject "tag" "contents" keyval val



type QueueMessage  =
    Fail QueueFailure
    | Info GameInfo
    | Ping

jsonDecQueueMessage : Json.Decode.Decoder ( QueueMessage )
jsonDecQueueMessage =
    let jsonDecDictQueueMessage = Dict.fromList
            [ ("Fail", Json.Decode.lazy (\_ -> Json.Decode.map Fail (jsonDecQueueFailure)))
            , ("Info", Json.Decode.lazy (\_ -> Json.Decode.map Info (jsonDecGameInfo)))
            , ("Ping", Json.Decode.lazy (\_ -> Json.Decode.succeed Ping))
            ]
        jsonDecObjectSetQueueMessage = Set.fromList ["Ping"]
    in  decodeSumTaggedObject "QueueMessage" "tag" "contents" jsonDecDictQueueMessage jsonDecObjectSetQueueMessage

jsonEncQueueMessage : QueueMessage -> Value
jsonEncQueueMessage  val =
    let keyval v = case v of
                    Fail v1 -> ("Fail", encodeValue (jsonEncQueueFailure v1))
                    Info v1 -> ("Info", encodeValue (jsonEncGameInfo v1))
                    Ping  -> ("Ping", encodeValue (Json.Encode.list identity []))
    in encodeSumTaggedObject "tag" "contents" keyval val



type alias Reward  =
   { reason: String
   , amount: Int
   }

jsonDecReward : Json.Decode.Decoder ( Reward )
jsonDecReward =
   Json.Decode.succeed Reward
   |> required "reason" (Json.Decode.string)
   |> required "amount" (Json.Decode.int)

jsonEncReward : Reward -> Value
jsonEncReward  val =
   Json.Encode.object
   [ ("reason", Json.Encode.string val.reason)
   , ("amount", Json.Encode.int val.amount)
   ]



type alias Skill  =
   { name: String
   , desc: String
   , classes: (Set String)
   , cost: Chakras
   , cooldown: Int
   , charges: Int
   , dur: Channeling
   , start: (List Target)
   , always: (List Target)
   , effects: (List Target)
   , end: (List Target)
   , owner: Int
   }

jsonDecSkill : Json.Decode.Decoder ( Skill )
jsonDecSkill =
   Json.Decode.succeed Skill
   |> required "name" (Json.Decode.string)
   |> required "desc" (Json.Decode.string)
   |> required "classes" (decodeSet (Json.Decode.string))
   |> required "cost" (jsonDecChakras)
   |> required "cooldown" (Json.Decode.int)
   |> required "charges" (Json.Decode.int)
   |> required "dur" (jsonDecChanneling)
   |> required "start" (Json.Decode.list (jsonDecTarget))
   |> required "always" (Json.Decode.list (jsonDecTarget))
   |> required "effects" (Json.Decode.list (jsonDecTarget))
   |> required "end" (Json.Decode.list (jsonDecTarget))
   |> required "owner" (Json.Decode.int)

jsonEncSkill : Skill -> Value
jsonEncSkill  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("desc", Json.Encode.string val.desc)
   , ("classes", (encodeSet Json.Encode.string) val.classes)
   , ("cost", jsonEncChakras val.cost)
   , ("cooldown", Json.Encode.int val.cooldown)
   , ("charges", Json.Encode.int val.charges)
   , ("dur", jsonEncChanneling val.dur)
   , ("start", (Json.Encode.list jsonEncTarget) val.start)
   , ("always", (Json.Encode.list jsonEncTarget) val.always)
   , ("effects", (Json.Encode.list jsonEncTarget) val.effects)
   , ("end", (Json.Encode.list jsonEncTarget) val.end)
   , ("owner", Json.Encode.int val.owner)
   ]



type alias Status  =
   { amount: Int
   , name: String
   , user: Int
   , skill: Skill
   , effects: (List Effect)
   , classes: (Set String)
   , bombs: (List Bomb)
   , dur: (Maybe Int)
   }

jsonDecStatus : Json.Decode.Decoder ( Status )
jsonDecStatus =
   Json.Decode.succeed Status
   |> required "amount" (Json.Decode.int)
   |> required "name" (Json.Decode.string)
   |> required "user" (Json.Decode.int)
   |> required "skill" (jsonDecSkill)
   |> required "effects" (Json.Decode.list (jsonDecEffect))
   |> required "classes" (decodeSet (Json.Decode.string))
   |> required "bombs" (Json.Decode.list (jsonDecBomb))
   |> fnullable "dur" (Json.Decode.int)

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
   , ("dur", (maybeEncode (Json.Encode.int)) val.dur)
   ]



type Target  =
    Self
    | Ally
    | Allies
    | XAlly
    | XAllies
    | RAlly
    | RXAlly
    | Enemy
    | Enemies
    | REnemy
    | XEnemies
    | Everyone

jsonDecTarget : Json.Decode.Decoder ( Target )
jsonDecTarget =
    let jsonDecDictTarget = Dict.fromList [("Self", Self), ("Ally", Ally), ("Allies", Allies), ("XAlly", XAlly), ("XAllies", XAllies), ("RAlly", RAlly), ("RXAlly", RXAlly), ("Enemy", Enemy), ("Enemies", Enemies), ("REnemy", REnemy), ("XEnemies", XEnemies), ("Everyone", Everyone)]
    in  decodeSumUnaries "Target" jsonDecDictTarget

jsonEncTarget : Target -> Value
jsonEncTarget  val =
    case val of
        Self -> Json.Encode.string "Self"
        Ally -> Json.Encode.string "Ally"
        Allies -> Json.Encode.string "Allies"
        XAlly -> Json.Encode.string "XAlly"
        XAllies -> Json.Encode.string "XAllies"
        RAlly -> Json.Encode.string "RAlly"
        RXAlly -> Json.Encode.string "RXAlly"
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
   , dur: (Maybe Int)
   }

jsonDecTrap : Json.Decode.Decoder ( Trap )
jsonDecTrap =
   Json.Decode.succeed Trap
   |> required "direction" (jsonDecDirection)
   |> required "trigger" (Json.Decode.string)
   |> required "name" (Json.Decode.string)
   |> required "skill" (jsonDecSkill)
   |> required "user" (Json.Decode.int)
   |> required "classes" (decodeSet (Json.Decode.string))
   |> fnullable "dur" (Json.Decode.int)

jsonEncTrap : Trap -> Value
jsonEncTrap  val =
   Json.Encode.object
   [ ("direction", jsonEncDirection val.direction)
   , ("trigger", Json.Encode.string val.trigger)
   , ("name", Json.Encode.string val.name)
   , ("skill", jsonEncSkill val.skill)
   , ("user", Json.Encode.int val.user)
   , ("classes", (encodeSet Json.Encode.string) val.classes)
   , ("dur", (maybeEncode (Json.Encode.int)) val.dur)
   ]



type alias Turn  =
   { chakra: Chakras
   , playing: Player
   , victor: (List Player)
   , inactive: (Int, Int)
   , ninjas: (List Ninja)
   , targets: (List (List (List Int)))
   }

jsonDecTurn : Json.Decode.Decoder ( Turn )
jsonDecTurn =
   Json.Decode.succeed Turn
   |> required "chakra" (jsonDecChakras)
   |> required "playing" (jsonDecPlayer)
   |> required "victor" (Json.Decode.list (jsonDecPlayer))
   |> required "inactive" (Json.Decode.map2 tuple2 (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.int)))
   |> required "ninjas" (Json.Decode.list (jsonDecNinja))
   |> required "targets" (Json.Decode.list (Json.Decode.list (Json.Decode.list (Json.Decode.int))))

jsonEncTurn : Turn -> Value
jsonEncTurn  val =
   Json.Encode.object
   [ ("chakra", jsonEncChakras val.chakra)
   , ("playing", jsonEncPlayer val.playing)
   , ("victor", (Json.Encode.list jsonEncPlayer) val.victor)
   , ("inactive", (\(t1,t2) -> Json.Encode.list identity [(Json.Encode.int) t1,(Json.Encode.int) t2]) val.inactive)
   , ("ninjas", (Json.Encode.list jsonEncNinja) val.ninjas)
   , ("targets", (Json.Encode.list (Json.Encode.list (Json.Encode.list Json.Encode.int))) val.targets)
   ]



type alias User  =
   { privilege: Privilege
   , name: String
   , avatar: String
   , background: (Maybe String)
   , wins: Int
   , losses: Int
   , streak: Int
   , record: Int
   , clan: (Maybe String)
   , condense: Bool
   , dna: Int
   , rank: String
   , level: Int
   , xp: Int
   }

jsonDecUser : Json.Decode.Decoder ( User )
jsonDecUser =
   Json.Decode.succeed User
   |> required "privilege" (jsonDecPrivilege)
   |> required "name" (Json.Decode.string)
   |> required "avatar" (Json.Decode.string)
   |> fnullable "background" (Json.Decode.string)
   |> required "wins" (Json.Decode.int)
   |> required "losses" (Json.Decode.int)
   |> required "streak" (Json.Decode.int)
   |> required "record" (Json.Decode.int)
   |> fnullable "clan" (Json.Decode.string)
   |> required "condense" (Json.Decode.bool)
   |> required "dna" (Json.Decode.int)
   |> required "rank" (Json.Decode.string)
   |> required "level" (Json.Decode.int)
   |> required "xp" (Json.Decode.int)

jsonEncUser : User -> Value
jsonEncUser  val =
   Json.Encode.object
   [ ("privilege", jsonEncPrivilege val.privilege)
   , ("name", Json.Encode.string val.name)
   , ("avatar", Json.Encode.string val.avatar)
   , ("background", (maybeEncode (Json.Encode.string)) val.background)
   , ("wins", Json.Encode.int val.wins)
   , ("losses", Json.Encode.int val.losses)
   , ("streak", Json.Encode.int val.streak)
   , ("record", Json.Encode.int val.record)
   , ("clan", (maybeEncode (Json.Encode.string)) val.clan)
   , ("condense", Json.Encode.bool val.condense)
   , ("dna", Json.Encode.int val.dna)
   , ("rank", Json.Encode.string val.rank)
   , ("level", Json.Encode.int val.level)
   , ("xp", Json.Encode.int val.xp)
   ]



type War  =
    Red
    | Blue

jsonDecWar : Json.Decode.Decoder ( War )
jsonDecWar =
    let jsonDecDictWar = Dict.fromList [("Red", Red), ("Blue", Blue)]
    in  decodeSumUnaries "War" jsonDecDictWar

jsonEncWar : War -> Value
jsonEncWar  val =
    case val of
        Red -> Json.Encode.string "Red"
        Blue -> Json.Encode.string "Blue"

