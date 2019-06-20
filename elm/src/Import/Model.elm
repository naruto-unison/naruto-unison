module Import.Model exposing (..)

import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (ObjectEncoding, encodeObject, encodeValue, decodeSumObjectWithSingleField, encodeSumObjectWithSingleField, decodeSumTwoElemArray, encodeSumTwoElementArray, encodeSumTaggedObject, decodeSumUnaries, decodeSumNullaries, decodeSumNullaryOrSingleField, decodeMap, encodeMap, jsonEncDict, jsonDecDict, encodeSet, decodeSet, maybeEncode, encodeSumUntagged, required, custom, fnullable, tuple2, tuple3)
import Dict exposing (Dict)
import Set exposing (Set)

import Import.Decode exposing (decodeSumTaggedObject)

type alias User  =
   { name: String
   , avatar: String
   , clan: (Maybe String)
   , xp: Int
   , wins: Int
   , losses: Int
   , streak: Int
   , background: (Maybe String)
   , privilege: Privilege
   , condense: Bool
   }

jsonDecUser : Json.Decode.Decoder ( User )
jsonDecUser =
   Json.Decode.succeed (\pname pavatar pclan pxp pwins plosses pstreak pbackground pprivilege pcondense -> {name = pname, avatar = pavatar, clan = pclan, xp = pxp, wins = pwins, losses = plosses, streak = pstreak, background = pbackground, privilege = pprivilege, condense = pcondense})
   |> required "name" (Json.Decode.string)
   |> required "avatar" (Json.Decode.string)
   |> fnullable "clan" (Json.Decode.string)
   |> required "xp" (Json.Decode.int)
   |> required "wins" (Json.Decode.int)
   |> required "losses" (Json.Decode.int)
   |> required "streak" (Json.Decode.int)
   |> fnullable "background" (Json.Decode.string)
   |> required "privilege" (jsonDecPrivilege)
   |> required "condense" (Json.Decode.bool)

jsonEncUser : User -> Value
jsonEncUser  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("avatar", Json.Encode.string val.avatar)
   , ("clan", (maybeEncode (Json.Encode.string)) val.clan)
   , ("xp", Json.Encode.int val.xp)
   , ("wins", Json.Encode.int val.wins)
   , ("losses", Json.Encode.int val.losses)
   , ("streak", Json.Encode.int val.streak)
   , ("background", (maybeEncode (Json.Encode.string)) val.background)
   , ("privilege", jsonEncPrivilege val.privilege)
   , ("condense", Json.Encode.bool val.condense)
   ]



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



type alias Context  =
   { skill: Skill
   , user: Int
   , target: Int
   }

jsonDecContext : Json.Decode.Decoder ( Context )
jsonDecContext =
   Json.Decode.succeed (\pskill puser ptarget -> {skill = pskill, user = puser, target = ptarget})
   |> required "skill" (jsonDecSkill)
   |> required "user" (Json.Decode.int)
   |> required "target" (Json.Decode.int)

jsonEncContext : Context -> Value
jsonEncContext  val =
   Json.Encode.object
   [ ("skill", jsonEncSkill val.skill)
   , ("user", Json.Encode.int val.user)
   , ("target", Json.Encode.int val.target)
   ]



type alias Game  =
   { chakra: (Chakras, Chakras)
   , ninjas: (List Ninja)
   , playing: Player
   , victor: (List Player)
   , targets: (List (List (List Int)))
   }

jsonDecGame : Json.Decode.Decoder ( Game )
jsonDecGame =
   Json.Decode.succeed (\pchakra pninjas pplaying pvictor ptargets -> {chakra = pchakra, ninjas = pninjas, playing = pplaying, victor = pvictor, targets = ptargets})
   |> required "chakra" (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecChakras)) (Json.Decode.index 1 (jsonDecChakras)))
   |> required "ninjas" (Json.Decode.list (jsonDecNinja))
   |> required "playing" (jsonDecPlayer)
   |> required "victor" (Json.Decode.list (jsonDecPlayer))
   |> required "targets" (Json.Decode.list (Json.Decode.list (Json.Decode.list (Json.Decode.int))))

jsonEncGame : Game -> Value
jsonEncGame  val =
   Json.Encode.object
   [ ("chakra", (\(t1,t2) -> Json.Encode.list identity [(jsonEncChakras) t1,(jsonEncChakras) t2]) val.chakra)
   , ("ninjas", (Json.Encode.list jsonEncNinja) val.ninjas)
   , ("playing", jsonEncPlayer val.playing)
   , ("victor", (Json.Encode.list jsonEncPlayer) val.victor)
   , ("targets", (Json.Encode.list (Json.Encode.list (Json.Encode.list Json.Encode.int))) val.targets)
   ]



type alias GameInfo  =
   { opponent: User
   , left: Int
   , game: Game
   , characters: (List Character)
   , player: Player
   }

jsonDecGameInfo : Json.Decode.Decoder ( GameInfo )
jsonDecGameInfo =
   Json.Decode.succeed (\popponent pleft pgame pcharacters pplayer -> {opponent = popponent, left = pleft, game = pgame, characters = pcharacters, player = pplayer})
   |> required "opponent" (jsonDecUser)
   |> required "left" (Json.Decode.int)
   |> required "game" (jsonDecGame)
   |> required "characters" (Json.Decode.list (jsonDecCharacter))
   |> required "player" (jsonDecPlayer)

jsonEncGameInfo : GameInfo -> Value
jsonEncGameInfo  val =
   Json.Encode.object
   [ ("opponent", jsonEncUser val.opponent)
   , ("left", Json.Encode.int val.left)
   , ("game", jsonEncGame val.game)
   , ("characters", (Json.Encode.list jsonEncCharacter) val.characters)
   , ("player", jsonEncPlayer val.player)
   ]



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



type alias Ninja  =
   { slot: Int
   , health: Int
   , defense: (List Defense)
   , barrier: (List Barrier)
   , statuses: (List Status)
   , charges: (List Int)
   , cooldowns: (List Int)
   , variants: (List (List Variant))
   , copies: (List (Maybe Copy))
   , channels: (List Channel)
   , traps: (List Trap)
   , face: (List Face)
   , parrying: (List Skill)
   , tags: (List ChannelTag)
   , lastSkill: (Maybe Skill)
   , skills: (List Skill)
   }

jsonDecNinja : Json.Decode.Decoder ( Ninja )
jsonDecNinja =
   Json.Decode.succeed (\pslot phealth pdefense pbarrier pstatuses pcharges pcooldowns pvariants pcopies pchannels ptraps pface pparrying ptags plastSkill pskills -> {slot = pslot, health = phealth, defense = pdefense, barrier = pbarrier, statuses = pstatuses, charges = pcharges, cooldowns = pcooldowns, variants = pvariants, copies = pcopies, channels = pchannels, traps = ptraps, face = pface, parrying = pparrying, tags = ptags, lastSkill = plastSkill, skills = pskills})
   |> required "slot" (Json.Decode.int)
   |> required "health" (Json.Decode.int)
   |> required "defense" (Json.Decode.list (jsonDecDefense))
   |> required "barrier" (Json.Decode.list (jsonDecBarrier))
   |> required "statuses" (Json.Decode.list (jsonDecStatus))
   |> required "charges" (Json.Decode.list (Json.Decode.int))
   |> required "cooldowns" (Json.Decode.list (Json.Decode.int))
   |> required "variants" (Json.Decode.list (Json.Decode.list (jsonDecVariant)))
   |> required "copies" (Json.Decode.list (Json.Decode.maybe (jsonDecCopy)))
   |> required "channels" (Json.Decode.list (jsonDecChannel))
   |> required "traps" (Json.Decode.list (jsonDecTrap))
   |> required "face" (Json.Decode.list (jsonDecFace))
   |> required "parrying" (Json.Decode.list (jsonDecSkill))
   |> required "tags" (Json.Decode.list (jsonDecChannelTag))
   |> fnullable "lastSkill" (jsonDecSkill)
   |> required "skills" (Json.Decode.list (jsonDecSkill))

jsonEncNinja : Ninja -> Value
jsonEncNinja  val =
   Json.Encode.object
   [ ("slot", Json.Encode.int val.slot)
   , ("health", Json.Encode.int val.health)
   , ("defense", (Json.Encode.list jsonEncDefense) val.defense)
   , ("barrier", (Json.Encode.list jsonEncBarrier) val.barrier)
   , ("statuses", (Json.Encode.list jsonEncStatus) val.statuses)
   , ("charges", (Json.Encode.list Json.Encode.int) val.charges)
   , ("cooldowns", (Json.Encode.list Json.Encode.int) val.cooldowns)
   , ("variants", (Json.Encode.list (Json.Encode.list jsonEncVariant)) val.variants)
   , ("copies", (Json.Encode.list (maybeEncode (jsonEncCopy))) val.copies)
   , ("channels", (Json.Encode.list jsonEncChannel) val.channels)
   , ("traps", (Json.Encode.list jsonEncTrap) val.traps)
   , ("face", (Json.Encode.list jsonEncFace) val.face)
   , ("parrying", (Json.Encode.list jsonEncSkill) val.parrying)
   , ("tags", (Json.Encode.list jsonEncChannelTag) val.tags)
   , ("lastSkill", (maybeEncode (jsonEncSkill)) val.lastSkill)
   , ("skills", (Json.Encode.list jsonEncSkill) val.skills)
   ]



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
        jsonDecObjectSetRequirement = Set.fromList []
    in  decodeSumTaggedObject "Requirement" "tag" "contents" jsonDecDictRequirement jsonDecObjectSetRequirement

jsonEncRequirement : Requirement -> Value
jsonEncRequirement  val =
    let keyval v = case v of
                    Usable  -> ("Usable", encodeValue (Json.Encode.list identity []))
                    Unusable  -> ("Unusable", encodeValue (Json.Encode.list identity []))
                    HasI v1 v2 -> ("HasI", encodeValue (Json.Encode.list identity [Json.Encode.int v1, Json.Encode.string v2]))
                    HasU v1 -> ("HasU", encodeValue (Json.Encode.string v1))
    in encodeSumTaggedObject "tag" "contents" keyval val



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
        jsonDecObjectSetTarget = Set.fromList []
    in  decodeSumTaggedObject "Target" "tag" "contents" jsonDecDictTarget jsonDecObjectSetTarget

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
    in encodeSumTaggedObject "tag" "contents" keyval val



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



type alias Channel  =
   { source: Int
   , skill: Skill
   , target: Int
   , dur: Channeling
   }

jsonDecChannel : Json.Decode.Decoder ( Channel )
jsonDecChannel =
   Json.Decode.succeed (\psource pskill ptarget pdur -> {source = psource, skill = pskill, target = ptarget, dur = pdur})
   |> required "source" (Json.Decode.int)
   |> required "skill" (jsonDecSkill)
   |> required "target" (Json.Decode.int)
   |> required "dur" (jsonDecChanneling)

jsonEncChannel : Channel -> Value
jsonEncChannel  val =
   Json.Encode.object
   [ ("source", Json.Encode.int val.source)
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



type alias ChannelTag  =
   { source: Int
   , user: Int
   , skill: Skill
   , ghost: Bool
   , dur: Int
   }

jsonDecChannelTag : Json.Decode.Decoder ( ChannelTag )
jsonDecChannelTag =
   Json.Decode.succeed (\psource puser pskill pghost pdur -> {source = psource, user = puser, skill = pskill, ghost = pghost, dur = pdur})
   |> required "source" (Json.Decode.int)
   |> required "user" (Json.Decode.int)
   |> required "skill" (jsonDecSkill)
   |> required "ghost" (Json.Decode.bool)
   |> required "dur" (Json.Decode.int)

jsonEncChannelTag : ChannelTag -> Value
jsonEncChannelTag  val =
   Json.Encode.object
   [ ("source", Json.Encode.int val.source)
   , ("user", Json.Encode.int val.user)
   , ("skill", jsonEncSkill val.skill)
   , ("ghost", Json.Encode.bool val.ghost)
   , ("dur", Json.Encode.int val.dur)
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



type alias Effect  =
   { desc: String
   , helpful: Bool
   , sticky: Bool
   , trap: Bool
   }

jsonDecEffect : Json.Decode.Decoder ( Effect )
jsonDecEffect =
   Json.Decode.succeed (\pdesc phelpful psticky ptrap -> {desc = pdesc, helpful = phelpful, sticky = psticky, trap = ptrap})
   |> required "desc" (Json.Decode.string)
   |> required "helpful" (Json.Decode.bool)
   |> required "sticky" (Json.Decode.bool)
   |> required "trap" (Json.Decode.bool)

jsonEncEffect : Effect -> Value
jsonEncEffect  val =
   Json.Encode.object
   [ ("desc", Json.Encode.string val.desc)
   , ("helpful", Json.Encode.bool val.helpful)
   , ("sticky", Json.Encode.bool val.sticky)
   , ("trap", Json.Encode.bool val.trap)
   ]



type alias Status  =
   { amount: Int
   , name: String
   , source: Int
   , user: Int
   , skill: Skill
   , effects: (List Effect)
   , classes: (List String)
   , bombs: (List (Bomb, (Maybe Bool)))
   , maxDur: Int
   , dur: Int
   }

jsonDecStatus : Json.Decode.Decoder ( Status )
jsonDecStatus =
   Json.Decode.succeed (\pamount pname psource puser pskill peffects pclasses pbombs pmaxDur pdur -> {amount = pamount, name = pname, source = psource, user = puser, skill = pskill, effects = peffects, classes = pclasses, bombs = pbombs, maxDur = pmaxDur, dur = pdur})
   |> required "amount" (Json.Decode.int)
   |> required "name" (Json.Decode.string)
   |> required "source" (Json.Decode.int)
   |> required "user" (Json.Decode.int)
   |> required "skill" (jsonDecSkill)
   |> required "effects" (Json.Decode.list (jsonDecEffect))
   |> required "classes" (Json.Decode.list (Json.Decode.string))
   |> required "bombs" (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecBomb)) (Json.Decode.index 1 (Json.Decode.maybe (Json.Decode.bool)))))
   |> required "maxDur" (Json.Decode.int)
   |> required "dur" (Json.Decode.int)

jsonEncStatus : Status -> Value
jsonEncStatus  val =
   Json.Encode.object
   [ ("amount", Json.Encode.int val.amount)
   , ("name", Json.Encode.string val.name)
   , ("source", Json.Encode.int val.source)
   , ("user", Json.Encode.int val.user)
   , ("skill", jsonEncSkill val.skill)
   , ("effects", (Json.Encode.list jsonEncEffect) val.effects)
   , ("classes", (Json.Encode.list Json.Encode.string) val.classes)
   , ("bombs", (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncBomb) t1,((maybeEncode (Json.Encode.bool))) t2])) val.bombs)
   , ("maxDur", Json.Encode.int val.maxDur)
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



type alias Face  =
   { icon: String
   , user: Int
   , dur: Int
   }

jsonDecFace : Json.Decode.Decoder ( Face )
jsonDecFace =
   Json.Decode.succeed (\picon puser pdur -> {icon = picon, user = puser, dur = pdur})
   |> required "icon" (Json.Decode.string)
   |> required "user" (Json.Decode.int)
   |> required "dur" (Json.Decode.int)

jsonEncFace : Face -> Value
jsonEncFace  val =
   Json.Encode.object
   [ ("icon", Json.Encode.string val.icon)
   , ("user", Json.Encode.int val.user)
   , ("dur", Json.Encode.int val.dur)
   ]



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



type alias Trap  =
   { direction: Direction
   , trigger: String
   , name: String
   , desc: String
   , user: Int
   , classes: (List String)
   , tracker: Int
   , dur: Int
   }

jsonDecTrap : Json.Decode.Decoder ( Trap )
jsonDecTrap =
   Json.Decode.succeed (\pdirection ptrigger pname pdesc puser pclasses ptracker pdur -> {direction = pdirection, trigger = ptrigger, name = pname, desc = pdesc, user = puser, classes = pclasses, tracker = ptracker, dur = pdur})
   |> required "direction" (jsonDecDirection)
   |> required "trigger" (Json.Decode.string)
   |> required "name" (Json.Decode.string)
   |> required "desc" (Json.Decode.string)
   |> required "user" (Json.Decode.int)
   |> required "classes" (Json.Decode.list (Json.Decode.string))
   |> required "tracker" (Json.Decode.int)
   |> required "dur" (Json.Decode.int)

jsonEncTrap : Trap -> Value
jsonEncTrap  val =
   Json.Encode.object
   [ ("direction", jsonEncDirection val.direction)
   , ("trigger", Json.Encode.string val.trigger)
   , ("name", Json.Encode.string val.name)
   , ("desc", Json.Encode.string val.desc)
   , ("user", Json.Encode.int val.user)
   , ("classes", (Json.Encode.list Json.Encode.string) val.classes)
   , ("tracker", Json.Encode.int val.tracker)
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

