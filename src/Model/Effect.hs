
module Model.Effect
  ( Amount(..)
  , Constructor(..)
  , Effect(..)
  , construct
  , helpful
  , sticky
  , bypassEnrage
  , boosted
  , identity
  ) where

import ClassyPrelude

import Data.Aeson ((.=), ToJSON(..), object)

import Core.Util ((∈))
import Class.Classed (Classed(..))
import Class.Display (Display(..))
import Model.Class (Class(..), lower)
import Model.Duration (Duration(..))
import Model.Slot (Slot)

data Amount = Flat | Percent deriving (Bounded, Enum, Eq, Ord, Show, Read)

identity :: ∀ a. Num a => Amount -> a
identity Flat    = 0
identity Percent = 1

data Constructor
    = Only Effect
    | Any (Class -> Effect)

instance Show Constructor where
    show (Only x) = show x
    show (Any xs) = fromMaybe shown $ stripSuffix (' ' : show All) shown
      where
        shown = show $ xs All

construct :: Constructor -> [Effect]
construct (Only x) = [x]
construct (Any x)  = x <$> [minBound..maxBound]

instance Eq Constructor where
    Only x == Only y = x == y
    Any  x == Any  y = x All == y All
    Any  x == Only y = y ∈ (x <$> [minBound..maxBound])
    Only x == Any  y = x ∈ (y <$> [minBound..maxBound])
    {-# INLINE (==) #-}

-- | Effects of 'Status'es.
data Effect
    = Afflict      Int                 -- ^ Deals damage every turn
    | AntiCounter                      -- ^ Cannot be countered or reflected
    | Bleed        Class Amount Int    -- ^ Adds to damage received
    | Bless        Int                 -- ^ Adds to healing 'Skill's
    | Block                            -- ^ Treats user as 'Invulnerable'
    | Boost        Int                 -- ^ Scales effects from allies
    | Build        Int                 -- ^ Adds to destructible defense 'Skill'
    | DamageToDefense                  -- ^ Damage received converts to defense
    | Duel         Slot                -- ^ 'Invulnerable' to everyone but user
    | Endure                           -- ^ Health cannot go below 1
    | Enrage                           -- ^ Ignore all harmful status effects
    | Exhaust      Class               -- ^ 'Skill's cost 1 additional random chakra
    | Expose                           -- ^ Cannot reduce damage or be 'Invulnerable'
    | Heal         Int                 -- ^ Heals every turn
    | Ignore       Constructor         -- ^ Invulnerable to certain effects
    | ImmuneSelf                       -- ^ Invulnerable to self-caused damage
    | Invulnerable Class               -- ^ Invulnerable to enemy 'Skill's
    | Limit        Int                 -- ^ Limits damage received
    | Pierce                           -- ^ Damage attacks become piercing
    | Plague                           -- ^ Invulnerable to healing and curing
    | Reduce       Class Amount Int    -- ^ Reduces damage by an amount
    | Redirect     Class Slot          -- ^ Transfers harmful 'Skill's
    | Reflect                          -- ^ Reflects the first 'Skill'
    | ReflectAll                       -- ^ 'Reflect' repeatedly
    | Replace      Duration Slot Text  -- ^ Copies a skill into user's skill slot
    | Restrict                         -- ^ Forces AoE attacks to be single-target
    | Reveal                           -- ^ Makes 'Invisible' effects visible
    | Seal                             -- ^ Ignore all friendly 'Skill's
    | Share        Slot                -- ^ Harmful skills are also applied to a target
    | Silence                          -- ^ Unable to cause non-damage effects
    | Snare        Int                 -- ^ Increases cooldowns
    | SnareTrap    Class Int           -- ^ Negates next skill and increases cooldown
    | Strengthen   Class Amount Int    -- ^ Adds to all damage dealt
    | Stun         Class               -- ^ Unable to use 'Skill's
    | Swap         Class               -- ^ Target's skills swap enemies and allies
    | Taunt        Slot                -- ^ Forced to attack a target
    | Threshold    Int                 -- ^ Invulnerable to baseline damage below a threhold
    | Throttle     Int Constructor     -- ^ Applying an effect lasts fewer turns
    | Undefend                         -- ^ Does not benefit from destructible defense
    | Uncounter                        -- ^ Cannot counter or reflect
    | Unexhaust                        -- ^ Decreases chakra costs by 1 random
    | Unreduce     Int                 -- ^ Reduces damage reduction 'Skill's
    | Weaken       Class Amount Int    -- ^ Lessens damage dealt
    deriving (Eq, Show)
instance Classed Effect where
    classes (Bleed cla _ _)      = singletonSet cla
    classes (Exhaust cla)        = singletonSet cla
    classes (Invulnerable cla)   = singletonSet cla
    classes (Reduce cla _ _)     = singletonSet cla
    classes (Redirect cla _)     = singletonSet cla
    classes (SnareTrap cla _)    = singletonSet cla
    classes (Strengthen cla _ _) = singletonSet cla
    classes (Stun cla)           = singletonSet cla
    classes (Swap cla)           = singletonSet cla
    classes (Weaken cla _ _)     = singletonSet cla
    classes _                    = mempty

instance ToJSON Effect where
    toJSON x = object
      [ "desc"    .= display' x
      , "helpful" .= helpful x
      , "sticky"  .= sticky x
      , "trap"    .= False
      ]

helpful :: Effect -> Bool
helpful Afflict{}       = False
helpful AntiCounter     = True
helpful (Bleed _ _ x)   = x < 0
helpful Bless{}         = True
helpful Block{}         = False
helpful Boost{}         = True
helpful (Build x)       = x >= 0
helpful DamageToDefense = True
helpful Duel{}          = True
helpful Endure          = True
helpful Enrage          = True
helpful Exhaust{}       = False
helpful Expose          = False
helpful Heal{}          = True
helpful Ignore{}        = True
helpful ImmuneSelf      = True
helpful Invulnerable{}  = True
helpful Limit{}         = True
helpful Pierce          = True
helpful Plague          = False
helpful (Reduce _ _ x)  = x >= 0
helpful Redirect{}      = True
helpful Reflect         = True
helpful ReflectAll      = True
helpful Replace{}       = False
helpful Restrict        = False
helpful Reveal          = False
helpful Seal            = False
helpful Share{}         = False
helpful Silence         = False
helpful (Snare x)       = x < 0
helpful SnareTrap{}     = False
helpful Strengthen{}    = True
helpful Stun{}          = False
helpful Swap{}          = False
helpful Taunt{}         = False
helpful Threshold{}     = True
helpful Throttle{}      = False
helpful Uncounter       = False
helpful Undefend        = False
helpful Unexhaust       = True
helpful Unreduce{}      = False
helpful Weaken{}        = False

-- | Effect cannot be removed.
sticky :: Effect -> Bool
sticky Block{}        = True
sticky Enrage         = True
sticky Invulnerable{} = True
sticky Redirect{}     = True
sticky Replace{}      = True
sticky Reflect        = True
sticky ReflectAll     = True
sticky Restrict       = True
sticky Reveal         = True
sticky Share{}        = True
sticky Swap{}         = True
sticky _              = False

displayAmt :: Amount -> Int -> TextBuilder
displayAmt Flat    = display
displayAmt Percent = (++ "%") . display

instance Display Effect where
    display (Afflict x) = "Receives " ++ display x ++ " affliction damage each turn."
    display AntiCounter = "Cannot be countered or reflected."
    display (Bleed cla amt x)
      | x >= 0    =  displayAmt amt x ++ " additional damage taken from " ++ lower cla ++ " skills."
      | otherwise = "Reduces all " ++ lower cla ++  " damage received by " ++ displayAmt amt (-x) ++ "."
    display (Bless x) = "Healing skills heal 1 additional " ++ display x ++ " health."
    display Block = "Unable to affect the source of this effect."
    display (Boost x) = "Active effects from allies are " ++ display x ++ " times as powerful."
    display (Build x)
      | x >= 0    = "Destructible skills provide " ++ display x ++ " additional points of defense."
      | otherwise =  "Destructible skills provide " ++ display (-x) ++ " fewer points of defense."
    display DamageToDefense = "Non-affliction damage received is converted into destructible defense."
    display (Duel _) = "Invulnerable to everyone but a specific target."
    display Endure = "Health cannot go below 1."
    display Enrage = "Ignores status effects from enemies except chakra cost changes."
    display (Exhaust cla) = display cla ++ " skills cost 1 additional random chakra."
    display Expose = "Unable to reduce damage or become invulnerable."
    display (Heal x) = "Gains " ++ display x ++ " health each turn."
    display (Ignore _) = "Ignores some effects."
    display ImmuneSelf = "Invulnerable to self-damage."
    display (Invulnerable cla) = "Invulnerable to " ++ lower cla ++ " skills."
    display (Limit x) = "Non-affliction damage received is reduced to at most " ++ display x ++ "."
    display Pierce = "Non-affliction skills deal piercing damage."
    display Plague = "Cannot be healed or cured."
    display (Reduce Affliction amt x)
      | x >= 0    = "Reduces all damage received—including piercing and affliction—by " ++ displayAmt amt x ++ "."
      | otherwise = "Increases all damage received—including piercing and affliction—by " ++ displayAmt amt x ++ "."
    display (Reduce cla amt x)
      | x >= 0    = "Reduces " ++ lower cla ++ " damage received by " ++ displayAmt amt x ++ ". Does not affect piercing or affliction damage."
      | otherwise = "Increases " ++ lower cla ++ " damage received by " ++ displayAmt amt (-x) ++ ". Does not affect piercing or affliction damage."
    display (Redirect cla _) = "Redirects " ++ lower cla  ++ " harmful skills to a different target."
    display Reflect = "Reflects the first harmful non-mental skill."
    display ReflectAll = "Reflects all non-mental skills."
    display (Replace d _ l) = "The next skill used will be copied to the source of this effect's [" ++ display l ++ "] skill for " ++ display d ++ " turns."
    display Reveal = "Reveals invisible skills to the enemy team. This effect cannot be removed."
    display Restrict = "Skills that normally affect all opponents must be targeted."
    display Seal = "Invulnerable to effects from allies."
    display (Share _) = "Harmful skills received are also reflected to another target."
    display Silence = "Unable to cause non-damage effects."
    display (Snare x)
      | x >= 0    = "Cooldowns increased by " ++ display x ++ "."
      | otherwise = "Cooldowns decreased by " ++ display (-x) ++ "."
    display (SnareTrap _ _) = "Next skill used will be negated and go on a longer cooldown."
    display (Strengthen cla amt x) = display cla ++ " damaging skills deal " ++ displayAmt amt x ++ " additional damage."
    display (Stun Affliction) = "Unable to deal affliction damage."
    display (Stun NonAffliction) = "Unable to deal non-affliction damage."
    display (Stun cla) = "Unable to use " ++ lower cla ++ " skills."
    display (Swap cla) = "Next " ++ lower cla ++ " skill will target allies instead of enemies and enemies instead of allies."
    display (Taunt _) = "Can only affect a specific target."
    display (Threshold x) = "Uninjured by attacks that deal " ++ display x ++ " baseline damage or lower."
    display (Throttle x _) = "Skills will apply " ++ display x ++ " fewer turns of some effects."
    display Uncounter = "Unable to benefit from counters or reflects."
    display Undefend = "Unable to benefit from destructible defense"
    display Unexhaust = "All skills cost 1 fewer random chakra."
    display (Unreduce x) = "Damage reduction skills reduce " ++ display x ++ " fewer damage."
    display (Weaken cla amt x) = display cla ++ " skills deal " ++ displayAmt amt x ++ " fewer damage. Does not affect affliction damage."

-- | Scales the power of an effect.
boosted :: Int -> Effect -> Effect
boosted b (Afflict  x) = Afflict  $ x * b
boosted b (Build    x) = Build    $ x * b
boosted b (Heal     x) = Heal     $ x * b
boosted b (Snare    x) = Snare    $ x * b
boosted b (Unreduce x) = Unreduce $ x * b
boosted b (Bleed      c Flat x) = Bleed      c Flat $ x * b
boosted b (Reduce     c Flat x) = Reduce     c Flat $ x * b
boosted b (Strengthen c Flat x) = Strengthen c Flat $ x * b
boosted b (Weaken     c Flat x) = Weaken     c Flat $ x * b
boosted _ ef = ef

-- | Not canceled by 'Enrage'.
bypassEnrage :: Effect -> Bool
bypassEnrage Exhaust{} = True
bypassEnrage Unexhaust = True
bypassEnrage ef        = helpful ef || sticky ef
