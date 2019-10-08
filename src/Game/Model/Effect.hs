module Game.Model.Effect
  ( Amount(..)
  , Constructor(..)
  , Effect(..)
  , construct
  , helpful
  , sticky
  , disabling
  , ignore
  , bypassEnrage
  , identity
  ) where

import ClassyPrelude

import Data.Aeson ((.=), ToJSON(..), object)

import Class.Classed (Classed(..))
import Class.Display (Display(..))
import Game.Model.Class (Class(..), lower)
import Game.Model.Slot (Slot)
import Util ((∈))

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
    = Absorb                           -- ^ Gain chakra when targeted by skills
    | Afflict      Int                 -- ^ Deals damage every turn
    | Alone                            -- ^ Cannot be targeted by allies
    | AntiCounter                      -- ^ Cannot be countered or reflected
    | Bleed        Class Amount Int    -- ^ Adds to damage received
    | Bless        Int                 -- ^ Adds to healing 'Skill's
    | Block        Slot                -- ^ Treats user as 'Invulnerable'
    | BlockAllies                      -- ^ Cannot affect allies
    | BlockEnemies                     -- ^ Cannot affect enemies
    | Boost        Int                 -- ^ Scales effects from allies
    | Build        Int                 -- ^ Adds to destructible defense 'Skill'
    | DamageToDefense                  -- ^ Damage received converts to defense
    | Duel         Slot                -- ^ 'Invulnerable' to everyone but user
    | Endure                           -- ^ Health cannot go below 1
    | Enrage                           -- ^ Ignore all harmful status effects
    | Exhaust      Class               -- ^ 'Skill's cost 1 additional random chakra
    | Expose                           -- ^ Cannot reduce damage or be 'Invulnerable'
    | Focus                            -- ^ Immune to 'Stun' and 'Throttle 0'
    | Heal         Int                 -- ^ Heals every turn
    | Invulnerable Class               -- ^ Invulnerable to enemy 'Skill's
    | Limit        Int                 -- ^ Limits damage received
    | NoIgnore                         -- ^ Ignore ignores
    | Nullify                          -- ^ Invulnerable but targetable
    | Pierce                           -- ^ Damage attacks become piercing
    | Plague                           -- ^ Invulnerable to healing and curing
    | Reduce       Class Amount Int    -- ^ Reduces damage by an amount
    | Redirect     Class Slot          -- ^ Transfers harmful 'Skill's
    | Reflect                          -- ^ Reflects the first 'Skill'
    | ReflectAll                       -- ^ 'Reflect' repeatedly
    | Restrict                         -- ^ Forces AoE attacks to be single-target
    | Reveal                           -- ^ Makes 'Invisible' effects visible
    | Seal                             -- ^ Ignores helpful status effects
    | Share        Slot                -- ^ Harmful skills are also applied to a target
    | Silence                          -- ^ Unable to cause non-damage effects
    | Snare        Int                 -- ^ Increases cooldowns
    | Strengthen   Class Amount Int    -- ^ Adds to all damage dealt
    | Stun         Class               -- ^ Unable to use 'Skill's
    | Swap                             -- ^ Target's skills swap enemies and allies
    | Taunt        Slot                -- ^ Forced to attack a target
    | Threshold    Int                 -- ^ Invulnerable to baseline damage below a threhold
    | Throttle     Int Constructor     -- ^ Applying an effect lasts fewer turns
    | Undefend                         -- ^ Does not benefit from destructible defense
    | Uncounter                        -- ^ Cannot counter or reflect
    | Unreduce     Int                 -- ^ Reduces damage reduction 'Skill's
    | Weaken       Class Amount Int    -- ^ Lessens damage dealt
    deriving (Eq, Show)
instance Classed Effect where
    classes (Bleed cla _ _)      = singletonSet cla
    classes (Exhaust cla)        = singletonSet cla
    classes (Invulnerable cla)   = singletonSet cla
    classes (Reduce cla _ _)     = singletonSet cla
    classes (Redirect cla _)     = singletonSet cla
    classes (Strengthen cla _ _) = singletonSet cla
    classes (Stun cla)           = singletonSet cla
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
helpful Absorb          = True
helpful Afflict{}       = False
helpful Alone           = False
helpful AntiCounter     = True
helpful (Bleed _ _ x)   = x < 0
helpful Bless{}         = True
helpful Block{}         = False
helpful BlockAllies{}   = False
helpful BlockEnemies{}  = False
helpful Boost{}         = True
helpful (Build x)       = x >= 0
helpful DamageToDefense = True
helpful Duel{}          = True
helpful Endure          = True
helpful Enrage          = True
helpful Exhaust{}       = False
helpful Expose          = False
helpful Focus           = True
helpful Heal{}          = True
helpful Invulnerable{}  = True
helpful Limit{}         = True
helpful NoIgnore        = False
helpful Nullify         = True
helpful Pierce          = True
helpful Plague          = False
helpful (Reduce _ _ x)  = x >= 0
helpful Redirect{}      = True
helpful Reflect         = True
helpful ReflectAll      = True
helpful Restrict        = False
helpful Reveal          = False
helpful Seal            = False
helpful Share{}         = False
helpful Silence         = False
helpful (Snare x)       = x < 0
helpful Strengthen{}    = True
helpful Stun{}          = False
helpful Swap            = False
helpful Taunt{}         = False
helpful Threshold{}     = True
helpful Throttle{}      = False
helpful Uncounter       = False
helpful Undefend        = False
helpful Unreduce{}      = False
helpful Weaken{}        = False

-- | Effect cannot be removed.
sticky :: Effect -> Bool
sticky Block{}        = True
sticky Enrage         = True
sticky Invulnerable{} = True
sticky Redirect{}     = True
sticky Reflect        = True
sticky ReflectAll     = True
sticky Restrict       = True
sticky Reveal         = True
sticky Share{}        = True
sticky Swap           = True
sticky _              = False

-- | Effect is affected by 'Focus'.
disabling :: Effect -> Bool
disabling Silence        = True
disabling (Stun _)       = True
disabling (Throttle 0 _) = True
disabling _              = False

-- | Not canceled by 'Enrage'.
bypassEnrage :: Effect -> Bool
bypassEnrage Exhaust{} = True
bypassEnrage Reveal    = True
bypassEnrage Share{}   = True
bypassEnrage ef        = helpful ef

-- | Effect is affected by 'NoIgnore'.
ignore :: Effect -> Bool
ignore Enrage  = True
ignore Focus   = True
ignore Nullify = True
ignore _       = False

displayAmt :: Amount -> Int -> TextBuilder
displayAmt Flat    = display
displayAmt Percent = (++ "%") . display

instance Display Effect where
    display Absorb = "Gains chakra equal to the chakra cost of skills received from enemies."
    display (Afflict x) = "Receives " ++ display x ++ " affliction damage each turn."
    display Alone = "Invulnerable to allies."
    display AntiCounter = "Cannot be countered or reflected."
    display (Bleed cla amt x)
      | x >= 0    =  displayAmt amt x ++ " additional damage taken from " ++ lower cla ++ " skills."
      | otherwise = "Reduces all " ++ lower cla ++  " damage received by " ++ displayAmt amt (-x) ++ "."
    display (Bless x) = "Healing skills heal " ++ display x ++ " additional health."
    display (Block _) = "Unable to affect a specific target."
    display BlockAllies = "Unable to affect allies."
    display BlockEnemies = "Unable to affect enemies."
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
    display Focus = "Ignores stuns and disabling effects."
    display NoIgnore = "Unable to ignore harm."
    display (Invulnerable cla) = "Invulnerable to " ++ lower cla ++ " skills."
    display (Limit x) = "Non-affliction damage received is reduced to at most " ++ display x ++ "."
    display Nullify = "Ignores enemy skills."
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
    display Reveal = "Reveals invisible skills to the enemy team. This effect cannot be removed."
    display Restrict = "Skills that normally affect all opponents must be targeted."
    display Seal = "Ignores helpful effects."
    display (Share _) = "Harmful skills received are also reflected to another target."
    display Silence = "Unable to cause non-damage effects."
    display (Snare x)
      | x >= 0    = "Cooldowns increased by " ++ display x ++ "."
      | otherwise = "Cooldowns decreased by " ++ display (-x) ++ "."
    display (Strengthen cla amt x) = display cla ++ " damaging skills deal " ++ displayAmt amt x ++ " additional damage."
    display (Stun Affliction) = "Unable to deal affliction damage."
    display (Stun NonAffliction) = "Unable to deal non-affliction damage."
    display (Stun cla) = "Unable to use " ++ lower cla ++ " skills."
    display Swap = "Next skill will target allies instead of enemies and enemies instead of allies."
    display (Taunt _) = "Can only affect a specific target."
    display (Threshold x) = "Uninjured by attacks that deal " ++ display x ++ " baseline damage or lower."
    display (Throttle 0 _) = "Skills cannot apply some effects."
    display (Throttle x _) = "Skills will apply " ++ display x ++ " fewer turns of some effects."
    display Uncounter = "Unable to benefit from counters or reflects."
    display Undefend = "Unable to benefit from destructible defense."
    display (Unreduce x) = "Damage reduction skills reduce " ++ display x ++ " fewer damage."
    display (Weaken cla amt x) = display cla ++ " skills deal " ++ displayAmt amt x ++ " fewer damage. Does not affect affliction damage."
