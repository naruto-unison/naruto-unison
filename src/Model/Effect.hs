
module Model.Effect
  ( Amount(..)
  , Effect(..)
  , helpful
  , sticky
  , bypassEnrage
  , boosted
  , identity
  ) where

import ClassyPrelude

import Model.Internal (Amount(..), Effect(..))

identity :: ∀ a. Num a => Amount -> a
identity Flat    = 0
identity Percent = 1

helpful :: Effect -> Bool
helpful Afflict{}      = False
helpful AntiCounter    = True
helpful (Bleed _ _ x)  = x < 0
helpful Bless{}        = True
helpful Block{}        = False
helpful Boost{}        = True
helpful (Build x)      = x >= 0
helpful Counter{}      = True
helpful CounterAll{}   = True
helpful Duel{}         = True
helpful Endure         = True
helpful Enrage         = True
helpful Exhaust{}      = False
helpful Expose         = False
helpful Heal{}         = True
helpful Invulnerable{} = True
helpful ImmuneSelf     = True
helpful Ignore{}       = True
helpful Invincible{}   = True
helpful Parry{}        = True
helpful ParryAll {}    = True
helpful Pierce         = True
helpful Plague         = False
helpful (Reduce _ _ x) = x >= 0
helpful Redirect{}     = True
helpful Reflect        = True
helpful ReflectAll     = True
helpful Replace{}      = False
helpful Restrict       = False
helpful Reveal         = False
helpful Seal           = False
helpful Share{}        = False
helpful Silence        = False
helpful Snapshot{}     = True
helpful (Snare x)      = x < 0
helpful SnareTrap{}    = False
helpful Strengthen{}   = True
helpful Stun{}         = False
helpful Swap{}         = False
helpful Taunt{}        = False
helpful Threshold{}    = True
helpful Throttle{}     = False
helpful Uncounter      = False
helpful Undefend       = False
helpful Unexhaust      = True
helpful Unreduce{}     = False
helpful Weaken{}       = False

-- | Effect cannot be removed.
sticky :: Effect -> Bool
sticky Block{}        = True
sticky Counter{}      = True
sticky CounterAll{}   = True
sticky Enrage         = True
sticky Invulnerable{} = True
sticky Invincible{}   = True
sticky Parry{}        = True
sticky ParryAll{}     = True
sticky Redirect{}     = True
sticky Replace{}      = True
sticky Reflect        = True
sticky ReflectAll     = True
sticky Restrict       = True
sticky Reveal         = True
sticky Share{}        = True
sticky Snapshot{}     = True
sticky Swap{}         = True
sticky _              = False

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
