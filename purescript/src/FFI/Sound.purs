module FFI.Sound (AUDIO, Sound(..), register, sound) where

import Prelude

import Control.Monad.Eff     (Eff, kind Effect)
import Data.Generic.Rep      (class Generic)
import Data.Generic.Rep.Show (genericShow)

foreign import data AUDIO ∷ Effect

data Sound = SFXApplySkill
           | SFXCancel
           | SFXClick
           | SFXDeath
           | SFXLose
           | SFXNextTurn
           | SFXScroll
           | SFXStartFirst
           | SFXStartTurn
           | SFXStartSecond
           | SFXTarget
           | SFXWin
derive instance genericSound ∷ Generic Sound _
instance showSound ∷ Show Sound where 
  show = genericShow

foreign import sfxPlay ∷ ∀ e. (Sound → String) → Sound → Eff (audio ∷ AUDIO | e) Unit
sound ∷ ∀ e. Sound → Eff (audio ∷ AUDIO | e) Unit
sound = sfxPlay show

foreign import sfxRegister ∷ ∀ e. (Sound → String) → Array Sound → Eff (e) Unit
register ∷ ∀ e. Eff (e) Unit
register = sfxRegister show [ SFXApplySkill
                            , SFXCancel
                            , SFXClick
                            , SFXDeath
                            , SFXLose
                            , SFXNextTurn
                            , SFXScroll
                            , SFXStartFirst
                            , SFXStartTurn
                            , SFXStartSecond
                            , SFXTarget
                            , SFXWin
                            ]