module FFI.Sound (Sound(..), register, sound) where

import StandardLibrary
import Generic as G

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
derive instance genericSound :: G.Generic Sound _
instance showSound :: Show Sound where 
  show = G.genericShow

foreign import sound_ :: (Sound -> String) -> Sound -> Effect Unit

sound :: ∀ m. MonadEffect m => Sound -> m Unit
sound = liftEffect <<< sound_ show

foreign import sfxRegister :: (Sound -> String) -> Array Sound -> Effect Unit
register :: Effect Unit
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
