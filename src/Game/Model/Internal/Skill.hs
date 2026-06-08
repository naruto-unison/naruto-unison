{-# OPTIONS_HADDOCK hide, not-home #-}
module Game.Model.Internal.Skill (Skill(..), key) where

import Game.Model.Internal (Key(Key), Skill(..))

-- | Generates a 'Key' used for 'Game.Ninja.cooldowns' and 'Game.Ninja.charges'.
key :: Skill -> Key
key Skill{name, owner} = Key name owner
