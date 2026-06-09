{-# OPTIONS_HADDOCK prune #-}

module Mission.Hooks.Chakra
  ( ChakraHook
  , deplete
  ) where

import ClassyPrelude

import Game.Model.Chakras (Chakras)

-- | Used in 'HookChakra'.
type ChakraHook = (Chakras, Chakras) -- ^ Chakra before action, user's first.
               -> (Chakras, Chakras) -- ^ Chakra after action, user's first.
               -> Int

-- | Number of 'Chakra.Chakra's depleted.
deplete :: ChakraHook
deplete (_, chak) (_, chak') = max 0 $ length chak' - length chak
