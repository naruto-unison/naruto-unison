{-# LANGUAGE CPP #-}
module Application.Definitions (isDevelopment) where

import Prelude (Bool(..))

isDevelopment :: Bool
#ifdef DEVELOPMENT
isDevelopment = True
#else
isDevelopment = False
#endif
