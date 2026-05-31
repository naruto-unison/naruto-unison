{-# LANGUAGE TemplateHaskell #-}
module Application.Static where

import Yesod.Static

$(staticFiles "static/")
