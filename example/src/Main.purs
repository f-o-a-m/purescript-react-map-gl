module Main where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)

main :: Eff (console :: CONSOLE) Unit
main = log "Example"
