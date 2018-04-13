module Main where

import Prelude
import Control.Monad.Eff (Eff)
import MapComponent (mapComponent)
import Halogen.VDom.Driver (runUI)
import Halogen.Aff as HA

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI mapComponent unit body
