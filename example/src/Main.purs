module Main where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Data.Maybe (fromJust)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window as Window
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import MapGL as MapGL
import Partial.Unsafe (unsafePartial)
import React as R
import React.DOM as D
import ReactDOM (render)


main :: forall eff. Eff (dom :: DOM | eff) Unit
main = void  $ elm' >>= render (R.createFactory mapAppClass unit)
  where
    elm' :: Eff (dom :: DOM | eff) Element
    elm' = do
      win <- window
      doc <- Window.document win
      elm <- getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))
      pure $ unsafePartial (fromJust elm)

mapAppClass :: R.ReactClass Unit
mapAppClass = R.createClassStateless (\this -> (D.text "hello"))


initialViewport :: Eff (dom :: DOM) MapGL.Viewport
initialViewport = do
  win <- window
  w <- Window.innerWidth win
  h <- Window.innerHeight win
  pure $
    MapGL.Viewport { width: w
                   , height: h
                   , longitude: -74.00539284665783
                   , latitude: 40.70544878575082
                   , zoom: 10.822714855509464
                   , pitch: 0.0
                   , bearing: 0.0
                   }

mapStyle :: String
mapStyle = "mapbox://styles/mapbox/dark-v9"

mapboxApiAccessToken :: String
mapboxApiAccessToken = "pk.eyJ1Ijoia2VqYWNlIiwiYSI6ImNqMWIxYnc2MzA5aGYycW1va2pmN3pzcXgifQ.sjdeDT6rkYOBQxCnoYLssw"

