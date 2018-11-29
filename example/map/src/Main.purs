module Main where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.Newtype (un)
import Effect (Effect)
import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1)
import MapGL as MapGL
import Partial.Unsafe (unsafePartial)
import React as R
import ReactDOM (render)
import Record (disjointUnion)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

main :: Effect Unit
main = void  $ elm' >>= render (R.createLeafElement mapClass {})
  where
    elm' :: Effect Element
    elm' = do
      win <- window
      doc <- Window.document win
      elm <- getElementById "app" (HTMLDocument.toNonElementParentNode doc)
      pure $ unsafePartial (fromJust elm)

mapClass :: R.ReactClass {}
mapClass = R.component "Map" \this -> do
  win <- window
  w <- Window.innerWidth win
  h <- Window.innerHeight win
  pure 
    { render: render this
    , state: 
        { vp: MapGL.Viewport
            { width: toNumber w
            , height: toNumber h
            , longitude: -74.00539284665783
            , latitude: 40.70544878575082
            , zoom: 10.822714855509464
            , pitch: 0.0
            , bearing: 0.0
            }
        }
    }
    where
      render this = R.getState this <#> \{vp} -> R.createElement MapGL.mapGL
        (un MapGL.Viewport vp `disjointUnion`
          { onViewportChange: mkEffectFn1 $ \newVp -> do
              log $ "Changed Viewport: " <> show newVp
              void $ R.writeState this {vp: newVp}
          , onClick: mkEffectFn1 $ \info -> do
              log $ "Clicked map: " <> show info.lngLat
          , mapStyle: mapStyle
          , mapboxApiAccessToken: mapboxApiAccessToken
          })
          []

mapStyle :: String
mapStyle = "mapbox://styles/mapbox/dark-v9"

mapboxApiAccessToken :: String
mapboxApiAccessToken = "pk.eyJ1IjoiYmxpbmt5MzcxMyIsImEiOiJjamVvcXZtbGYwMXgzMzNwN2JlNGhuMHduIn0.ue2IR6wHG8b9eUoSfPhTuQ"


