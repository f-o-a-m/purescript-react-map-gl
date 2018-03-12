module Main where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.Record.Builder (merge, build)
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
import Unsafe.Coerce (unsafeCoerce)


main :: forall eff. Eff (dom :: DOM | eff) Unit
main = void  $ elm' >>= render (R.createFactory mapClass unit)
  where
    elm' :: Eff (dom :: DOM | eff) Element
    elm' = do
      win <- window
      doc <- Window.document win
      elm <- getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))
      pure $ unsafePartial (fromJust elm)

mapClass :: forall props . R.ReactClass props
mapClass = R.createClass mapSpec

mapSpec ::  forall props eff . R.ReactSpec props MapGL.Viewport R.ReactElement (console :: CONSOLE, dom :: DOM | eff)
mapSpec = R.spec' (const initialViewport) render
  where
    render this = do
      let mapProps' = merge { onChangeViewport: mkEffFn1 $ \newVp -> do
                                 log $ "Changed Viewport: " <> show newVp
                                 void $ R.writeState this newVp
                            , onClick: mkEffFn1 $ \info -> do
                                 log $ unsafeCoerce info
                                 log $ "Clicked map: (" <> show (MapGL.lat info.lngLat) <> ", " <> show (MapGL.lng info.lngLat) <> ")"
                            , mapStyle: mapStyle
                            , mapboxApiAccessToken: mapboxApiAccessToken
                            }
      vp <- unwrap <$> R.readState this
      let mapProps = build mapProps' vp
      pure $ R.createFactory MapGL.mapGL mapProps

initialViewport :: forall eff . Eff (dom :: DOM | eff) MapGL.Viewport
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
mapboxApiAccessToken = "pk.eyJ1IjoiYmxpbmt5MzcxMyIsImEiOiJjamVvcXZtbGYwMXgzMzNwN2JlNGhuMHduIn0.ue2IR6wHG8b9eUoSfPhTuQ"


