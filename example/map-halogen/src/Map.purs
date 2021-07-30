module Map
  ( Messages(..)
  , Commands(..)
  , MapMessages(..)
  , mapClass
  ) where

import Prelude

import Control.Lazy (fix)
import Data.Newtype (un)
import Data.Tuple (snd)
import Effect.Aff (launchAff_, error)
import Effect.Class (liftEffect)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Bus as Bus
import Effect.Uncurried (mkEffectFn1)
import MapGL (ClickInfo, Viewport)
import MapGL as MapGL
import React as R
import Record (disjointUnion)

data MapMessages
  = OnViewportChange Viewport
  | OnClick ClickInfo

data Commands
  = SetViewport' Viewport
  | AskViewport' (AVar Viewport)

data Messages
  = IsInitialized (Bus.BusW Commands)
  | PublicMsg MapMessages

--------------------------------------------------------------------------------

type Props =
  { messages :: Bus.BusW Messages
  , width :: Number
  , height :: Number
  }

type State =
  { command :: Bus.BusRW Commands
  , viewport :: MapGL.Viewport
  }

mapClass :: R.ReactClass Props
mapClass = R.component "Map" \this -> do
  command <- Bus.make
  { messages, width, height } <- R.getProps this
  launchAff_ $ Bus.write (IsInitialized $ snd $ Bus.split command) messages
  pure
    { componentDidMount: componentDidMount this
    , componentWillUnmount: componentWillUnmount this
    , render: render this
    , state:
        { viewport: MapGL.Viewport
          { width
          , height
          , longitude: -74.00539284665783
          , latitude: 40.70544878575082
          , zoom: 10.822714855509464
          , pitch: 0.0
          , bearing: 0.0
          }
        , command
        }
    }
  where
    componentWillUnmount :: R.ReactThis Props State -> R.ComponentWillUnmount
    componentWillUnmount this = R.getState this >>= \{ command } ->
      launchAff_ $ do
        props <- liftEffect $ R.getProps this
        Bus.kill (error "kill from componentWillUnmount") command

    componentDidMount :: R.ReactThis Props State -> R.ComponentDidMount
    componentDidMount this = do
      { command } <- R.getState this
      launchAff_ $ fix \loop -> do
        msg <- Bus.read command
        case msg of
          SetViewport' vp -> liftEffect $ R.modifyState this _{viewport = vp}
          AskViewport' var -> liftEffect (R.getState this) >>= \{viewport} -> AVar.put viewport var
        loop

    render :: R.ReactThis Props State -> R.Render
    render this = do
      { messages } <- R.getProps this
      { viewport } <- R.getState this
      pure $ R.createElement MapGL.mapGL
        (un MapGL.Viewport viewport `disjointUnion`
        { onViewportChange: mkEffectFn1 $ \newVp -> do
            launchAff_ $ Bus.write (PublicMsg $ OnViewportChange newVp) messages
            void $ R.modifyState this _{viewport = newVp}
        , onClick: mkEffectFn1 $ \info -> do
            launchAff_ $ Bus.write (PublicMsg $ OnClick info) messages
        , mapStyle: mapStyle
        , mapboxApiAccessToken: mapboxApiAccessToken
        , onLoad: pure unit
        , dragRotate: true
        , touchZoom: true
        , touchRotate: true
        })
        []

mapStyle :: String
mapStyle = "mapbox://styles/mapbox/dark-v9"

mapboxApiAccessToken :: String
mapboxApiAccessToken = "pk.eyJ1IjoiYmxpbmt5MzcxMyIsImEiOiJjamVvcXZtbGYwMXgzMzNwN2JlNGhuMHduIn0.ue2IR6wHG8b9eUoSfPhTuQ"
