module Map
  ( Messages(..)
  , MapMessages(..)
  , Commands(..)
  , mapClass
  ) where

import Prelude
import Effect.Class.Console (log)
import Control.Lazy (fix)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (un)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn1)
import MapGL (ClickInfo, InteractiveMap, Viewport(..))
import MapGL as MapGL
import Mapbox as Mapbox
import React (ComponentDidMount, ComponentWillUnmount, ReactClass, ReactThis, Render, component, createElement, getProps, getState, setState) as R
import React.Ref (ReactInstance, Ref, fromEffect, getCurrentRef) as R
import Record (disjointUnion)
import Simple.JSON (writeImpl)
import Unsafe.Coerce (unsafeCoerce)

data MapMessages
  = OnClick ClickInfo

type MapRef
  = Ref (Maybe InteractiveMap)

data Commands
  = SetFillExtrusionVisibilty Boolean

data Messages
  = IsInitialized (Bus.BusW Commands)
  | PublicMsg MapMessages

type Props
  = { messages :: Bus.BusW Messages
    , width :: Number
    , height :: Number
    }

type State
  = { command :: Bus.BusRW Commands
    , viewport :: Viewport
    }

mapClass :: R.ReactClass Props
mapClass =
  R.component "Map" \this -> do
    mapRef <- liftEffect $ Ref.new Nothing
    command <- Bus.make
    { width, height, messages } <- R.getProps this
    launchAff_ $ Bus.write (IsInitialized $ snd $ Bus.split command) messages
    pure
      { componentDidMount: componentDidMount this mapRef
      , componentWillUnmount: componentWillUnmount this mapRef
      , render: render this mapRef
      , state:
          { viewport:
              Viewport
                { width
                , height
                , longitude: -100.0
                , latitude: 40.0
                , zoom: 3.0
                , pitch: 0.0
                , bearing: 0.0
                }
          , command
          }
      }
  where
  componentWillUnmount :: R.ReactThis Props State -> MapRef -> R.ComponentWillUnmount
  componentWillUnmount this mapRef = do
    liftEffect $ Ref.write Nothing mapRef
    { command } <- R.getState this
    launchAff_
      $ do
          props <- liftEffect $ R.getProps this
          Bus.kill (error "kill from componentWillUnmount") command

  componentDidMount :: R.ReactThis Props State -> MapRef -> R.ComponentDidMount
  componentDidMount this mapRef = do
    { command } <- R.getState this
    launchAff_
      $ fix \loop -> do
          msg <- Bus.read command
          case msg of
            SetFillExtrusionVisibilty visible ->
              liftEffect
                $ do
                    iMap <- Ref.read mapRef
                    for_ (MapGL.getMap =<< iMap) \map ->
                      -- make sure a `mapLayerId` is already available at this point
                      when (isJust $ Mapbox.getSource map mapSourceId)
                        $ Mapbox.setLayerVisibilty map mapLayerId visible
          loop

  mapOnLoadHandler ::
    MapRef ->
    Effect Unit
  mapOnLoadHandler mapRef = do
    iMap <- Ref.read mapRef
    for_ (MapGL.getMap =<< iMap) \map -> do
      -- initial fillextrusion layer
      Mapbox.addLayer map fillExtrusionLayer
    log $ unsafeCoerce $ writeImpl fillExtrusionLayer

  instanceToInteractiveMap :: R.ReactInstance -> InteractiveMap
  instanceToInteractiveMap = unsafeCoerce

  mapRefHandler :: MapRef -> R.Ref R.ReactInstance -> Effect Unit
  mapRefHandler mapRef ref = do
    (content :: Maybe R.ReactInstance) <- R.getCurrentRef ref
    Ref.write (map instanceToInteractiveMap content) mapRef

  render :: R.ReactThis Props State -> MapRef -> R.Render
  render this mapRef = do
    { messages } <- R.getProps this
    { viewport } <- R.getState this
    pure
      $ R.createElement MapGL.mapGL
          ( un MapGL.Viewport viewport
              `disjointUnion`
                { onViewportChange:
                    mkEffectFn1
                      $ \vp ->
                          void $ R.setState this { viewport: vp }
                , onClick:
                    mkEffectFn1
                      $ \info -> do
                          launchAff_ $ Bus.write (PublicMsg $ OnClick info) messages
                , onLoad: mapOnLoadHandler mapRef
                , mapStyle
                , mapboxApiAccessToken
                , ref: R.fromEffect $ mapRefHandler mapRef
                , dragRotate: true
                , touchZoom: true
                , touchRotate: true
                }
          )
          []

mapStyle :: String
mapStyle = "mapbox://styles/mapbox/light-v10"

mapboxApiAccessToken :: String
mapboxApiAccessToken = "pk.eyJ1IjoiYmxpbmt5MzcxMyIsImEiOiJjamVvcXZtbGYwMXgzMzNwN2JlNGhuMHduIn0.ue2IR6wHG8b9eUoSfPhTuQ"

data AjaxError
  = HTTPStatus String
  | ResponseError String
  | DecodingError String

instance showAjaxError :: Show AjaxError where
  show = case _ of
    HTTPStatus s -> "HTTP status error" <> s
    ResponseError s -> "Response error" <> s
    DecodingError s -> "Decode JSON error" <> s

dataUrl :: String
dataUrl = "https://docs.mapbox.com/mapbox-gl-js/assets/earthquakes.geojson"

mapSourceId :: Mapbox.SourceId
mapSourceId = Mapbox.SourceId "composite"

mapLayerId :: Mapbox.LayerId
mapLayerId = Mapbox.LayerId "buildinglayer"

fillExtrusionHeight :: Mapbox.PaintProperty
fillExtrusionHeight =
  Mapbox.mkPaintProperty "fill-extrusion-height"
    $ Mapbox.SEArray
        [ Mapbox.SEString "interpolate"
        , Mapbox.SEArray [ Mapbox.SEString "linear" ]
        , Mapbox.SEArray [ Mapbox.SEString "zoom" ]
        , Mapbox.SENumber 15.0
        , Mapbox.SENumber 0.0
        , Mapbox.SENumber 15.5
        , Mapbox.SEArray [ Mapbox.SEString "get", Mapbox.SEString "height" ]
        ]

fillExtrusionBase :: Mapbox.PaintProperty
fillExtrusionBase =
  Mapbox.mkPaintProperty "fill-extrusion-base"
    $ Mapbox.SEArray
        [ Mapbox.SEString "interpolate"
        , Mapbox.SEArray [ Mapbox.SEString "linear" ]
        , Mapbox.SEArray [ Mapbox.SEString "zoom" ]
        , Mapbox.SENumber 15.0
        , Mapbox.SENumber 0.0
        , Mapbox.SENumber 15.5
        , Mapbox.SEArray [ Mapbox.SEString "get", Mapbox.SEString "min_height" ]
        ]

fillExtrusionColor :: Mapbox.PaintProperty
fillExtrusionColor = Mapbox.mkPaintProperty "fill-extrusion-color" $ Mapbox.SEString "#aaa"

fillExtrusionOpacity :: Mapbox.PaintProperty
fillExtrusionOpacity = Mapbox.mkPaintProperty "fill-extrusion-opacity" $ Mapbox.SENumber 0.6

paint :: Mapbox.Paint
paint =
  Mapbox.Paint
    [ fillExtrusionHeight
    , fillExtrusionColor
    , fillExtrusionBase
    , fillExtrusionOpacity
    ]

layout :: Mapbox.Layout
layout = Mapbox.FillExtrusionLayout { visibility: Mapbox.LayerVisible }

fillExtrusionLayer :: Mapbox.FillExtrusionLayer
fillExtrusionLayer =
  Mapbox.Layer
    { id: mapLayerId
    , source: mapSourceId
    , type: Mapbox.FillExtrusion
    , "source-layer": "building"
    , filter: Mapbox.SEArray [ Mapbox.SEString "==", Mapbox.SEString "extrude", Mapbox.SEString "true" ]
    , minzoom: 10.0
    , maxzoom: 24.0
    , paint
    , layout
    }
