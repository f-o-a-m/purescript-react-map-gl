module Map
  ( Messages(..)
  , MapMessages(..)
  , Commands(..)
  , mapClass
  ) where

import Prelude

import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Control.Lazy (fix)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (un)
import Data.Nullable (Nullable)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as C
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn1)
import GeoJson as GeoJson
import MapGL (ClickInfo, InteractiveMap, Viewport(..))
import MapGL as MapGL
import Mapbox as Mapbox
import React (ComponentDidMount, ComponentWillUnmount, ReactClass, ReactThis, Render, component, createElement, getProps, getState, setState) as R
import React.Ref (ReactInstance, Ref, fromEffect, getCurrentRef) as R
import Record (disjointUnion)
import Simple.JSON as JSON
import Unsafe.Coerce (unsafeCoerce)


data MapMessages
  = OnClick ClickInfo

type MapRef = Ref (Maybe InteractiveMap)

data Commands
  = SetFillExtrusionVisibilty Boolean

data Messages
  = IsInitialized (Bus.BusW Commands)
  | PublicMsg MapMessages

type Props =
  { messages :: Bus.BusW Messages
  , width :: Number
  , height :: Number
  }

type State =
  { command :: Bus.BusRW Commands
  , viewport :: Viewport
  }

mapClass :: R.ReactClass Props
mapClass = R.component "Map" \this -> do
  mapRef <- liftEffect $ Ref.new Nothing
  command <- Bus.make
  { width, height, messages } <- R.getProps this
  launchAff_ $ Bus.write (IsInitialized $ snd $ Bus.split command) messages
  pure
    { componentDidMount: componentDidMount this mapRef
    , componentWillUnmount: componentWillUnmount this mapRef
    , render: render this mapRef
    , state:
        { viewport: Viewport
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
      launchAff_ $ do
        props <- liftEffect $ R.getProps this
        Bus.kill (error "kill from componentWillUnmount") command

    componentDidMount :: R.ReactThis Props State -> MapRef -> R.ComponentDidMount
    componentDidMount this mapRef = do
      { command } <- R.getState this
      launchAff_ $ fix \loop -> do
        msg <- Bus.read command
        case msg of
          SetFillExtrusionVisibilty visible -> liftEffect $ do
            iMap <- Ref.read mapRef
            for_ (MapGL.getMap =<< iMap) \map ->
              -- make sure a `fillextrusion-layer` is already available at this point
              when (isJust $ Mapbox.getSource map mapSourceId) $
                Mapbox.setLayerVisibilty map mapLayerId visible
        loop

    mapOnLoadHandler
      :: MapRef
      -> Effect Unit
    mapOnLoadHandler mapRef = do
      iMap <- Ref.read mapRef
      for_ (MapGL.getMap =<< iMap) \map -> do
        -- set initial (empty) data
        let (source :: HeatmapData) = Mapbox.mkGeoJsonSource $ GeoJson.mkFeatureCollection []
        Mapbox.addSource map mapSourceId source
        -- initial fillextrusion layer
        Mapbox.addLayer map fillextrusionLayer
        -- load data
        launchAff_ $ do
          result <- getMapData
          case result of
            Right mapData -> do
              -- update data of fillextrusion layer
              liftEffect $ Mapbox.setData map mapSourceId mapData
            Left err -> do
              liftEffect $ C.error $ "error while loading earthquake data: " <> show err
              pure unit


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
      pure $ R.createElement MapGL.mapGL
              (un MapGL.Viewport viewport `disjointUnion`
              { onViewportChange: mkEffectFn1 $ \vp ->
                  void $ R.setState this {viewport: vp}
              , onClick: mkEffectFn1 $ \info -> do
                  launchAff_ $ Bus.write (PublicMsg $ OnClick info) messages
              , onLoad: mapOnLoadHandler mapRef
              , mapStyle
              , mapboxApiAccessToken
              , ref: R.fromEffect $ mapRefHandler mapRef
              , dragRotate: true
              , touchZoom: true
              , touchRotate: true
              })
              []

mapStyle :: String
mapStyle = "mapbox://styles/mapbox/dark-v9"

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

getMapData
  :: forall m
  . MonadAff m
  => m (Either AjaxError HeatmapDataFeatureCollection)
getMapData = liftAff do
  resp <- Affjax.get ResponseFormat.string dataUrl
  case resp of
    Left err ->
      pure $ Left $ ResponseError $ Affjax.printError err
    Right {body: str} ->
      pure $ either (Left <<< DecodingError <<< show) pure (JSON.readJSON str)

dataUrl :: String
dataUrl = "https://docs.mapbox.com/mapbox-gl-js/assets/earthquakes.geojson"

type HeatmapData = Mapbox.GeoJsonSource HeatmapDataFeatureCollection
type HeatmapDataFeatureCollection = GeoJson.FeatureCollection HeatmapDataFeature
type HeatmapDataFeature = GeoJson.Feature GeoJson.PointGeometry HeatmapDataProps

type HeatmapDataProps =
  { id :: String
  , mag :: Number
  , time :: Number
  , felt :: Nullable Number
  , tsunami :: Number
  }

mapSourceId :: Mapbox.SourceId
mapSourceId = Mapbox.SourceId "fillextrusion-source"

mapLayerId :: Mapbox.LayerId
mapLayerId = Mapbox.LayerId "fillextrusion-layer"

maxZoom :: Number
maxZoom = 9.0

fillExtrusionHeight :: Mapbox.PaintProperty
fillExtrusionHeight = Mapbox.mkPaintProperty "fill-extrusion-height"
  [ Mapbox.SEString "interpolate"
  , Mapbox.SEArray [Mapbox.SEString "linear"]
  , Mapbox.SEArray [Mapbox.SEString "zoom"]
  , Mapbox.SENumber 15.0
  , Mapbox.SENumber 0.0
  , Mapbox.SENumber 15.5
  , Mapbox.SEArray [Mapbox.SEString "get", Mapbox.SEString "height"]
  ]

fillExtrusionBase :: Mapbox.PaintProperty
fillExtrusionBase = Mapbox.mkPaintProperty "fill-extrusion-base"
  [ Mapbox.SEString "interpolate"
  , Mapbox.SEArray [Mapbox.SEString "linear"]
  , Mapbox.SEArray [Mapbox.SEString "zoom"]
  , Mapbox.SENumber 15.0
  , Mapbox.SENumber 0.0
  , Mapbox.SENumber 15.5
  , Mapbox.SEArray [Mapbox.SEString "get", Mapbox.SEString "min_height"]
  ]

fillExtrusionColor :: Mapbox.PaintProperty
fillExtrusionColor = Mapbox.mkPaintProperty "fill-extrusion-color"
  [ Mapbox.SEString "rgba(33,102,172,0)"
  ]

fillExtrusionOpacity :: Mapbox.PaintProperty
fillExtrusionOpacity = Mapbox.mkPaintProperty "fill-extrusion-opacity"
  [ Mapbox.SENumber 0.6
  ]

paint :: Mapbox.Paint
paint = Mapbox.Paint
  [ fillExtrusionHeight
  , fillExtrusionColor
  , fillExtrusionBase
  , fillExtrusionOpacity
  ]

layout :: Mapbox.Layout
layout = Mapbox.FillExtrusionLayout { visibility: Mapbox.LayerNone }

fillextrusionLayer :: Mapbox.Layer
fillextrusionLayer = Mapbox.Layer
  { id: mapLayerId
  , source: mapSourceId
  , type: Mapbox.FillExtrusion
  , minzoom: 15.0
  , maxzoom: maxZoom
  , paint
  , layout
  }