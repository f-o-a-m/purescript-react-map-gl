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
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as C
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn1)
import GeoJson as GeoJson
import MapGL (ClickInfo, InteractiveMap, Viewport(..))
import MapGL as MapGL
import Mapbox as Mapbox
import React as R
import React.Ref as R
import Record (disjointUnion)
import Simple.JSON as JSON
import Unsafe.Coerce (unsafeCoerce)


type MapRef = Ref (Maybe InteractiveMap)

data Commands
  = ToggleHeatmap'

data Messages
  = IsInitialized (Bus.BusW Commands)
  | PublicMsg MapMessages

data MapMessages
  = OnClick ClickInfo

type Props =
  { messages :: Bus.BusW Messages
  , width :: Number
  , height :: Number
  }

type State =
  { command :: Bus.BusRW Commands
  , viewport :: Viewport
  , showHeatmap :: Boolean
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
        , showHeatmap: true
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
          ToggleHeatmap' -> liftEffect $ do
            {showHeatmap} <- R.getState this
            let visible = not showHeatmap
            iMap <- Ref.read mapRef
            for_ (MapGL.getMap =<< iMap) \map -> do
              Mapbox.setLayerVisibilty map mapLayerId visible
            R.setState this {showHeatmap: visible}
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
        -- initial heatmap layer
        Mapbox.addLayer map heatmapLayer
        -- load data
        launchAff_ $ do
          result <- getMapData
          case result of
            Right mapData -> do
              -- update data of heatmap layer
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
dataUrl = "https://gist.githubusercontent.com/kejace/1cb8711792da3e1fc309a40f77ea3266/raw/dd7f1e19d4a9056c15a898a22e04c3a2c7c7cbd2/signals2.geojson"

type HeatmapData = Mapbox.GeoJsonSource HeatmapDataFeatureCollection
type HeatmapDataFeatureCollection = GeoJson.FeatureCollection HeatmapDataFeature
type HeatmapDataFeature = GeoJson.Feature GeoJson.PointGeometry HeatmapDataProps

type HeatmapDataProps =
  { tokenId :: String
  , radius :: Number
  , stake :: Number
  }

mapSourceId :: Mapbox.SourceId
mapSourceId = Mapbox.SourceId "heatmap-source"

mapLayerId :: Mapbox.LayerId
mapLayerId = Mapbox.LayerId "heatmap-layer"

maxZoom :: Number
maxZoom = 20.0

-- Color ramp for heatmap.  Domain is 0 (low) to 1 (high).
-- Begin color ramp at 0-stop with a 0-transparancy color
-- to create a blur-like effect.
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#paint-heatmap-heatmap-color
heatmapColor :: Mapbox.PaintProperty
heatmapColor = Mapbox.mkPaintProperty "heatmap-color"
  [ Mapbox.SEString "interpolate"
  , Mapbox.SEArray [Mapbox.SEString "linear"]
  , Mapbox.SEArray [Mapbox.SEString "heatmap-density"]
  , Mapbox.SENumber 0.0
  , Mapbox.SEString "rgba(33,102,172,0)" -- color for zero values == kinda transparent background
  , Mapbox.SENumber 0.2
  , Mapbox.SEString "rgb(103,169,207)"
  , Mapbox.SENumber 0.4
  , Mapbox.SEString "rgb(209,229,240)"
  , Mapbox.SENumber 0.6
  , Mapbox.SEString "rgb(253,219,199)"
  , Mapbox.SENumber 0.8
  , Mapbox.SEString "rgb(247,193,171)"
  , Mapbox.SENumber 0.9
  , Mapbox.SEString "rgb(255,201,101)"
  , Mapbox.SENumber 0.93
  , Mapbox.SEString "rgb(255,207,117)"
  , Mapbox.SENumber 0.95
  , Mapbox.SEString "rgb(255,210,128)"
  , Mapbox.SENumber 0.97
  , Mapbox.SEString "rgb(255,214,138)"
  , Mapbox.SENumber 1.0
  , Mapbox.SEString "rgb(255,219,153)"
  ]

-- Transition from heatmap to circle layer by zoom level
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#paint-heatmap-heatmap-opacity
heatmapOpacity :: Mapbox.PaintProperty
heatmapOpacity = Mapbox.mkPaintProperty "heatmap-opacity"
  [ Mapbox.SEString "interpolate"
  , Mapbox.SEArray [Mapbox.SEString "linear"]
  , Mapbox.SEArray [Mapbox.SEString "zoom"]
  -- min. values of `zoom`, `heatmap-opacity` pair
  , Mapbox.SENumber 0.0 -- zoom
  , Mapbox.SENumber 1.0 -- opacity
  -- max. values of `zoom`, `heatmap-opacity` pair
  , Mapbox.SENumber 20.0 -- zoom
  , Mapbox.SENumber 0.0 -- opacity
  ]

-- Adjust the heatmap radius by zoom level
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#paint-heatmap-heatmap-radius
heatmapRadius :: Mapbox.PaintProperty
heatmapRadius = Mapbox.mkPaintProperty "heatmap-radius"
  [ Mapbox.SEString "interpolate"
  , Mapbox.SEArray [Mapbox.SEString "exponential", Mapbox.SENumber 1.75]
  , Mapbox.SEArray [Mapbox.SEString "zoom"]
  , Mapbox.SENumber 1.0
  , over "radius" 250.0
  , Mapbox.SENumber 6.0
  , over "radius" 150.0
  , Mapbox.SENumber 12.0
  , over "radius" 9.0
  , Mapbox.SENumber 20.0
  , over "radius" 0.1
  ]
    where
  over r n =
    Mapbox.SEArray
      [ Mapbox.SEString "/"
      , Mapbox.SEArray
        [ Mapbox.SEString "get"
        , Mapbox.SEString r
        ]
      , Mapbox.SENumber n
      ]

-- Increase the heatmap weight based on a property.
-- This property has to be defined in a `feature` of a `FeatureCollection`
heatmapWeight :: Mapbox.PaintProperty
heatmapWeight = Mapbox.mkPaintProperty "heatmap-weight"
  [ -- interpolate expression
    -- https://docs.mapbox.com/mapbox-gl-js/style-spec/#expressions-interpolate
    Mapbox.SEString "interpolate"
  , Mapbox.SEArray [Mapbox.SEString "exponential", Mapbox.SENumber 1.75]
  , Mapbox.SEArray [Mapbox.SEString "zoom"]
  , Mapbox.SENumber 1.0
  , linear "stake" 0.2 10000000.0
  , Mapbox.SENumber 6.0
  , linear "stake" 0.5 1000000.0
  , Mapbox.SENumber 12.0
  , linear "stake" 3.0 100000.0
  , Mapbox.SENumber 20.0
  , linear "stake" 4.0 1000.0
  ]
    where
  linear s c n =
    Mapbox.SEArray
      [ Mapbox.SEString "+"
      , Mapbox.SENumber c
      , Mapbox.SEArray
        [ Mapbox.SEString "/"
        , Mapbox.SEArray
          [ Mapbox.SEString "get"
          , Mapbox.SEString s
          ]
        , Mapbox.SENumber n
        ]
      ]

-- Increase the heatmap color weight weight by zoom level
-- heatmap-intensity is a multiplier on top of heatmap-weight
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#paint-heatmap-heatmap-intensity
heatmapIntensity :: Mapbox.PaintProperty
heatmapIntensity = Mapbox.mkPaintProperty "heatmap-intensity"
  [ Mapbox.SEString "interpolate"
  , Mapbox.SEArray [Mapbox.SEString "linear"]
  , Mapbox.SEArray [Mapbox.SEString "zoom"]
  , Mapbox.SENumber 1.0
  , Mapbox.SENumber 1.2
  , Mapbox.SENumber 20.0
  , Mapbox.SENumber 0.0
  ]

paint :: Mapbox.Paint
paint = Mapbox.Paint
  [ heatmapWeight
  , heatmapIntensity
  , heatmapColor
  , heatmapRadius
  , heatmapOpacity
  ]

layout :: Mapbox.Layout
layout = Mapbox.HeatmapLayout { visibility: Mapbox.LayerVisible }

heatmapLayer :: Mapbox.Layer
heatmapLayer = Mapbox.Layer
  { id: mapLayerId
  , source: mapSourceId
  , type: Mapbox.Heatmap
  , minzoom: 0.0
  , maxzoom: maxZoom
  , paint
  , layout
  }
