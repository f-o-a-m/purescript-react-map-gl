module MapComponent
  ( MapQuery(SetViewport, AskViewport)
  , MapProps
  , MapMessages(..)
  , mapComponent
  ) where

import Prelude

import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Lazy (fix)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as C
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn1)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import MapGL (ClickInfo, InteractiveMap, Map, MapboxLayerId(..), MapboxSourceId(..), Viewport(..), addMapboxLayer, addMapboxSource, getMap, setMapboxSourceData)
import MapGL as MapGL
import MapGL.Heatmap (HeatmapWeightProperty(..))
import MapGL.Heatmap as Heatmap
import Partial.Unsafe (unsafeCrashWith)
import React as R
import ReactDOM (render) as RDOM
import Record (disjointUnion)
import Simple.JSON as JSON
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window


type MapState = Maybe (Bus.BusW Commands)

type MapProps = Unit

data MapQuery a
  = Initialize a
  | SetViewport Viewport a
  | AskViewport (Viewport -> a)
  | HandleMessages Messages a

data MapMessages
  = OnViewportChange Viewport
  | OnClick ClickInfo
  | OnLoad

type MapRef = Ref (Maybe InteractiveMap) 

mapSourceId :: MapboxSourceId
mapSourceId = MapboxSourceId "heatmap-source"

mapLayerId :: MapboxLayerId
mapLayerId = MapboxLayerId "heatmap-layer"

type MapSourceProps = (mag::Number)


mapComponent :: forall m. MonadAff m => H.Component HH.HTML MapQuery MapProps MapMessages m
mapComponent =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState :: MapState
  initialState = Nothing

  render :: MapState -> H.ComponentHTML MapQuery
  render = const $ HH.div [ HP.ref (H.RefLabel "map") ] []

  eval :: MapQuery ~> H.ComponentDSL MapState MapQuery MapMessages m
  eval = case _ of
    Initialize next -> do
      mapRef <- H.liftEffect $ Ref.new Nothing
      H.getHTMLElementRef (H.RefLabel "map") >>= case _ of
        Nothing -> unsafeCrashWith "There must be an element with ref `map`"
        Just el' -> do
          win <- liftEffect window
          width <- liftEffect $ toNumber <$> Window.innerWidth win
          height <- liftEffect $ toNumber <$> Window.innerHeight win
          messages <- liftAff Bus.make
          liftEffect $ void $ RDOM.render (R.createLeafElement mapClass { messages: snd $ Bus.split messages, width, height, mapRef}) (HTMLElement.toElement el')
          H.subscribe $ H.eventSource (\emit -> launchAff_ $ fix \loop -> do
              Bus.read messages >>= emit >>> liftEffect
              loop
            )
            (Just <<< flip HandleMessages ES.Listening)
      pure next
    HandleMessages msg next -> do
      case msg of
        IsInitialized bus -> H.put $ Just bus
        PublicMsg msg' -> H.raise msg'
      pure next
    SetViewport vp next -> do
      mbBus <- H.get
      case mbBus of
        Nothing -> unsafeCrashWith "At this point bus must be in state from eval SetViewport"
        Just bus -> do
          liftAff $ Bus.write (SetViewport' vp) bus
      pure next
    AskViewport reply -> do
      mbBus <- H.get
      case mbBus of
        Nothing -> unsafeCrashWith "At this point bus must be in state from eval AskViewport"
        Just bus -> do
          var <- liftAff AVar.empty
          liftAff $ Bus.write (AskViewport' var) bus
          vp <- liftAff $ AVar.take var
          pure $ reply vp

data Commands
  = SetViewport' Viewport
  | AskViewport' (AVar Viewport)

data Messages
  = IsInitialized (Bus.BusW Commands)
  | PublicMsg MapMessages

type Props =
  { messages :: Bus.BusW Messages
  , width :: Number
  , height :: Number
  , mapRef :: MapRef
  }

type State =
  { command :: Bus.BusRW Commands
  , viewport :: Viewport
  }

mapClass :: R.ReactClass Props
mapClass = R.component "Map" \this -> do
  command <- Bus.make
  { messages, width, height, mapRef } <- R.getProps this
  launchAff_ $ Bus.write (IsInitialized $ snd $ Bus.split command) messages
  pure 
    { componentDidMount: componentDidMount this
    , componentWillUnmount: componentWillUnmount this
    , render: render this
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
    componentWillUnmount :: R.ReactThis Props State -> R.ComponentWillUnmount
    componentWillUnmount this = R.getState this >>= \{ command } ->
      launchAff_ $ do
        {mapRef} <- liftEffect $ R.getProps this
        liftEffect $ Ref.write Nothing mapRef
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

    mapOnLoadHandler 
      :: MapRef
      -> Effect Unit
    mapOnLoadHandler mapRef = do
      C.log "mapOnLoadHandler "
      iMap <- Ref.read mapRef
      for_ (getMap =<< iMap) \map -> do
        -- set initial (empty) data
        -- addMapboxSource map mapSourceId {type: "geojson", data: {}}
        -- initial heatmap layer
        -- addMapboxLayer map $ Heatmap.mkHeatmapLayer mapLayerId mapSourceId (HeatmapWeightProperty "mag")
        -- load data
        launchAff_ $ do 
          C.log "load data"
          result <- getData 
          case result of
            Right d -> do 
              C.log "add data"
              -- set initial data
              liftEffect $ addMapboxSource map mapSourceId {type: "geojson", data: d}
              C.log "add layer"
              -- initial heatmap layer
              liftEffect$ addMapboxLayer map $ Heatmap.mkHeatmapLayer mapLayerId mapSourceId (HeatmapWeightProperty "mag")
              -- liftEffect $ setMapData map d.features
            Left err -> do
              liftEffect $ C.error $ "error while loading earthquake data: "
              pure unit
      pure unit
  
    mapRefHandler :: MapRef -> (Nullable R.ReactRef)-> Effect Unit
    mapRefHandler mapRef ref = do
      _ <- Ref.write (Nullable.toMaybe $ unsafeCoerce ref ) mapRef
      pure unit

    setMapData :: forall r. Map -> (Array (|r)) -> Effect Unit
    setMapData map features = do 
      -- traceM features
      setMapboxSourceData map mapSourceId features

    render :: R.ReactThis Props State -> R.Render
    render this = do
      { messages, mapRef } <- R.getProps this
      { viewport } <- R.getState this
      pure $ R.createElement MapGL.mapGL
              (un MapGL.Viewport viewport `disjointUnion`
              { onViewportChange: mkEffectFn1 $ \newVp -> do
                  launchAff_ $ Bus.write (PublicMsg $ OnViewportChange newVp) messages
                  void $ R.modifyState this _{viewport = newVp}
              , onClick: mkEffectFn1 $ \info -> do
                  launchAff_ $ Bus.write (PublicMsg $ OnClick info) messages
              , onLoad: mapOnLoadHandler mapRef
              , mapStyle
              , mapboxApiAccessToken
              , ref: mkEffectFn1 $ mapRefHandler mapRef
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

getData 
  :: forall m 
  . MonadAff m 
  => m (Either AjaxError DataSource)
getData = liftAff do
  {body, status} <- Affjax.get ResponseFormat.string dataUrl
  if (status /= StatusCode 200) 
    then
      pure $ Left $ HTTPStatus $ show status
    else
      case body of 
        Left err ->
          pure $ Left $ ResponseError$ Affjax.printResponseFormatError err
        Right str -> 
          pure $ either (Left <<< DecodingError <<< show) pure (JSON.readJSON str)


dataUrl :: String 
dataUrl = "https://docs.mapbox.com/mapbox-gl-js/assets/earthquakes.geojson"

type DataSource = 
  { features :: Array 
      { type:: String
      , properties :: { id:: String
                      , mag::Number
                      , time::Number
                      , felt::(Nullable Number)
                      , tsunami::Number 
                      } 
      , geometry :: { type::String
                    , coordinates::Array Number 
                    }
      }
  }

-- mkHeatmapLayer layerId sourceId weightProperty = do
--     let maxZoomLevel = 9
--     { id: layerId
--     , source: sourceId
--     , maxzoom: maxZoomLevel
--     , type: "heatmap"
--     , paint: 
--         { -- Increase the heatmap weight based on a property.
--           -- This property has to be defined in every Feature of a FeatureCollection
--           "heatmap-weight": 
--             [ "interpolate"
--             , ["linear"]
--             , ["get", weightProperty]
--             , 0, 0
--             , 6, 1
--             ]
--             -- Increase the heatmap color weight weight by zoom level
--             -- heatmap-intensity is a multiplier on top of heatmap-weight
--             -- https://docs.mapbox.com/mapbox-gl-js/style-spec/#paint-heatmap-heatmap-intensity
--         , "heatmap-intensity": 
--             [ "interpolate"
--             , ["linear"]
--             , ["zoom"]
--             , 0, 1
--             , maxZoomLevel, 3
--             ]
--         -- Color ramp for heatmap.  Domain is 0 (low) to 1 (high).
--         -- Begin color ramp at 0-stop with a 0-transparancy color
--         -- to create a blur-like effect.
--         -- https://docs.mapbox.com/mapbox-gl-js/style-spec/#paint-heatmap-heatmap-color
--         , "heatmap-color": 
--             [ "interpolate"
--             , ["linear"]
--             , ["heatmap-density"]
--             , 0, "rgba(33,102,172,0)"
--             , 0.2, "rgb(103,169,207)"
--             , 0.4, "rgb(209,229,240)"
--             , 0.6, "rgb(253,219,199)"
--             , 0.8, "rgb(239,138,98)"
--             , 0.9, "rgb(255,201,101)"
--             ]
--         , -- Adjust the heatmap radius by zoom level
--           -- https://docs.mapbox.com/mapbox-gl-js/style-spec/#paint-heatmap-heatmap-radius
--           "heatmap-radius": 
--             [ "interpolate"
--             , ["linear"]
--             , ["zoom"]
--             , --zoom is 0 -> radius will be 2px
--               0, 2
--             , -- zoom is 9 -> radius will be 20px
--               maxZoomLevel, 20
--             ]
--         , -- Transition from heatmap to circle layer by zoom level
--           -- https://docs.mapbox.com/mapbox-gl-js/style-spec/#paint-heatmap-heatmap-opacity
--           "heatmap-opacity": 
--           [ "interpolate"
--           , ["linear"]
--           , ["zoom"]
--           , -- zoom is 7 (or less) -> opacity will be 1
--             7, 1,
--             -- zoom is 9 (or greater) -> opacity will be 0
--             maxZoomLevel, 0
--           ]
--         }
--     }