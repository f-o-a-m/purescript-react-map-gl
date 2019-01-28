module MapComponent
  ( MapQuery(SetViewport, AskViewport)
  , MapProps
  , MapMessages(..)
  , mapComponent
  ) where

import Prelude

import Control.Lazy (fix)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (un)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple (snd)
import Debug.Trace as Debug
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn1)
import GeoJson (Feature(..), FeatureCollection(..))
import GeoJson (GeoJson(..))
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import MapGL (ClickInfo, InteractiveMap, Map, MapboxSourceId(..), Viewport(..), getMap, getMapboxSource, setMapboxSourceData)
import MapGL as MapGL
import Partial.Unsafe (unsafeCrashWith)
import React as R
import ReactDOM (render) as RDOM
import Record (disjointUnion)
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
mapSourceId = MapboxSourceId "earthquake-source-id"

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

    mapOnLoadHandler :: Effect Unit
    mapOnLoadHandler =
      -- TODO(sectore) Load data from https://docs.mapbox.com/mapbox-gl-js/assets/earthquakes.geojson
      log "log onload "

    mapRefHandler :: MapRef -> (Nullable R.ReactRef)-> Effect Unit
    mapRefHandler mapRef ref = do
      _ <- Ref.write (Nullable.toMaybe $ unsafeCoerce ref ) mapRef
      pure unit

    setMapData :: forall p. InteractiveMap -> (Array (Feature p)) -> Effect Unit
    setMapData iMap features = do 
      for_ (getMap iMap) \map -> do
        source <- getMapboxSource map mapSourceId
        -- TODO(sectore) Add real GeoJson data - mocking data is just for debugging
        let geojson = GeoJsonFeatureCollection {features: [{properties: Just {mag: 2.3}}]}
        setMapboxSourceData source geojson

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
              , onLoad: mapOnLoadHandler
              , mapStyle
              , mapboxApiAccessToken
              , ref: mkEffectFn1 $ mapRefHandler mapRef
              })
              []

mapStyle :: String
mapStyle = "mapbox://styles/mapbox/dark-v9"

mapboxApiAccessToken :: String
mapboxApiAccessToken = "pk.eyJ1IjoiYmxpbmt5MzcxMyIsImEiOiJjamVvcXZtbGYwMXgzMzNwN2JlNGhuMHduIn0.ue2IR6wHG8b9eUoSfPhTuQ"
