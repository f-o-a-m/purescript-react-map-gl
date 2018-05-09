module MapComponent
  ( MapQuery(SetViewport, AskViewport)
  , MapProps
  , MapMessages(..)
  , mapComponent
  ) where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Aff (error, launchAff_)
import Control.Monad.Aff.AVar (AVar, takeVar, putVar, makeEmptyVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlElementToElement)
import DOM.HTML.Window as Window
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Halogen (liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import MapGL (ClickInfo, Viewport)
import MapGL as MapGL
import Partial.Unsafe (unsafeCrashWith)
import React as R
import ReactDOM (render) as RDOM


type MapState = Maybe (Bus.BusW Commands)

type MapProps = Unit

data MapQuery a
  = Initialize a
  | SetViewport Viewport a
  | AskViewport (Viewport -> a)
  | HandleMessages Messages a

data MapMessages
  = OnChangeViewport Viewport
  | OnClick ClickInfo

mapComponent :: forall eff m. MonadAff (dom :: DOM, avar :: AVAR | eff) m => H.Component HH.HTML MapQuery MapProps MapMessages m
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
      H.getHTMLElementRef (H.RefLabel "map") >>= case _ of
        Nothing -> unsafeCrashWith "There must be an element with ref `map`"
        Just el' -> do
          win <- liftEff window
          width <- liftEff $ Window.innerWidth win
          height <- liftEff $ Window.innerHeight win
          messages <- liftAff Bus.make
          liftEff $ void $ RDOM.render (R.createFactory mapClass { messages: snd $ Bus.split messages, width, height}) (htmlElementToElement el')
          H.subscribe $ H.eventSource (\emit -> launchAff_ $ fix \loop -> do
              Bus.read messages >>= emit >>> liftEff
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
          var <- liftAff makeEmptyVar
          liftAff $ Bus.write (AskViewport' var) bus
          vp <- liftAff $ takeVar var
          pure $ reply vp

data Commands
  = SetViewport' Viewport
  | AskViewport' (AVar Viewport)

data Messages
  = IsInitialized (Bus.BusW Commands)
  | PublicMsg MapMessages

type Props =
  { messages :: Bus.BusW Messages
  , width :: Int
  , height :: Int
  }

type State =
  { command :: Bus.BusRW Commands
  , viewport :: MapGL.Viewport
  }

mapClass :: R.ReactClass Props
mapClass = R.createClass spec
    { componentDidMount = componentDidMount
    , componentWillUnmount = componentWillUnmount
    }
  where
    componentWillUnmount :: forall eff. R.ComponentWillUnmount Props State (avar :: AVAR | eff)
    componentWillUnmount this = R.readState this >>= \{ command } ->
      launchAff_ $ do
        props <- liftEff $ R.getProps this
        Bus.kill (error "kill from componentWillUnmount") command

    componentDidMount :: forall eff. R.ComponentDidMount Props State (avar :: AVAR, dom ∷ DOM |eff)
    componentDidMount this = do
      { command } <- R.readState this
      launchAff_ $ fix \loop -> do
        msg <- Bus.read command
        case msg of
          SetViewport' vp -> liftEff $ R.transformState this _{viewport = vp}
          AskViewport' var -> liftEff (R.readState this) >>= \{viewport} -> putVar viewport var
        loop

    spec :: forall eff. R.ReactSpec Props State R.ReactElement (dom :: DOM, avar :: AVAR | eff)
    spec = R.spec' initialState render
    
    render :: forall eff. R.Render Props State R.ReactElement eff
    render this = do
      { messages } <- R.getProps this
      { viewport } <- R.readState this
      pure
        $ R.createFactory MapGL.mapGL
        $ MapGL.mkProps viewport
          { onChangeViewport: mkEffFn1 $ \newVp -> do
              launchAff_ $ Bus.write (PublicMsg $ OnChangeViewport newVp) messages
              void $ R.transformState this _{viewport = newVp}
          , onClick: mkEffFn1 $ \info -> do
              launchAff_ $ Bus.write (PublicMsg $ OnClick info) messages
          , mapStyle: mapStyle
          , mapboxApiAccessToken: mapboxApiAccessToken
          }

    initialState :: forall eff. R.GetInitialState Props State (dom :: DOM, avar :: AVAR | eff)
    initialState this = do
      command <- Bus.make
      { messages, width, height } <- R.getProps this
      launchAff_ $ Bus.write (IsInitialized $ snd $ Bus.split command) messages
      pure
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


mapStyle :: String
mapStyle = "mapbox://styles/mapbox/dark-v9"

mapboxApiAccessToken :: String
mapboxApiAccessToken = "pk.eyJ1IjoiYmxpbmt5MzcxMyIsImEiOiJjamVvcXZtbGYwMXgzMzNwN2JlNGhuMHduIn0.ue2IR6wHG8b9eUoSfPhTuQ"
