module Main where

import Prelude
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Container as Container
import Map as Map
import MapGL (Viewport(..))
import WebMercator.LngLat (LngLat)
import WebMercator.LngLat as LngLat

type State
  = {}

data Action
  = GoTo LngLat
  | HandleMapUpdate Map.MapMessages

type Slots
  = ( map :: Container.Slot Unit
    )

_map :: SProxy "map"
_map = SProxy

ui ::
  forall f m.
  MonadAff m =>
  H.Component HH.HTML f Unit Void m
ui =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: State
  initialState = {}

  render :: State -> H.ComponentHTML Action Slots m
  render _ =
    HH.div_
      [ HH.slot _map unit Container.mapComponent unit (Just <<< HandleMapUpdate)
      , HH.button
          [ HP.class_ (HH.ClassName "goto")
          , HE.onClick (\_ -> Just $ GoTo $ LngLat.make { lng: 44.81647122397245, lat: 41.661632116606455 })
          ]
          [ HH.text "GoTo Tbilisi" ]
      ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction (GoTo lnglat) = do
  mvp <- H.query _map unit $ H.request Container.AskViewport
  for_ mvp \(Viewport vp) -> do
    let
      nextVp = Viewport $ vp { latitude = LngLat.lat lnglat, longitude = LngLat.lng lnglat, zoom = 12.0 }
    H.query _map unit $ H.tell $ Container.SetViewport nextVp

handleAction (HandleMapUpdate msg) = do
  case msg of
    Map.OnViewportChange vp -> H.liftEffect $ log $ show vp
    Map.OnClick info -> H.liftEffect $ log $ show info.lngLat

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ui unit body
