module Main where

import Prelude

import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import MapComponent (MapMessages(..), MapQuery(..), mapComponent)
import MapGL (Viewport(..))
import WebMercator.LngLat (LngLat)
import WebMercator.LngLat as LngLat

type State = {}

data Query a
  = GoTo LngLat a
  | HandleMapUpdate MapMessages a

data MapSlot = MapSlot
derive instance eqMapSlot :: Eq MapSlot
derive instance ordMapSlot :: Ord MapSlot

ui 
  :: forall m
  . MonadAff m
  => H.Component HH.HTML Query Unit Void m
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = {}

  render :: State -> H.ParentHTML Query MapQuery MapSlot m
  render _ =
    HH.div_
      [ HH.slot MapSlot mapComponent unit $ Just <<< H.action <<< HandleMapUpdate
      , HH.button
          [ HP.class_ (HH.ClassName "goto")
          , HE.onClick (HE.input_ $ GoTo $ LngLat.make { lng: 44.81647122397245, lat: 41.661632116606455 })
          ]
          [ HH.text "GoTo Tbilisi" ]
      ]

  eval :: Query ~> H.ParentDSL State Query MapQuery MapSlot Void m
  eval (GoTo lnglat next) = do
    mbVp <- H.query MapSlot $ H.request AskViewport
    for_ mbVp \vp -> do
      let nextVp = over Viewport (_{ latitude = LngLat.lat lnglat, longitude = LngLat.lng lnglat, zoom = 12.0}) vp
      H.query MapSlot $ H.action $ SetViewport nextVp
    pure next
  eval (HandleMapUpdate msg next) = do
    case msg of
      OnViewportChange vp -> H.liftEffect $ log $ show vp
      OnClick info -> H.liftEffect $ log $ show info.lngLat
    pure next

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
