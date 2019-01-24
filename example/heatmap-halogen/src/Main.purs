module Main where

import Prelude

import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import MapComponent (MapMessages(..), MapQuery, mapComponent)

type State = {}

data Query a
  = HandleMapUpdate MapMessages a

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
      ]

  eval :: Query ~> H.ParentDSL State Query MapQuery MapSlot Void m
  eval (HandleMapUpdate msg next) = do
    case msg of
      OnViewportChange vp -> H.liftEffect $ log $ show vp
      OnClick info -> H.liftEffect $ log $ show info.lngLat
      OnLoad -> H.liftEffect $ log "onload from map"
    pure next

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
