module Main where

import Prelude
import Container as Container
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Map as Map
import Type.Proxy

type State
  = Unit

data Action
  = HandleMapUpdate Map.MapMessages

type Slots f
  = ( map :: Container.Slot f Unit
    )

_map :: Proxy "map"
_map = Proxy

ui 
  :: forall f m
  . MonadAff m
  => H.Component f State Void m
ui =
  H.mkComponent
    { initialState: const unit
    , render
    , eval:
        H.mkEval
          $ H.defaultEval { handleAction = handleAction }
    }
  where
    render :: State -> H.ComponentHTML Action (Slots f) m
    render _ =
      HH.div_
        [ HH.slot _map unit Container.mapComponent unit HandleMapUpdate
        ]

    handleAction :: forall o. Action -> H.HalogenM State Action (Slots f) o m Unit
    handleAction (HandleMapUpdate msg) = do
      case msg of
        Map.OnClick info -> liftEffect $ log $ show info.lngLat

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ui unit body
