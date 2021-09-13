module Container
  ( State
  , Slot
  , mapComponent
  ) where

import Prelude
import Control.Lazy (fix)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (forkAff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Map as Map
import Partial.Unsafe (unsafeCrashWith)
import React as R
import ReactDOM (render) as RDOM
import Web.HTML (window)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Control.Monad.Rec.Class (forever)

type Slot f
  = H.Slot f Map.MapMessages

type State
  = { bus :: Maybe (Bus.BusW Map.Commands)
    , showFillExtrusion :: Boolean
    }

data Action
  = Initialize
  | HandleMessages Map.Messages
  | ToggleFillExtrusion

mapComponent :: forall f i m. MonadAff m => H.Component f i Map.MapMessages m
mapComponent =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval
    }
  where
  initialState :: State
  initialState = { bus: Nothing, showFillExtrusion: false }

render :: forall s m. State -> H.ComponentHTML Action s m
render st =
  HH.div
    [ HP.class_ $ HH.ClassName "map-wrapper" ]
    [ HH.div [ HP.ref (H.RefLabel "map") ] []
    , HH.button
      [ HP.class_ $ HH.ClassName "btn-toggle"
      , HE.onClick $ \_ -> ToggleFillExtrusion
      ]
      [ HH.text $
          (if st.showFillExtrusion
              then "Hide"
              else "Show")
          <> " fillextrusion"
      ]
    ]

eval :: forall f i s m. MonadAff m => H.HalogenQ f Action i ~> H.HalogenM State Action s Map.MapMessages m
eval =
  H.mkEval
    $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
  where
    handleAction :: Action -> H.HalogenM State Action s Map.MapMessages m Unit
    handleAction = case _ of
      Initialize -> do
        H.getHTMLElementRef (H.RefLabel "map") >>= case _ of
          Nothing -> unsafeCrashWith "There must be an element with ref `map`"
          Just el' -> do
            win <- liftEffect window
            width <- liftEffect $ toNumber <$> Window.innerWidth win
            height <- liftEffect $ toNumber <$> Window.innerHeight win
            messages <- liftAff Bus.make
            let (Tuple messagesR messagesW) = Bus.split messages
            liftEffect $ void $ RDOM.render (R.createLeafElement Map.mapClass { messages: messagesW, width, height}) (HTMLElement.toElement el')
            { emitter, listener } <- H.liftEffect HS.create
            void $ H.subscribe emitter
            void
              $ H.liftAff
              $ forkAff
              $ forever do
                Bus.read messagesR >>= (\a -> H.liftEffect $ HS.notify listener (HandleMessages a))

      HandleMessages msg -> do
        case msg of
          Map.PublicMsg msg' -> H.raise msg'
          Map.IsInitialized bus -> H.modify_ _{bus = Just bus}
      ToggleFillExtrusion -> do
        {bus: mbBus, showFillExtrusion} <- H.get
        let visible = not showFillExtrusion
        for_ mbBus \bus ->
          liftAff $ Bus.write (Map.SetFillExtrusionVisibilty visible) bus
        H.modify_ _{showFillExtrusion = visible}