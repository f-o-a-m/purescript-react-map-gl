module Container
  ( Query(..)
  , Slot
  , mapComponent
  ) where

import Prelude

import Control.Lazy (fix)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (launchAff_, forkAff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Map (mapClass, MapMessages, Messages(..), Commands(..))
import MapGL (Viewport)
import Partial.Unsafe (unsafeCrashWith)
import React as R
import ReactDOM (render) as RDOM
import Web.HTML (window)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Control.Monad.Rec.Class (forever)

type Slot = H.Slot Query MapMessages

type State = Maybe (Bus.BusW Commands)

data Query a
  = SetViewport Viewport a
  | AskViewport (Viewport -> a)

data Action
  = Initialize
  | HandleMessages Messages

mapComponent :: forall i m. MonadAff m => H.Component Query i MapMessages m
mapComponent =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval
    }

render :: forall s m. State -> H.ComponentHTML Action s m
render = const $ HH.div [ HP.ref (H.RefLabel "map") ] []

eval :: forall i s m. MonadAff m => H.HalogenQ Query Action i ~> H.HalogenM State Action s MapMessages m
eval = H.mkEval $ H.defaultEval 
  { handleQuery = handleQuery
  , handleAction = handleAction
  , initialize = Just Initialize
  }
  where
    handleQuery :: forall a. Query a -> H.HalogenM State Action s MapMessages m (Maybe a)
    handleQuery = case _ of
      SetViewport vp next -> do
        mbBus <- H.get
        case mbBus of
          Nothing -> unsafeCrashWith "At this point bus must be in state from eval SetViewport"
          Just bus -> do
            liftAff $ Bus.write (SetViewport' vp) bus
        pure $ Just next
      AskViewport reply -> do
        mbBus <- H.get
        case mbBus of
          Nothing -> unsafeCrashWith "At this point bus must be in state from eval AskViewport"
          Just bus -> do
            var <- liftAff AVar.empty
            liftAff $ Bus.write (AskViewport' var) bus
            vp <- liftAff $ AVar.take var
            pure $ Just $ reply vp

    handleAction :: Action -> H.HalogenM State Action s MapMessages m Unit
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
            liftEffect $ void $ RDOM.render 
              ( R.createLeafElement mapClass 
                  { messages: messagesW
                  , width, height
                  }
              ) (HTMLElement.toElement el')
            { emitter, listener } <- H.liftEffect HS.create
            void $ H.subscribe emitter
            void
              $ H.liftAff
              $ forkAff
              $ forever do
                Bus.read messagesR >>= (\a -> H.liftEffect $ HS.notify listener (HandleMessages a))

      HandleMessages msg -> do
        case msg of
          IsInitialized bus -> H.put $ Just bus
          PublicMsg msg' -> H.raise msg'