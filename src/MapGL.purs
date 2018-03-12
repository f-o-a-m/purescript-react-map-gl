module MapGL where

import Prelude
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import React as R

-- | `Viewport` is the most basic state type for the map component.
newtype Viewport =
  Viewport { width :: Int
           , height :: Int
           , latitude :: Number
           , longitude :: Number
           , zoom :: Number
           , bearing :: Number
           , pitch :: Number
           }

derive instance genericViewport :: Generic Viewport _

derive instance newtypeViewport :: Newtype Viewport _

derive instance eqViewport :: Eq Viewport

instance showViewport :: Show Viewport where
  show = genericShow

-- | A handler to be run whenever the viewport changes
type OnChangeViewport eff = EffFn1 eff Viewport Unit

foreign import data LatLng :: Type

foreign import lat :: LatLng -> Number

foreign import lng :: LatLng -> Number

-- | The type exposed by the picking engine (abbreviated).
-- | - `latLng`: The latitude and longitude of the point picked.
type ClickInfo =
  { latLng :: LatLng
  }

-- | A handler to run when the picking engine fires.
type OnClickMap eff = EffFn1 eff ClickInfo Unit

type MapProps eff =
  { width :: Int
  , height :: Int
  , latitude :: Number
  , longitude :: Number
  , zoom :: Number
  , bearing :: Number
  , pitch :: Number
  , onChangeViewport :: OnChangeViewport eff
  , onClick :: OnClickMap eff
  , mapStyle :: String
  , mapboxApiAccessToken :: String
  }

foreign import mapGL :: forall eff. R.ReactClass (MapProps eff)

