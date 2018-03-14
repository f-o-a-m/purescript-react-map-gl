module MapGL
  ( Viewport(..)
  , LngLat
  , OnChangeViewport
  , ClickInfo
  , OnClickMap
  , MapProps
  , mapGL
  , lng
  , lat
  , makeLngLat
  ) where

import Prelude
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1)
import Data.Array ((!!))
import Data.Maybe (fromJust)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import React as R
import Partial.Unsafe (unsafePartial)

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

-- | LngLat is used because the native representation of a set of
-- | coordinates is an array of length 2.
newtype LngLat = LngLat (Array Number)

lng :: LngLat -> Number
lng (LngLat lnglat) = unsafePartial fromJust $ lnglat !! 0

lat :: LngLat -> Number
lat (LngLat lnglat) = unsafePartial fromJust $ lnglat !! 1

makeLngLat :: Number -> Number -> LngLat
makeLngLat x y = LngLat [x,y]

derive instance genericLngLat :: Generic LngLat _
derive instance newtypeLngLat :: Newtype LngLat _
derive instance eqLngLat :: Eq LngLat

instance showLngLat :: Show LngLat where
  show = genericShow

-- | A handler to be run whenever the viewport changes
type OnChangeViewport eff = EffFn1 eff Viewport Unit

-- | The type exposed by the picking engine (abbreviated).
-- | - `latLng`: The latitude and longitude of the point picked.
type ClickInfo =
  { lngLat :: LngLat
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
