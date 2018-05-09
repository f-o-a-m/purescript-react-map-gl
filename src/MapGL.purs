module MapGL
  ( Viewport(..)
  , ViewportR
  , LngLat
  , OnViewportChange
  , ClickInfo
  , OnClickMap
  , MapProps
  , MapPropsR
  , mkProps
  , mapGL
  , lng
  , lat
  , makeLngLat
  ) where

import Prelude

import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1)
import Data.Array ((!!))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype)
import Data.Record.Builder (build, merge)
import Partial.Unsafe (unsafePartial)
import React as R

-- | `Viewport` is the most basic state type for the map component.
newtype Viewport =
  Viewport (Record (ViewportR ()))

type ViewportR r =
  ( width :: Int
  , height :: Int
  , latitude :: Number
  , longitude :: Number
  , zoom :: Number
  , bearing :: Number
  , pitch :: Number
  | r
  )

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
type OnViewportChange eff = EffFn1 eff Viewport Unit

-- | The type exposed by the picking engine (abbreviated).
-- | - `latLng`: The latitude and longitude of the point picked.
type ClickInfo =
  { lngLat :: LngLat
  }

-- | A handler to run when the picking engine fires.
type OnClickMap eff = EffFn1 eff ClickInfo Unit

type MapPropsR eff r =
  ( onViewportChange :: OnViewportChange eff
  , onClick :: OnClickMap eff
  , mapStyle :: String
  , mapboxApiAccessToken :: String
  | r
  )

type MapProps eff = Record (ViewportR (MapPropsR eff ()))

mkProps :: forall eff. Viewport -> Record (MapPropsR eff ()) -> MapProps eff
mkProps (Viewport vp) rest = build (merge rest) vp

foreign import mapGL :: forall eff. R.ReactClass (MapProps eff)
