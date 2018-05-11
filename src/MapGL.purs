module MapGL
  ( Viewport(..)
  , OnViewportChange
  , ClickInfo
  , OnClickMap
  , MapProps
  , MapPropsR
  , mkProps
  , mapGL
  ) where

import Prelude

import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Record.Builder (build, merge)
import React as R
import WebMercator.Viewport (LngLat, ViewportR)

-- TODO add animation props to Viewport

-- | `Viewport` is the most basic state type for the map component.
newtype Viewport =
  Viewport (Record (ViewportR ()))

derive instance genericViewport :: Generic Viewport _
derive instance newtypeViewport :: Newtype Viewport _
derive instance eqViewport :: Eq Viewport

instance showViewport :: Show Viewport where
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
