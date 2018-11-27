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

import Effect.Uncurried (EffectFn1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Record.Builder (build, merge)
import React as R
import WebMercator.Viewport (ViewportR)
import WebMercator.LngLat (LngLat)


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
type OnViewportChange = EffectFn1 Viewport Unit

-- | The type exposed by the picking engine (abbreviated).
-- | - `latLng`: The latitude and longitude of the point picked.
type ClickInfo =
  { lngLat :: LngLat
  }

-- | A handler to run when the picking engine fires.
type OnClickMap = EffectFn1 ClickInfo Unit

type MapPropsR r =
  ( onViewportChange :: OnViewportChange
  , onClick :: OnClickMap
  , mapStyle :: String
  , mapboxApiAccessToken :: String
  | r
  )

type MapProps = Record (ViewportR (MapPropsR ()))

mkProps :: Viewport -> Record (MapPropsR ()) -> MapProps
mkProps (Viewport vp) rest = build (merge rest) vp

foreign import mapGL :: R.ReactClass MapProps
