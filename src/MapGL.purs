module MapGL
  ( Viewport(..)
  , OnViewportChange
  , ClickInfo
  , OnClickMap
  , MapProps
  , MapPropsR
  , mapGL
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Effect.Uncurried (EffectFn1)
import React as R
import Record (disjointUnion)
import WebMercator.LngLat (LngLat)
import WebMercator.Viewport (ViewportR)


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

type MapProps = Record (ViewportR (MapPropsR (children :: R.Children)))

foreign import mapGL :: R.ReactClass MapProps
