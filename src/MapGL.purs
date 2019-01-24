module MapGL
  ( Viewport(..)
  , OnViewportChange
  , ClickInfo
  , OnLoadMap
  , OnClickMap
  , MapProps
  , MapPropsR
  , mkProps
  , mapGL
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Effect (Effect)
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
type OnViewportChange = R.SyntheticEventHandler Viewport

-- | The type exposed by the picking engine (abbreviated).
-- | - `latLng`: The latitude and longitude of the point picked.
type ClickInfo =
  { lngLat :: LngLat
  }

-- | The onLoad callback for the map
type OnLoadMap = Effect Unit

-- | A handler to run when the picking engine fires.
type OnClickMap = R.SyntheticEventHandler ClickInfo

type MapPropsR r =
  ( onLoad :: OnLoadMap
  , onViewportChange :: OnViewportChange
  , onClick :: OnClickMap
  , mapStyle :: String
  , mapboxApiAccessToken :: String
  , children :: R.Children
  | r
  )

type MapProps = Record (ViewportR (MapPropsR ()))

mkProps :: Viewport -> Record (MapPropsR ()) -> MapProps
mkProps (Viewport vp) rest = disjointUnion rest vp

foreign import mapGL :: R.ReactClass MapProps
