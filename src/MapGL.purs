module MapGL
  ( Viewport(..)
  , InteractiveMap
  , Map
  , OnViewportChange
  , ClickInfo
  , OnLoadMap
  , OnClickMap
  , MapProps
  , MapPropsR
  , MapboxSource(..)
  , MapboxSourceId(..)
  , MapboxLayer(..)
  , MapboxLayerId(..)
  , mkProps
  , mapGL
  , getMap
  , getMapboxSource
  , addMapboxSource
  , addMapboxLayer
  , setMapboxSourceData
  ) where

import Prelude

import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import React as R
import Record (disjointUnion)
import WebMercator.LngLat (LngLat)
import WebMercator.Viewport (ViewportR)


-- TODO(safareli) add animation props to Viewport

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

-- | The onLoad callback for the map
type OnLoadMap = Effect Unit

-- | A handler to run when the picking engine fires.
type OnClickMap = EffectFn1 ClickInfo Unit

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

-- Default map component by `ReactMapGL` to render `MapboxGL`
-- https://github.com/uber/react-map-gl/blob/master/docs/components/interactive-map.md
foreign import data InteractiveMap :: Type
-- `Map` object to have full access to underlaying `MapboxGL`s API
-- https://docs.mapbox.com/mapbox-gl-js/api/#map#addsource
foreign import data Map :: Type

foreign import getMapImpl :: Fn1 InteractiveMap (Nullable Map)
getMap :: InteractiveMap -> Maybe Map
getMap = Nullable.toMaybe <<< runFn1 getMapImpl

-- A source of Mapbox' style
type MapboxSource r = (|r)
-- Id of a source in Mapbox' style
newtype MapboxSourceId = MapboxSourceId String
-- A Mapbox' style layer
type MapboxLayer r = (|r)
-- Id of a Mapbox' layer
newtype MapboxLayerId = MapboxLayerId String

foreign import addMapboxSourceImpl :: forall r. EffectFn3 Map MapboxSourceId (MapboxSource r) Unit
addMapboxSource :: forall r. Map -> MapboxSourceId -> MapboxSource r -> Effect Unit
addMapboxSource = runEffectFn3 addMapboxSourceImpl

foreign import addMapboxLayerImpl :: forall r. EffectFn2 Map (MapboxLayer r) Unit
addMapboxLayer :: forall r. Map -> MapboxLayer r -> Effect Unit
addMapboxLayer = runEffectFn2 addMapboxLayerImpl

foreign import getMapboxSourceImpl :: forall r. Fn2 Map MapboxSourceId (MapboxSource r)
getMapboxSource :: forall r. Map -> MapboxSourceId -> MapboxSource r
getMapboxSource = runFn2 getMapboxSourceImpl

-- TODO (sectore) Define generic type for `data`
foreign import setMapboxSourceDataImpl :: forall d. EffectFn3 Map MapboxSourceId d Unit
setMapboxSourceData :: forall d. Map -> MapboxSourceId -> d -> Effect Unit 
setMapboxSourceData = runEffectFn3 setMapboxSourceDataImpl