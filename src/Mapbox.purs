-- | This module exposes types and functions of Mapbox GL JS
-- | https://docs.mapbox.com/mapbox-gl-js/api/
-- |
-- | Important note: The API of Mapbox GL JS is not fully covered.
-- | Most of the stuff are to access underlaying Map of `react-map-gl`
-- | and to render a heatmap. More stuff will be added if needed...
module Mapbox
  ( Map
  , Source(..)
  , SourceId(..)
  , Layer(..)
  , LayerR
  , LayerId(..)
  , LayerType(..)
  , Paint(..)
  , PaintProperty(..)
  , StyleExpression(..)
  , GeoJsonData
  , GeoJsonSource
  , Layout(..)
  , LayerVisibility(..)
  , FillExtrusionLayer
  , FillExtrusionLayerR
  , HeatmapLayoutProperties
  , FillExtrusionLayoutProperties
  , getSource
  , addSource
  , addLayer
  , setData
  , mkPaintPropertyA
  , mkPaintPropertyV
  , mkGeoJsonSource
  , setLayerVisibilty
  ) where

import Prelude
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Foreign (Foreign)
import Foreign.Object as FO
import GeoJson (GeoJson)
import Simple.JSON (class WriteForeign, writeImpl)
import Unsafe.Coerce

-- `Map` object to have full access to underlaying `MapboxGL`s API
-- https://docs.mapbox.com/mapbox-gl-js/api/#map#addsource
foreign import data Map :: Type

-- A source of Mapbox' style
type Source r
  = ( | r )

-- `Source` typed as source of `geojson` data
-- Note: For now we do support `geojson` data only,
-- but by following `Mapbox` documentation it could be
-- `vector`, `raster`, `raster-dem`, `geojson`, `image`, `video`
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#sources
data GeoJsonData

type GeoJsonSource d
  = Source (GeoJson d)

-- Factory to create a `Source` based on `GeoJson` data
mkGeoJsonSource :: forall d. d -> GeoJsonSource d
mkGeoJsonSource d =
  { type: "geojson"
  , data: d
  }

-- Id of a source in Mapbox' style
newtype SourceId
  = SourceId String

derive newtype instance writeForeignSourceId :: WriteForeign SourceId

-- Layer types
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#layer-type
data LayerType
  = Fill
  | Background
  | Line
  | Symbol
  | Raster
  | Circle
  | FillExtrusion
  | Heatmap
  | Hillshade

instance writeForeignLayerType :: WriteForeign LayerType where
  writeImpl = case _ of
    Fill -> writeImpl "fill"
    Background -> writeImpl "background"
    Line -> writeImpl "line"
    Symbol -> writeImpl "symbol"
    Raster -> writeImpl "raster"
    Circle -> writeImpl "circle"
    FillExtrusion -> writeImpl "fill-extrusion"
    Heatmap -> writeImpl "heatmap"
    Hillshade -> writeImpl "hillshade"

-- `paint` property
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#layer-paint
data PaintProperty
  = PPArray String (Array StyleExpression)
  | PPValue String StyleExpression

instance writeForeignPaintProperty :: WriteForeign PaintProperty where
  writeImpl (PPArray s arr) = writeImpl $ show s <> " : " <> (unsafeCoerce $ writeImpl arr)
  writeImpl (PPValue s se) = writeImpl $ show s <> " : " <> unsafeCoerce se

mkPaintPropertyA :: String -> (Array StyleExpression) -> PaintProperty
mkPaintPropertyA = PPArray

mkPaintPropertyV :: String -> StyleExpression -> PaintProperty
mkPaintPropertyV = PPValue

-- Object to describe paint properties
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#layer-paint
-- It's a hash map, where its key is the `expression-name`
-- and the value a list of expressions
newtype Paint
  = Paint (Array PaintProperty)

instance writeForeignPaint :: WriteForeign Paint where
  writeImpl (Paint arr) = writeImpl arr

-- `layout` property to show or hide a layer
-- Example https://docs.mapbox.com/mapbox-gl-js/style-spec/#layout-heatmap-visibility
data LayerVisibility
  = LayerVisible
  | LayerNone

instance writeForeignLayerVisibility :: WriteForeign LayerVisibility where
  writeImpl = case _ of
    LayerVisible -> writeImpl "visible"
    LayerNone -> writeImpl "none"

-- `layout` properties of a `heatmap-layer`
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#layers-heatmap
type HeatmapLayoutProperties
  = { visibility :: LayerVisibility }

type FillExtrusionLayoutProperties
  = { visibility :: LayerVisibility }

-- Object to describe layout properties of any kind of layer
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#layer-layout
-- Note: Currently we do support layout of `heatmap-layer` only
data Layout
  = HeatmapLayout HeatmapLayoutProperties
  | FillExtrusionLayout FillExtrusionLayoutProperties

instance writeForeignLayout :: WriteForeign Layout where
  writeImpl = case _ of
    HeatmapLayout props -> writeImpl props
    FillExtrusionLayout props -> writeImpl props

-- A Mapbox' style layer
newtype Layer r
  = Layer (Record (LayerR r))

type LayerR r
  = ( id :: LayerId
    , source :: SourceId
    , type :: LayerType
    , minzoom :: Number
    , maxzoom :: Number
    , paint :: Paint
    , layout :: Layout
    | r
    )

derive newtype instance writeForeignLayer :: (WriteForeign (Record (LayerR r))) => WriteForeign (Layer r)

type FillExtrusionLayerR
  = ( "source-layer" :: String )

type HeatMapLayer
  = Layer ()

type FillExtrusionLayer
  = Layer FillExtrusionLayerR

-- Id of a Mapbox' layer
newtype LayerId
  = LayerId String

derive newtype instance writeForeignLayerId :: WriteForeign LayerId

-- Mapbox' style expression
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#expressions
data StyleExpression
  = SEArray (Array StyleExpression)
  | SEString String
  | SENumber Number

instance writeForeignExpression :: WriteForeign StyleExpression where
  writeImpl = case _ of
    SEArray arr -> writeImpl arr
    SEString str -> writeImpl str
    SENumber no -> writeImpl no

foreign import addSourceImpl :: forall r. EffectFn3 Map SourceId (Source r) Unit

-- | Adds a source to Mapbox' map style.
-- | https://docs.mapbox.com/mapbox-gl-js/api/#map#addsource
addSource :: forall r. Map -> SourceId -> Source r -> Effect Unit
addSource = runEffectFn3 addSourceImpl

foreign import addLayerImpl :: EffectFn2 Map Foreign Unit

-- | Adds a layer to Mapbox' map style.
-- | https://docs.mapbox.com/mapbox-gl-js/api/#map#addlayer
addLayer :: forall r. (WriteForeign (Layer r)) => Map -> Layer r -> Effect Unit
addLayer map layer = runEffectFn2 addLayerImpl map (writeImpl layer)

foreign import getSourceImpl :: forall r. Fn2 Map SourceId (Nullable (Source r))

-- | Returns a source of Mapbox' map style by a given Id
-- | https://docs.mapbox.com/mapbox-gl-js/api/#map#getsource
getSource :: forall r. Map -> SourceId -> Maybe (Source r)
getSource map = Nullable.toMaybe <<< runFn2 getSourceImpl map

foreign import setDataImpl :: forall r. EffectFn3 Map SourceId (Source r) Unit

-- | Sets the GeoJSON data and re-renders the map.
-- | https://docs.mapbox.com/mapbox-gl-js/api/#geojsonsource#setdata
setData :: forall r. Map -> SourceId -> Source r -> Effect Unit
setData = runEffectFn3 setDataImpl

foreign import setLayerVisibiltyImpl :: EffectFn3 Map LayerId Boolean Unit

-- | Show or hide a layer by changing `visibility` property
-- | https://docs.mapbox.com/mapbox-gl-js/style-spec/#layout-background-visibility
setLayerVisibilty :: Map -> LayerId -> Boolean -> Effect Unit
setLayerVisibilty = runEffectFn3 setLayerVisibiltyImpl
