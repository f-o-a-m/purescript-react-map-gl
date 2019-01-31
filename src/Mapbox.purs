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
  , LayerId(..)
  , LayerType(..)
  , Paint(..)
  , PaintProperty(..)
  , StyleExpression(..)
  , getSource
  , addSource
  , addLayer
  , setData
  , mkPaintProperty
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Foreign (Foreign)
import Foreign.Object as FO
import Simple.JSON (class WriteForeign, writeImpl)

-- `Map` object to have full access to underlaying `MapboxGL`s API
-- https://docs.mapbox.com/mapbox-gl-js/api/#map#addsource
foreign import data Map :: Type

-- A source of Mapbox' style
type Source r = (|r)
-- Id of a source in Mapbox' style
newtype SourceId = SourceId String
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

-- Paint property
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#layer-paint
type PaintProperty = Tuple String (Array StyleExpression)

mkPaintProperty :: String -> (Array StyleExpression) -> PaintProperty
mkPaintProperty = Tuple

-- Object to describe paint properties
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#layer-paint
-- It's a hash map, where its key is the `expression-name` 
-- and the value a list of expressions 
newtype Paint = Paint (Array PaintProperty)
instance writeForeignPaint :: WriteForeign Paint where
  writeImpl (Paint arr)= writeImpl $ FO.fromFoldable arr

-- A Mapbox' style layer
newtype Layer = Layer 
  { id:: LayerId
  , source :: SourceId 
  , type :: LayerType
  , minzoom :: Number
  , maxzoom :: Number
  , paint :: Paint
  }

instance writeForeignLayer :: WriteForeign Layer where
  writeImpl (Layer l)= writeImpl l

-- Id of a Mapbox' layer
newtype LayerId = LayerId String
derive newtype instance writeForeignLayerId :: WriteForeign LayerId

-- Mapbox' style expression
-- https://docs.mapbox.com/mapbox-gl-js/style-spec/#expressions
data StyleExpression 
  = SEArray (Array String)
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
addLayer :: Map -> Layer -> Effect Unit
addLayer map layer = runEffectFn2 addLayerImpl map (writeImpl layer)

foreign import getSourceImpl :: forall r. Fn2 Map SourceId (Source r)
-- | Returns a source of Mapbox' map style by a given Id
-- | https://docs.mapbox.com/mapbox-gl-js/api/#map#getsource
getSource :: forall r. Map -> SourceId -> Source r
getSource = runFn2 getSourceImpl

type GeoJson r = Record(type::String|r)

foreign import setDataImpl :: forall r. EffectFn3 Map SourceId (GeoJson r) Unit
-- | Sets the GeoJSON data and re-renders the map.
-- | https://docs.mapbox.com/mapbox-gl-js/api/#geojsonsource#setdata
setData :: forall r. Map -> SourceId -> GeoJson r -> Effect Unit 
setData = runEffectFn3 setDataImpl
