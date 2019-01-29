module MapGL.Heatmap where

import Data.Function.Uncurried (Fn3, runFn3)
import MapGL (MapboxLayer, MapboxLayerId, MapboxSourceId)

newtype HeatmapWeightProperty = HeatmapWeightProperty String

foreign import mkHeatmapLayerImpl :: forall a. Fn3 MapboxLayerId MapboxSourceId HeatmapWeightProperty (MapboxLayer a)
mkHeatmapLayer :: forall a. MapboxLayerId -> MapboxSourceId -> HeatmapWeightProperty -> MapboxLayer a
mkHeatmapLayer = runFn3 mkHeatmapLayerImpl
