module MapGL.Heatmap where

import Data.Function.Uncurried (Fn3, runFn3)
import MapGL (MapboxLayer, MapboxLayerId, MapboxSource)

newtype HeatmapWeigthProperty = HeatmapWeightProperty String

foreign import mkHeatmapLayerImpl :: forall a b. Fn3 MapboxLayerId (MapboxSource a) HeatmapWeigthProperty (MapboxLayer b)
mkHeatmapLayer :: forall a b. MapboxLayerId -> MapboxSource a -> HeatmapWeigthProperty -> MapboxLayer b
mkHeatmapLayer = runFn3 mkHeatmapLayerImpl
