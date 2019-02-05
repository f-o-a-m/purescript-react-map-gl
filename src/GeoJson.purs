-- Module of `GeoJson` types
-- Types are based on RFC 7946 https://tools.ietf.org/html/rfc7946

-- Note: This module does not cover all GeoJSON specification of RFC 7946, just some of them.  
-- It includes mostly types needed by defining heatmap data.
-- Example heatmap GeoJSON data: https://docs.mapbox.com/mapbox-gl-js/assets/earthquakes.geojson
module GeoJson where

-- GeoJson
-- https://tools.ietf.org/html/rfc7946#section-3
type GeoJson d = 
  { type :: String
  , data :: d 
  }

-- FeatureCollection
-- https://tools.ietf.org/html/rfc7946#section-3.3
type FeatureCollection f = 
  { type :: String 
  , features :: Array f 
  }

mkFeatureCollection :: forall f . Array f -> FeatureCollection f 
mkFeatureCollection features = 
  { type: "FeatureCollection"
  , features
  }

-- Feature
-- https://tools.ietf.org/html/rfc7946#section-3.2
type Feature g p =
  { type :: String 
  , geometry :: Geometry g 
  , properties :: p
  }

mkFeature :: forall g p . Geometry g -> p -> Feature g p
mkFeature geometry properties = 
  { type: "Feature"
  , geometry
  , properties
  }

-- Geometry
-- `t` is a phantom type to describe its kind of content, e.g. `Point`
-- https://tools.ietf.org/html/rfc7946#section-3.1
type Geometry t = 
  { type :: String 
  , coordinates :: Array Number 
  }

-- A `Geometry` typed as a `Point`
-- Note: For now we do support a `Point` geometry only, 
-- but by following documentation of `GeoJson`'s `Geometry` object it could be 
-- `Position`, `MultiPoint`, `LineString`, `MultiLineString`, `Polygon`, `MultiPolygon`, `GeometryCollection`
-- https://tools.ietf.org/html/rfc7946#section-3.1
data PointData
type PointGeometry = Geometry PointData

-- Helper to create a Point as a Geometry
-- https://tools.ietf.org/html/rfc7946#section-3.1.2
mkPoint :: Array Number -> PointGeometry 
mkPoint coordinates = 
  { type: "Point"
  , coordinates 
  }