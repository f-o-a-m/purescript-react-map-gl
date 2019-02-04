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
type Feature p =
    { type :: String 
    , geometry :: Geometry 
    , properties :: p
    }

mkFeature :: forall p. Geometry -> p -> Feature p 
mkFeature geometry properties = 
    { type: "Feature"
    , geometry
    , properties
    }

-- Geometry
-- https://tools.ietf.org/html/rfc7946#section-3.1
type Geometry = 
    { type :: String 
    , coordinates :: Array Number 
    }

-- Helper to create a Point as a Geometry
-- https://tools.ietf.org/html/rfc7946#section-3.1.2
mkPoint :: Array Number -> Geometry 
mkPoint coordinates = 
    { type: "Point"
    , coordinates 
    }