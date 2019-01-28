-- Module of `GeoJson` types
-- Types are based on RFC 7946 https://tools.ietf.org/html/rfc7946

-- Note: This module does not cover all GeoJSON specification of RFC 7946, just some of them.  
-- Currently it does include just types needed to create source data for a heatmap.
-- An example of such heatmap GeoJSON data is https://docs.mapbox.com/mapbox-gl-js/assets/earthquakes.geojson
 

module GeoJson where

import Data.Maybe (Maybe)

-- GeoJson
-- https://tools.ietf.org/html/rfc7946#section-3
data GeoJson p 
    = GeoJsonGeometry (Geometry p)
    | GeoJsonFeature (Feature p)
    | GeoJsonFeatureCollection (FeatureCollection p)

-- Point
-- https://tools.ietf.org/html/rfc7946#section-3.1.2
type Point =
    { x :: Number
    , y :: Number
    }

-- FeatureCollection
-- https://tools.ietf.org/html/rfc7946#section-3.3
type FeatureCollection p = { features :: Array (Feature p) }

type Feature p = { properties :: Maybe p }

-- Geometry
-- https://tools.ietf.org/html/rfc7946#section-3.1
type Geometry p = { type :: GeometryType p }

-- https://tools.ietf.org/html/rfc7946#section-1.4
data GeometryType prop
    = GeometryPoint Point
    | GeometryMultiPoint
    | GeometryLineString
    | GeometryMultiLineString
    | GeometryPolygon
    | GeometryMultiPolygon
    | GeometryCollection
    | GeometryFeature (Feature prop)
    | GeometryFeatureCollection (FeatureCollection prop)