const MapGL = require('react-map-gl');

exports.mapGL = MapGL.default;


exports.getMapImpl = function (mapRef) {
    return mapRef 
        ? mapRef.getMap()
        : null;
};

// Adds a source to Mapbox' map style.
// https://docs.mapbox.com/mapbox-gl-js/api/#map#addsource
exports.addMapboxSourceImpl = function (map, sourceId, source) {
    map.addSource(sourceId, source);
};

// Adds a layer to Mapbox' map style.
// https://docs.mapbox.com/mapbox-gl-js/api/#map#addlayer
exports.addMapboxLayerImpl = function (map, layer, sourceId) {
    map.addLayer(layer, sourceId);
};

// Returns a source of Mapbox' map style by a given Id
// https://docs.mapbox.com/mapbox-gl-js/api/#map#getsource
exports.getMapboxSourceImpl = function (map, sourceId) {
    return map.getSource(sourceId);
};
// Sets the GeoJSON data and re-renders the map.
// https://docs.mapbox.com/mapbox-gl-js/api/#geojsonsource#setdata
exports.setMapboxSourceDataImpl = function(map, sourceId, data) {
    var source = map.getSource(sourceId);
    console.log("data", data);
    
    source.setData(data);
};