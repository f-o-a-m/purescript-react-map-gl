const MapGL = require('react-map-gl');

exports.mapGL = MapGL.default;


exports.getMapImpl = function (mapRef) {
    // console.log("getMapImpl getMap", mapRef.getMap());
    // console.log("getMapImpl getMap().addLayer", mapRef.getMap().addLayer);
    // console.log("getMapImpl getMap().addSource", mapRef.getMap().addSource);
    // console.log("getMapImpl getMap().getSource", mapRef.getMap().getSource);
    return mapRef 
        ? mapRef.getMap()
        : null;
};

// Adds a source to Mapbox' map style.
// https://docs.mapbox.com/mapbox-gl-js/api/#map#addsource
exports.addMapboxSourceImpl = function (map, id, source) {
    map.addSource(id, source);
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
exports.setMapboxSourceData = function(source, data) {
    source.setData(data);
};