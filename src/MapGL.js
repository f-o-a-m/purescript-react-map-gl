const MapGL = require('react-map-gl');

exports.mapGL = MapGL.default;

exports.lat = function (lngLat) {
    return lngLat[1];
};

exports.lng = function (lngLat) {
    return lngLat[0];
};

exports.makeLngLat = function (lng) {
    return function (lat) {
        return [lng, lat];
    };
};
