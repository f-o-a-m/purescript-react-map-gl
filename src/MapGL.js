const MapGL = require('react-map-gl');

exports.mapGL = MapGL.default;

exports.lat = function (latLng) {
    return latLng[0];
};

exports.lng = function (latLng) {
    return latLng[1];
};
