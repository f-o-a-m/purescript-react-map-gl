const MapGL = require('react-map-gl');

exports.mapGL = MapGL.default;
exports.defaultProps = MapGL.default.defaultProps;

exports.getMapImpl = function (mapRef) {
    return mapRef 
        ? mapRef.getMap()
        : null;
};
