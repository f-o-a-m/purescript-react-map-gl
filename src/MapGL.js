const MapGL = require('react-map-gl');

exports.mapGL = MapGL.default;
exports.defaultProps = MapGL.default.defaultProps;

// https://github.com/visgl/react-map-gl/blob/ba454a475ef6513de3689265f6bd4912e0a742f0/src/components/interactive-map.js#L265
exports.getMapImpl = function (mapRef) {
    return mapRef
        ? mapRef.getMap()
        : null;
};
