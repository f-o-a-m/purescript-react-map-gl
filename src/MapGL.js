import { default as MapGL } from 'react-map-gl';

export const mapGL = MapGL;
export const defaultProps = MapGL.defaultProps;

// https://github.com/visgl/react-map-gl/blob/ba454a475ef6513de3689265f6bd4912e0a742f0/src/components/interactive-map.js#L265
export const getMapImpl = function (mapRef) {
    return mapRef
        ? mapRef.getMap()
        : null;
};
