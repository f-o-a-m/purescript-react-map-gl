export const addSourceImpl = function (map, sourceId, source) {
    map.addSource(sourceId, source);
};

export const addLayerImpl = function (map, layer, sourceId) {
    map.addLayer(layer, sourceId);
};

export const getSourceImpl = function (map, sourceId) {
    const source = map.getSource(sourceId);
    return source !== undefined ? source : null;
};

export const setDataImpl = function(map, sourceId, data) {
    var source = map.getSource(sourceId);
    source.setData(data);
};

export const setLayerVisibiltyImpl = function(map, layerId, visible) {
    const value = visible ? 'visible' : 'none';
    map.setLayoutProperty(layerId, 'visibility', value);
};

// export const setSourceLayerImpl = function(map, sourceId, data) {
//     var source = map.getSource(sourceId);
//     source.sourceLayer = data;
// };
