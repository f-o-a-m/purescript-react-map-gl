exports.addSourceImpl = function (map, sourceId, source) {
    map.addSource(sourceId, source);
};

exports.addLayerImpl = function (map, layer, sourceId) {
    map.addLayer(layer, sourceId);
};

exports.getSourceImpl = function (map, sourceId) {
    return map.getSource(sourceId);
};

exports.setDataImpl = function(map, sourceId, data) {
    var source = map.getSource(sourceId);
    source.setData(data);
};