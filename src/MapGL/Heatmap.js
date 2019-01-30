
// Heatmap layer
// https://docs.mapbox.com/mapbox-gl-js/style-spec/#layers-heatmap
exports.mkHeatmapLayerImpl = function (id, sourceId, weightProperty) {
    const MAX_ZOOM_LEVEL = 9;
    return {
        id: id,
        source: sourceId,
        maxzoom: MAX_ZOOM_LEVEL,
        type: 'heatmap',
        paint: {
            // Increase the heatmap weight based on a property.
            // This property has to be defined in every Feature of a FeatureCollection
            "heatmap-weight": [
                "interpolate",
                ["linear"],
                ["get", weightProperty],
                0, 0,
                6, 1
            ],
            // Increase the heatmap color weight weight by zoom level
            // heatmap-intensity is a multiplier on top of heatmap-weight
            // https://docs.mapbox.com/mapbox-gl-js/style-spec/#paint-heatmap-heatmap-intensity
            "heatmap-intensity": [
                "interpolate",
                ["linear"],
                ["zoom"],
                0, 1,
                MAX_ZOOM_LEVEL, 3
            ],
            // Color ramp for heatmap.  Domain is 0 (low) to 1 (high).
            // Begin color ramp at 0-stop with a 0-transparancy color
            // to create a blur-like effect.
            // https://docs.mapbox.com/mapbox-gl-js/style-spec/#paint-heatmap-heatmap-color
            "heatmap-color": [
                "interpolate",
                ["linear"],
                ["heatmap-density"],
                0, "rgba(33,102,172,0)",
                0.2, "rgb(103,169,207)",
                0.4, "rgb(209,229,240)",
                0.6, "rgb(253,219,199)",
                0.8, "rgb(239,138,98)",
                0.9, "rgb(255,201,101)"
            ],
            // Adjust the heatmap radius by zoom level
            // https://docs.mapbox.com/mapbox-gl-js/style-spec/#paint-heatmap-heatmap-radius
            "heatmap-radius": [
                "interpolate",
                ["linear"],
                ["zoom"],
                // zoom is 0 -> radius will be 2px
                0, 2,
                // zoom is 9 -> radius will be 20px
                MAX_ZOOM_LEVEL, 20
            ],
            // Transition from heatmap to circle layer by zoom level
            // https://docs.mapbox.com/mapbox-gl-js/style-spec/#paint-heatmap-heatmap-opacity
            "heatmap-opacity": [
                "interpolate",
                ["linear"],
                ["zoom"],
                // zoom is 7 (or less) -> opacity will be 1
                7, 1,
                // zoom is 9 (or greater) -> opacity will be 0
                MAX_ZOOM_LEVEL, 0
            ],
        }
    };
};