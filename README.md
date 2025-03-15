# SMAPR - recreational project
## Static map renderer

Goal is to render MapBox [Vector tiles](https://github.com/mapbox/vector-tile-spec/blob/master/2.1/README.md) as Diagrams.

Config structure. For smapr.cfg:
```
api {
	base_url = "tile server host"
	lines_path = "feature's path"
        format = "pbf"
}

maptiler_api {
        base_url = "https://api.maptiler.com/tiles/v3-openmaptiles/"
        api_key = "your API key"
        format = "supports only pbf"
}

test_dest_path = "absolute path"

test_tile_path = "..."

json_test_path = "..."
```