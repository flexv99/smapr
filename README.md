# SMAPR
## Svg map renderer

Goal is to render MapBox [Vector tiles](https://github.com/mapbox/vector-tile-spec/blob/master/2.1/README.md) as SVG.

Config structure. For smapr.cfg:
```
api {
	base_url = "tile server host"
	lines_path = "feature's path"
}

nextzen_api {
        base_url = "https://tile.nextzen.org/tilezen/vector/v1/512/all/"
        api_key = "your API key"
        format = "supports only mvt"
}

test_dest_path = "absolute path"
```