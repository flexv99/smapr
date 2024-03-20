# SMAPR
## Svg map renderer

Renders MapBox [Vector tiles](https://github.com/mapbox/vector-tile-spec/blob/master/2.1/README.md) as SVG.

Config structure. For smapr.cfg:
```
api {
	base_url = "tile server host"
	lines_path = "feature's path"
}

test_dest_path = "absolute path"
```