/* Global variables */
const width =
    window.innerWidth <= 560 ? 400 : window.innerWidth <= 960 ? 800 : 1100,
  height =
    window.innerWidth <= 560 ? 400 : window.innerWidth <= 960 ? 700 : 900;

// Buildings color
const buildingsFill = "#3288bd";
const roadsStroke = "#ff57ae";
// const roadsStroke = '#99d594';

const svg = d3
  .select("#chart")
  .append("svg")
  .attr("width", width)
  .attr("height", height)
  .style("background-color", "#fff")
  .style("font", "10px sans-serif");

const g = svg
  .append("g")
  .attr("class", "map-features")
  .attr("width", width)
  .attr("height", height)
  .attr("transform", `translate(0, 0)`);

// Map projection
const zoom = 23;
const center = [11.893096, 46.615577];

const projection = d3
  .geoMercator()
  .center(center)
  .scale(Math.pow(2, zoom) / (2 * Math.PI))
  .translate([width / 2, height / 2]);

// Path
const path = d3.geoPath(projection);

// Tiler
const tile = d3
  .tile()
  .tileSize(512)
  .size([width, height])
  .scale(projection.scale() * 2 * Math.PI)
  .translate(projection([0, 0]));

const geojson = ([x, y, z], layer) => {
  if (!layer) return;
  const features = [];
  for (let i = 0; i < layer.length; ++i) {
    const f = layer.feature(i).toGeoJSON(x, y, z);
    features.push(f);
  }
  return { type: "FeatureCollection", features };
};

const tiles_promises = Promise.all(
  tile().map(async (d) => {
    console.log(`http://0.0.0.0:3000/polygons/${d[2]}/${d[0]}/${d[1]}`);
    d.layers = new VectorTile(
      new Pbf(
        await d3.buffer(`http://0.0.0.0:3000/polygons/${d[2]}/${d[0]}/${d[1]}`)
      )
    ).layers;
    return d;
  })
);

const line_tiles_promises = Promise.all(
  tile().map(async (d) => {
    d.layers = new VectorTile(
      new Pbf(
        await d3.buffer(`http://0.0.0.0:3000/lines/${d[2]}/${d[0]}/${d[1]}`)
      )
    ).layers;
    return d;
  })
);

tiles_promises.then((tiles) => {
  tiles.map((d) => {
    const background = g
      .selectAll("g.background")
      .append("rect")
      .style("fill", "#262626")
      .style("fill-opacity", 0.35)
      .attr("width", 100)
      .attr("height", 60);

    const polygons = g
      .selectAll("g.polygons")
      .data(d)
      .enter()
      .append("path")
      .style("fill", "brown")
      .attr("d", path(geojson(d, d.layers.polygons)))
      .style("stroke-width", 0.5)
      .style("stroke", "#fff");
  });
});

line_tiles_promises.then((tiles) => {
  tiles.map((d) => {
    const routes = g
      .selectAll("g.routes")
      .data(d)
      .enter()
      .append("path")
      .style("fill", "none")
      .attr("d", path(geojson(d, d.layers.lines)))
      .style("stroke-width", 1.0)
      .style("stroke", "green");
  });
});
