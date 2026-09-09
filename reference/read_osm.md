# Read Open Street Map data

Read Open Street Map data. OSM tiles are read and returned as a spatial
raster. Vectorized OSM data is not supported anymore (see details).

## Usage

``` r
read_osm(
  x,
  zoom = NULL,
  type = "osm",
  minNumTiles = NULL,
  mergeTiles = NULL,
  use.colortable = FALSE,
  ...
)
```

## Arguments

- x:

  object that can be coerced to a bounding box with
  [`bb`](https://r-tmap.github.io/tmaptools/reference/bb.md) (e.g. an
  existing bounding box or a shape). In the first case, other arguments
  can be passed on to
  [`bb`](https://r-tmap.github.io/tmaptools/reference/bb.md) (see
  `...`). If an existing bounding box is specified in projected
  coordinates, plesae specify `current.projection`.

- zoom:

  passed on to
  [`openmap`](https://rdrr.io/pkg/OpenStreetMap/man/openmap.html). Only
  applicable when `raster=TRUE`.

- type:

  tile provider, by default `"osm"`, which corresponds to OpenStreetMap
  Mapnik. See
  [`openmap`](https://rdrr.io/pkg/OpenStreetMap/man/openmap.html) for
  options. Only applicable when `raster=TRUE`.

- minNumTiles:

  passed on to
  [`openmap`](https://rdrr.io/pkg/OpenStreetMap/man/openmap.html) Only
  applicable when `raster=TRUE`.

- mergeTiles:

  passed on to
  [`openmap`](https://rdrr.io/pkg/OpenStreetMap/man/openmap.html) Only
  applicable when `raster=TRUE`.

- use.colortable:

  should the colors of the returned raster object be stored in a
  [`colortable`](https://rdrr.io/pkg/raster/man/colortable.html)? If
  `FALSE`, a RasterStack is returned with three layers that correspond
  to the red, green and blue values betweeen 0 and 255.

- ...:

  arguments passed on to
  [`bb`](https://r-tmap.github.io/tmaptools/reference/bb.md).

## Value

The output of `read_osm` is a
[`raster`](https://rdrr.io/pkg/raster/man/raster.html) object.

## Details

As of version 2.0, `read_osm` cannot be used to read vectorized OSM data
anymore. The reason is that the package that was used under the hood,
`osmar`, has some limitations and is not actively maintained anymore.
Therefore, we recommend the package `osmdata`. Since this package is
very user-friendly, there was no reason to use `read_osm` as a wrapper
for reading vectorized OSM data.

## Examples

``` r
if (FALSE) { # \dontrun{
if (require(tmap)) {
    #### Choropleth with OSM background

    # load Netherlands shape
    data(NLD_muni)

    # read OSM raster data
    osm_NLD <- read_osm(NLD_muni, ext=1.1)

    # plot with regular tmap functions
    tm_shape(osm_NLD) +
      tm_rgb() +
    tm_shape(NLD_muni) +
      tm_polygons("population", convert2density=TRUE, style="kmeans", alpha=.7, palette="Purples")

    #### A close look at the building of Statistics Netherlands in Heerlen

    # create a bounding box around the CBS (Statistics Netherlands) building
    CBS_bb <- bb("CBS Weg 11, Heerlen", width=.003, height=.002)

    # read Microsoft Bing satellite and OpenCycleMap OSM layers
    CBS_osm1 <- read_osm(CBS_bb, type="bing")
    CBS_osm2 <- read_osm(CBS_bb, type="opencyclemap")

    # plot OSM raster data
    qtm(CBS_osm1)
    qtm(CBS_osm2)

}
} # }
```
