# Thematic Map Tools

This package offers a set of handy tool functions for reading and
processing spatial data. The aim of these functions is to supply the
workflow to create thematic maps, e.g. read shape files, set map
projections, append data, calculate areas and distances, and query
OpenStreetMap. The visualization of thematic maps can be done with the
tmap package.

## Details

This page provides a brief overview of all package functions.

## Tool functions (shape)

|  |  |
|----|----|
| [`approx_areas`](https://r-tmap.github.io/tmaptools/reference/approx_areas.md) | Approximate area sizes of polygons |
| [`approx_distances`](https://r-tmap.github.io/tmaptools/reference/approx_distances.md) | Approximate distances |
| [`bb`](https://r-tmap.github.io/tmaptools/reference/bb.md) | Create, extract or modify a bounding box |
| [`bb_poly`](https://r-tmap.github.io/tmaptools/reference/bb_poly.md) | Convert bounding box to a polygon |
| [`get_asp_ratio`](https://r-tmap.github.io/tmaptools/reference/get_asp_ratio.md) | Get the aspect ratio of a shape object |
| ————————— | ————————————————————————————————— |

## Tool functions (colors)

|  |  |
|----|----|
| [`map_coloring`](https://r-tmap.github.io/tmaptools/reference/map_coloring.md) | Find different colors for adjacent polygons |
| ————————— | ————————————————————————————————— |

## Spatial transformation functions

|  |  |
|----|----|
| [`crop_shape`](https://r-tmap.github.io/tmaptools/reference/crop_shape.md) | Crop shape objects |
| [`simplify_shape`](https://r-tmap.github.io/tmaptools/reference/simplify_shape.md) | Simplify a shape |
| ————————— | ————————————————————————————————— |

## Input and output functions

|  |  |
|----|----|
| [`geocode_OSM`](https://r-tmap.github.io/tmaptools/reference/geocode_OSM.md) | Get a location from an address description |
| [`read_GPX`](https://r-tmap.github.io/tmaptools/reference/read_GPX.md) | Read a GPX file |
| [`read_osm`](https://r-tmap.github.io/tmaptools/reference/read_osm.md) | Read Open Street Map data |
| [`rev_geocode_OSM`](https://r-tmap.github.io/tmaptools/reference/rev_geocode_OSM.md) | Get an address description from a location |
| ————————— | ————————————————————————————————— |

## See also

Useful links:

- <https://github.com/r-tmap/tmaptools>

- <https://r-tmap.github.io/tmaptools/>

- Report bugs at <https://github.com/r-tmap/tmaptools/issues>

## Author

**Maintainer**: Martijn Tennekes <mtennekes@gmail.com>

Authors:

- Martijn Tennekes <mtennekes@gmail.com>
