# Changelog

## version 3.4

- [`map_coloring()`](https://r-tmap.github.io/tmaptools/reference/map_coloring.md)
  has been rebuilt from scratch. It gets by on fewer colors (a map that
  can be done in 4 no longer ends up with 5), counts polygons as
  adjacent only when they share a stretch of border rather than a single
  corner, treats `ncols` as a target that is used evenly instead of an
  upper bound, and always returns the same coloring for the same map.
  The new argument `permutation` picks between the many colorings a map
  has; `minimize` has been removed

## version 3.3

CRAN release: 2025-07-24

- deprecated functions removed
- bb target aspect ration added

## version 3.2

CRAN release: 2025-01-13

- examples updated with tmap4
- deprecated some functions

## version 3.1-1

CRAN release: 2021-01-19

- fixed minor bugs (see github)

## version 3.1

CRAN release: 2020-07-01

- bb takes asp.limit argument which controls the maximum aspect ratio
- improved geocode_OSM when no results are found; also added argument
  keep.unfound
- simplify_shape now works on sfc objects and sf objects with only a
  geometry column

## version 3.0

CRAN release: 2020-03-30

- stars support
- removed hardcoded proj4 shortcuts
- geocode_OSM also returns a bounding box if as.sf=TRUE

## version 2.0-2

CRAN release: 2019-07-18

- sp-based functions marked as deprecated
- fixed bug in rev_geocode_OSM
- fixed bug in poly_to_raster

## version 2.0-1

CRAN release: 2018-08-07

- fixed bug in poly_to_raster
- fixed smooth_map bugs related to sp/sf conversions
- fixed bug in rev_geocode_OSM

## version 2.0

CRAN release: 2018-07-14

- tmaptools migrated from sp to sf, all functions support sf, but some
  still rely on sp
- bb now returns a vector instead of a matrix, similar to sf::st_bbox
- palette_explorer expanded with viridis palettes
- read_osm doens’t support vector OSM data anymore, since the osmdata
  package is a better alternative

## version 1.2-2

CRAN release: 2017-11-24

- improved smooth_map

## version 1.2-1

CRAN release: 2017-05-11

- added threshold argument to smooth_map
- fixed bug in approx_areas (all warnings now ignored when
  show.warnings=FALSE)
- fixed bug in simplify_shape (ms_simplify is not case sensitive)

## version 1.2

CRAN release: 2017-01-29

- added palette_explorer

## version 1.0

CRAN release: 2017-01-03

- first CRAN release after package split of tmap
- non-plotting functions of tmap 1.6-1 are contained in tmaptools
- added aggregate_map
- added simplify_shape
- added approx_distances, improved approx_areas, added projection_units
- added get_brewer_pal, to get and modify brewer palettes
- added bb_sp, to convert a bounding box to a SpatialPolygons object
- sf (simple features) objects supported
