# version 3.2
- examples updated with tmap4
- deprecated some functions

# version 3.1-1
- fixed minor bugs (see github)

# version 3.1
- bb takes asp.limit argument which controls the maximum aspect ratio
- improved geocode_OSM when no results are found; also added argument keep.unfound
- simplify_shape now works on sfc objects and sf objects with only a geometry column

# version 3.0
- stars support
- removed hardcoded proj4 shortcuts
- geocode_OSM also returns a bounding box if as.sf=TRUE

# version 2.0-2
- sp-based functions marked as deprecated
- fixed bug in rev_geocode_OSM
- fixed bug in poly_to_raster

# version 2.0-1
- fixed bug in poly_to_raster
- fixed smooth_map bugs related to sp/sf conversions
- fixed bug in rev_geocode_OSM

# version 2.0
- tmaptools migrated from sp to sf, all functions support sf, but some still rely on sp
- bb now returns a vector instead of a matrix, similar to sf::st_bbox
- palette_explorer expanded with viridis palettes
- read_osm doens't support vector OSM data anymore, since the osmdata package is a better alternative

# version 1.2-2
- improved smooth_map

# version 1.2-1
- added threshold argument to smooth_map
- fixed bug in approx_areas (all warnings now ignored when show.warnings=FALSE)
- fixed bug in simplify_shape (ms_simplify is not case sensitive)

# version 1.2
- added palette_explorer

# version 1.0
- first CRAN release after package split of tmap
- non-plotting functions of tmap 1.6-1 are contained in tmaptools
- added aggregate_map
- added simplify_shape
- added approx_distances, improved approx_areas, added projection_units
- added get_brewer_pal, to get and modify brewer palettes
- added bb_sp, to convert a bounding box to a SpatialPolygons object
- sf (simple features) objects supported
