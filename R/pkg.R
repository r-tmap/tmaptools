#' Thematic Map Tools
#'
#' This package offers a set of handy tool functions for reading and processing spatial data. The aim of these functions is to supply the workflow to create thematic maps, e.g. read shape files, set map projections, append data, calculate areas and distances, and query OpenStreetMap. The visualization of thematic maps can be done with the tmap package.
#'
#' This page provides a brief overview of all package functions.
#'
#' @section Tool functions (shape):
#' \tabular{ll}{
#' \code{\link{approx_areas}}\tab Approximate area sizes of polygons \cr
#' \code{\link{approx_distances}}\tab Approximate distances \cr
#' \code{\link{bb}}\tab Create, extract or modify a bounding box \cr
#' \code{\link{bb_sp}}\tab Convert bounding box to a spatial polygon \cr
#' \code{\link{get_asp_ratio}}\tab Get the aspect ratio of a shape object \cr
#' \code{\link{get_IDs}}\tab Get ID values of a shape object \cr
#' \code{\link{is_projected}}\tab Check if the map is projected \cr
#' \code{\link{get_projection}}\tab Get the map projection \cr
#' \code{\link{projection_units}}\tab Get or translate units of a projection \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#'
#' @section Tool functions (data):
#' \tabular{ll}{
#' \code{\link{append_data}}\tab Append a data frame to a shape object \cr
#' \code{\link{calc_densities}}\tab Calculate density values \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#'
#' @section Tool functions (colors):
#' \tabular{ll}{
#' \code{\link{get_brewer_pal}}\tab Get and plot a (modified) Color Brewer palette \cr
#' \code{\link{map_coloring}}\tab Find different colors for adjacent polygons \cr
#' \code{\link{palette_explorer}}\tab Explore Color Brewer palettes \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#'
#' @section Spatial transformation functions:
#' \tabular{ll}{
#' \code{\link{aggregate_map}}\tab Aggregate the units of a map \cr
#' \code{\link{crop_shape}}\tab Crop shape objects \cr
#' \code{\link{points_to_raster}}\tab Bin spatial points to a raster \cr
#' \code{\link{poly_to_raster}}\tab Convert polygons to a raster \cr
#' \code{\link{sbind}}\tab Bind shape objects \cr
#' \code{\link{sample_dots}}\tab Sample dots from polygons \cr
#' \code{\link{set_projection}}\tab Set the map projection \cr
#' \code{\link{simplify_shape}}\tab Simplify a shape \cr
#' \code{\link{smooth_map}}\tab Create a smooth map using a kernel density estimator \cr
#' \code{\link{smooth_raster_cover}}\tab Create a smooth cover from a raster object \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#'
#' @section Input and output functions:
#' \tabular{ll}{
#' \code{\link{geocode_OSM}}\tab Get a location from an address description \cr
#' \code{\link{read_GPX}}\tab Read a GPX file \cr
#' \code{\link{read_osm}}\tab Read Open Street Map data \cr
#' \code{\link{read_shape}}\tab Read a shape object \cr
#' \code{\link{rev_geocode_OSM}}\tab Get an address description from a location \cr
#' \code{\link{write_shape}}\tab Write a shape object \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#'
#' @name tmaptools-package
#' @aliases tmaptools
#' @docType package
#' @author Martijn Tennekes \email{mtennekes@@gmail.com}
#' @keywords GIS, thematic maps, spatial data
NULL

#' Pipe operator
#'
#' The pipe operator from magrittr, \code{\%>\%}, can also be used in functions from \code{tmaptools}.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs Left-hand side
#' @param rhs Right-hand side
#' @example ./examples/pipe.R
NULL
