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
#' \code{\link{bb_poly}}\tab Convert bounding box to a polygon \cr
#' \code{\link{get_asp_ratio}}\tab Get the aspect ratio of a shape object \cr
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
#' \code{\link{crop_shape}}\tab Crop shape objects \cr
#' \code{\link{simplify_shape}}\tab Simplify a shape \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#'
#' @section Input and output functions:
#' \tabular{ll}{
#' \code{\link{geocode_OSM}}\tab Get a location from an address description \cr
#' \code{\link{read_GPX}}\tab Read a GPX file \cr
#' \code{\link{read_osm}}\tab Read Open Street Map data \cr
#' \code{\link{rev_geocode_OSM}}\tab Get an address description from a location \cr
#' --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr
#' }
#'
#' @name tmaptools-package
#' @aliases tmaptools
#' @docType package
#' @author Martijn Tennekes \email{mtennekes@@gmail.com}
#' @concept GIS
#' @concept thematic maps
#' @concept spatial data
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
NULL
