#' Deprecated tmaptools functions
#'
#' The following functions are not used anymore or deprecated as of tmaptools version 2.0
#'
#' \itemize{
#'  \item \code{osm_poly}: for reading vector OSM data, use the \code{osmdata} package
#'  \item \code{osm_line}: for reading vector OSM data, use the \code{osmdata} package
#'  \item \code{osm_point}: for reading vector OSM data, use the \code{osmdata} package
#' }
#'
#' @rdname deprecated_functions
#' @name deprecated_functions
NULL

#' @rdname deprecated_functions
#' @name osm_poly
#' @keywords internal
#' @export
osm_poly <- function() {
    stop("for reading vector OSM data, use the osmdata package")
}

#' @rdname deprecated_functions
#' @name osm_line
#' @keywords internal
#' @export
osm_line <- function() {
    stop("for reading vector OSM data, use the osmdata package")
}

#' @rdname deprecated_functions
#' @name osm_point
#' @keywords internal
#' @export
osm_point <- function() {
    stop("for reading vector OSM data, use the osmdata package")
}
