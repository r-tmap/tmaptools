#' Approximate area sizes of the shapes
#'
#' Approximate the area sizes of the polygons in real-world area units (such as sq km or sq mi), absolute numbers based on the polygon coordinates, proportional numbers, or normalized numbers.
#'
#' Note that this method is an approximation, since it depends on the used projection and the level of detail of the shape object. Projections with equal-area property are highly recommended. See \url{https://en.wikipedia.org/wiki/List_of_map_projections} for equal area world map projections.
#'
#' For projected shapes, \code{\link[rgeos:gArea]{gArea}} is used, and for unprojected shapes, \code{\link[geosphere:areaPolygon]{areaPolygon}}.
#'
#' @param shp shape object, i.e., a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}, or an \code{sf} object that can be coerced as such.
#' @param target target unit, one of
#' \describe{
#' 	\item{\code{"abs"}:}{Absolute numbers based on polygon coordinates.}
#' 	\item{\code{"prop"}:}{Proportional numbers. In other words, the sum of the area sizes equals one.}
#' 	\item{\code{"norm"}:}{Normalized numbers. All area sizes are normalized to the largest area, of which the area size equals one.}
#' 	\item{\code{"metric"} (default):}{Output area sizes will be either \code{"km"} (kilometer) or \code{"m"} (meter) depending on the map scale}
#' 	\item{\code{"imperial"}:}{Output area sizes will be either \code{"mi"} (miles) or \code{"ft"} (feet) depending on the map scale}
#' 	\item{other:}{Predefined values are "km^2", "m^2", "mi^2", and "ft^2". Other values can be specified as well, in which case \code{to} is required).}}
#' These units are the output units. See \code{orig} for the coordinate units used by the shape \code{shp}.
#' @param orig original unit, i.e. by which the coordinates are defined. By default, the value is taken from the crs projection string defined in \code{shp}. Not needed for non-projected shapes, since their areas are determined in another way (see details).
#' @param to multiplier used as follows: \code{orig * to = target}. Only needed when \code{orig} or \code{target} is unknown. For instance, if \code{target} is set to \code{"hm"} (hectameter), and \code{orig} is \code{"m"}, then \code{to} should be 100, meaning 1 hectameter equals 100 meters.
#' @param total.area total area size of \code{shp} in number of target units (defined by \code{target}). Useful if the total area of the \code{shp} differs from a reference total area value. For \code{"metric"} and \code{"imperial"} units, please provide the total area in squared kilometers respectively miles.
#' @param show.warnings should warnings be shown?
#' @return Numeric vector of area sizes. An attribute called unit is added to specify the used target units, which is especially useful when units were set to metric or imperial.
#' @example ./examples/approx_areas.R
#' @seealso \code{\link{projection_units}} and \code{\link{approx_distances}}
#' @importFrom rgeos gArea
#' @importFrom units set_units parse_unit
#' @export
approx_areas <- function(shp, target="metric", total.area=NULL) {
    is_metric <- target=="metric"
    is_imperial <- target=="imperial"

    if (is_metric) target <- "km km"
    if (is_imperial) target <- "mi mi"

    nct <- nchar(target)
    if (substr(target, nct-1, nct) == "^2") target <- paste(substr(target, 1, nct-2), substr(target, 1, nct-2))

    if (inherits(shp, "Spatial")) shp <- as(shp, "sf")

    areas <- sf::st_area(shp)

    if (target == "prop") {
        areas <- areas / sum(areas)
    } else if (target == "norm") {
        areas <- areas / max(areas)
    } else {
        areas <- set_units(areas, parse_unit(target))
        if (!is.null(total.area)) {
            fact <- total.area / sum(areas)
            areas <- areas * fact
        }
    }

    areas
}
#'
#'
# approx_areas <- function(shp, target="metric", orig=NA, to=NA, total.area=NA, show.warnings=TRUE) {
#
#     is_metric <- target=="metric"
#     is_imperial <- target=="imperial"
#
#     if (is_metric) target <- "km"
#     if (is_imperial) target <- "mi"
#
#     if (!(target %in% c("abs", "prop", "norm"))) {
#         res <- projection_units(get_projection(shp), target=target, orig=orig, to=to)
#
#         projected <- res$projected
#         newtarget <- res$target
#         orig <- res$orig
#         to <- res$to
#     } else {
#         projected <- is_projected(shp)
#         newtarget <- target
#     }
#
#     # determine area sizes and corresponding units
#     if (projected) {
#         x <- rgeos::gArea(shp, byid = TRUE)
#     } else {
#         x <- geosphere::areaPolygon(shp)
#         orig <- "m"
#         to <- to_m["m"] / to_m[newtarget]
#     }
#
#     if (!(target %in% c("abs", "prop", "norm")) && is.na(to)) {
#         if (show.warnings) warning("Target unit or original unit unknown. Please specify valid the arguments target and orig, or the argument to")
#         target <- "abs"
#     }
#
#     if (any(is.na(x)) || any(is.infinite(x))) {
#         naid <- sort(union(which(is.na(x)), which(is.infinite(x))))
#         if (show.warnings) warning("cannot determine area of polygon(s) ", paste(naid, collapse=","))
#     }
#
# 	if (is.na(total.area)) total.area <- sum(x, na.rm = TRUE) * (to^2)
#     denom <- switch(target, norm=max(x), prop=sum(x), abs=1, sum(x, na.rm = TRUE)/total.area)
#     x2 <- x / denom
#
#     # revert back to meters or feet is needed
#     if (is_metric) {
#         if (max(x2, na.rm = TRUE) < 1) {
#             x2 <- x2 * 1e6
#             newtarget <- "m"
#         }
#     } else if (is_imperial) {
#         if (max(x2, na.rm = TRUE) < 1) {
#             x2 <- x2 * 27878400
#             newtarget <- "ft"
#         }
#     }
#
#     if (!(target %in% c("abs", "prop", "norm"))) newtarget <- paste("sq", newtarget)
#     attr(x2, "unit") <- newtarget
#     x2
# }
