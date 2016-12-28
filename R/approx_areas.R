#' Approximate area sizes of the shapes
#'
#' Approximate the area sizes of the polygons either in 1) absolute numbers based on the polygon coordinates, 2) proportional numbers, 3) normalized numbers and 4) squared units (e.g. kilometers).
#'
#' Note that this method is an approximation, since it depends on the used projection and the level of detail of the SpatialPolygons object. Projections with equal-area property are highly recommended.
#'
#' For projected shapes, \code{\link[rgeos:gArea]{gArea}} is used, and for unprojected shapes, \code{\link[geosphere:areaPolygon]{areaPolygon}}.
#'
#' @param shp shape object, i.e., a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}. Also \code{sf} objects are allowed.
#' @param unit one of
#' \describe{
#' 	\item{\code{"abs"}:}{Absolute numbers based on polygon coordinates.}
#' 	\item{\code{"prop"}:}{Proportional numbers. In other words, the sum of the area sizes equals one.}
#' 	\item{\code{"norm"}:}{Normalized numbers. All area sizes are normalized to the largest area, of which the area size equals one.}
#' 	\item{\code{"metric"} (default):}{Output area sizes will be either \code{"km"} (kilometer) or \code{"m"} (meter) depending on the map scale}
#' 	\item{\code{"imperial"}:}{Output area sizes will be either \code{"mi"} (miles) or \code{"ft"} (feet) depending on the map scale}
#' 	\item{other:}{Predefined values are "km", "m", "mi", and "ft". Other values can be specified as well. In that case, \code{unit.size} is required).}}
#' These units are the output units. See \code{coords.unit} for the coordinate units used by the shape \code{shp}.
#' @param coords.unit unit by which the coordinates are defined. By default, the value is taken from the crs projection string defined in \code{shp}. Not needed for non-projected shapes, since their areas are determined in another way (see details).
#' @param unit.size size of the target unit (specified by \code{unit}) in terms of the coordinate units (specified by \code{coords.unit}). Only needed when \code{unit} or \code{coords.unit} are unknown (i.e. not listed above) and cannot be determined. For instance, if unit is set to \code{"hm"} (hectameter), and \code{corods.unit} is \code{"m"}, then \code{unit.size} should be 100, meaning 1 hectameter equals 100 meters.
#' @param total.area total area size of \code{shp} in number of target units (defined by \code{unit}). Useful if the total area of the \code{shp} differs from a reference total area value. For \code{"metric"} and \code{"imperial"} units, please provide the total area in squared kilometers respectively miles.
#' @return Numeric vector of area sizes. An attribute called unit is added to specify the used units, which is especially useful when units were set to metric or imperial.
#' @example ./examples/approx_areas.R
#' @importFrom rgeos gArea
#' @importFrom geosphere areaPolygon
#' @export
approx_areas <- function(shp, unit="metric", coords.unit=NA, unit.size=NA, total.area=NA) {
    is_proj <- is_projected(shp)


    # determine area sizes and corresponding units (coords.units)
    if (is_proj) {
        x <- rgeos::gArea(shp, byid = TRUE)
        p <- get_projection(shp)
        if (is.na(coords.unit)) coords.unit <- get_shape_units(projection=p)$unit
    } else {
        x <- geosphere::areaPolygon(shp)
        coords.unit <- "m"
    }

    if (!(unit %in% c("abs", "prop", "norm"))) {
        # determine unit.size
        if (is.na(unit.size)) {
            unit.size <- convert_shape_units(coords.unit, unit)$to
            if (is.na(unit.size)) {
                if (!(coords.unit %in% names(to_m))) {
                    stop("Unknown coords.unit: please specify unit.size.")
                } else {
                    stop("Unknown unit specification: please specify unit.size.")
                }
            }
        }
    }

	if (is.na(total.area)) total.area <- sum(x)/(unit.size^2)
    denom <- switch(unit, norm=max(x), prop=sum(x), abs=1, sum(x)/total.area)
    x2 <- x / denom

    # revert back to meters or feet is needed
    if (unit=="metric") {
        if (max(x2) < 1) {
            x2 <- x2 * 1e6
            unit <- "m"
        } else {
            unit <- "km"
        }
    } else if (unit=="imperial") {
        if (max(x2) < 1) {
            x2 <- x2 * 27878400
            unit <- "ft"
        } else {
            unit <- "mi"
        }
    }

    if (!(unit %in% c("abs", "prop", "norm"))) unit <- paste("sq", unit)
    attr(x2, "unit") <- unit
    x2
}
