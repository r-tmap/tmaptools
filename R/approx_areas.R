#' Approximate area sizes of the shapes
#'
#' Approximate the area sizes of the polygons either in 1) absolute numbers based on the polygon coordinates, 2) proportional numbers, 3) normalized numbers and 4) squared units (e.g. kilometers).
#'
#' Note that this method is an approximation, since it depends on the used projection and the level of detail of the SpatialPolygons object. Projections with equal-area property are highly recommended.
#'
#' For projected shapes, \code{\link[rgeos:gArea]{gArea}} is used, and for unprojected shapes, \code{\link[geosphere:areaPolygon]{areaPolygon}}.
#'
#' @param shp shape object, i.e., a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}
#' @param unit one of
#' \describe{
#' 	\item{\code{"abs"}:}{Absolute numbers based on polygon coordinates.}
#' 	\item{\code{"prop"}:}{Proportional numbers. In other words, the sum of the area sizes equals one.}
#' 	\item{\code{"norm"}:}{Normalized numbers. All area sizes are normalized to the largest area, of which the area size equals one.}
#' 	\item{other:}{Predefined For instance, "km", "m", "miles", or "us-ft". These units are the output units. See \code{coords.unit} for the units that are used by which the coordinates are defined. For this method, \code{total.area} or \code{unit.size} is required. In this case, the area sizes are returned in squared units, e.g., in squared kilometers.}}
#' 	The default method is \code{"km"}.
#' @param coords.unit unit by which the coordinates are defined. Only applicable for projected shapes. By default, the value is taken from the crs projection string.
#' @param unit.size size of the target unit (specified by \code{unit}) in terms of the coordinate units (specified by \code{coords.unit}). For instance, if \code{unit="km"} and \code{coords.unit="m"}, then \code{unit.size} shoudl be 1000. Only needs to be specified if units are used other than "km", "m", "miles", or "us-ft".
#' @param total.area total area size of \code{shp} in number of target units (defined by \code{unit}). Useful if the total area of the \code{shp} differs from a reference total area value.
#' @return Numeric vector of area sizes.
#' @example ./examples/approx_areas.R
#' @importFrom rgeos gArea
#' @importFrom geosphere areaPolygon
#' @export
approx_areas <- function(shp, unit="km", coords.unit=NA, unit.size=NA, total.area=NA) {
    is_proj <- is_projected(shp)

    to_m <- c(m=1, km=1000, miles=1609.344, 'us-ft'=0.304800609601219, NA)

    # determine area sizes and corresponding units (coords.units)
    if (is_proj) {
        x <- rgeos::gArea(shp, byid = TRUE)
        p <- get_projection(shp)
        if (is.na(coords.unit)) {
            pat <- '^.*\\+units ?= ?(\\S*)(.*)$'
            coords.unit <- sub(pat, '\\1', p[grepl(pat,p)])
        }
    } else {
        x <- geosphere::areaPolygon(shp)
        coords.unit <- "m"
    }

    if (!(unit %in% c("abs", "prop", "norm"))) {
        # determine unit.size
        if (is.na(unit.size)) {
            unit.size <- to_m[unit] / to_m[coords.unit]
            if (is.na(unit.size)) {
                if (!(coord.unit %in% names(to_m))) {
                    stop("Unknown coord.unit: please specify unit.size.")
                } else {
                    stop("Unknown unit specification: please specify unit.size.")
                }
            }
        }
    }

	if (is.na(total.area)) total.area <- sum(x)/(unit.size^2)
    denom <- switch(unit, norm=max(x), prop=sum(x), abs=1, sum(x)/total.area)
    x / denom
}
