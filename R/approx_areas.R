#' Approximate area sizes of the shapes
#'
#' Approximate the area sizes of the polygons in real-world area units (such as sq km or sq mi), proportional numbers, or normalized numbers. Also, the areas can be calibrated to a prespecified area total. This function is a convenient wrapper around \code{\link[sf:geos_measures]{st_area}}.
#'
#' Note that the method of determining areas is an approximation, since it depends on the used projection and the level of detail of the shape object. Projections with equal-area property are highly recommended. See \url{https://en.wikipedia.org/wiki/List_of_map_projections} for equal area world map projections.
#'
#' @param shp shape object, i.e., an \code{\link[sf:sf]{sf}} or \code{sp} object.
#' @param target target unit, one of
#' \describe{
#' 	\item{\code{"prop"}:}{Proportional numbers. In other words, the sum of the area sizes equals one.}
#' 	\item{\code{"norm"}:}{Normalized numbers. All area sizes are normalized to the largest area, of which the area size equals one.}
#' 	\item{\code{"metric"} (default):}{Output area sizes will be either \code{"km"} (kilometer) or \code{"m"} (meter) depending on the map scale}
#' 	\item{\code{"imperial"}:}{Output area sizes will be either \code{"mi"} (miles) or \code{"ft"} (feet) depending on the map scale}
#' 	\item{other:}{Predefined values are "km^2", "m^2", "mi^2", and "ft^2". Other values can be specified as well, in which case \code{to} is required).}}
#' These units are the output units. See \code{orig} for the coordinate units used by the shape \code{shp}.
#' @param total.area total area size of \code{shp} in number of target units (defined by \code{target}). Useful if the total area of the \code{shp} differs from a reference total area value. For \code{"metric"} and \code{"imperial"} units, please provide the total area in squared kilometers respectively miles.
#' @return Numeric vector of area sizes (class \code{\link[units:units]{units}}).
#' @example ./examples/approx_areas.R
#' @seealso \code{\link{approx_distances}}
#' @importFrom units set_units as_units
#' @importFrom lwgeom st_transform_proj st_geod_area
#' @export
approx_areas <- function(shp, target="metric", total.area=NULL) {
    is_metric <- target=="metric"
    is_imperial <- target=="imperial"

    if (is_metric) target <- "km km"
    if (is_imperial) target <- "mi mi"

    nct <- nchar(target)
    if (substr(target, nct-1, nct) == "^2") target <- paste(substr(target, 1, nct-2), substr(target, 1, nct-2))

    if (inherits(shp, "Spatial")) shp <- as(shp, "sf")

    areas <- tryCatch(sf::st_area(shp),
                  error = function(e) {
                      lwgeom::st_geod_area(lwgeom::st_transform_proj(shp, crs = 4326))
                  })

    if (target == "prop") {
        areas <- areas / sum(areas)
    } else if (target == "norm") {
        areas <- areas / max(areas)
    } else {
        areas <- set_units(areas, as_units(target), mode = "standard")
        if (!is.null(total.area)) {
            fact <- total.area / sum(areas)
            areas <- areas * fact
        }
    }

    areas
}
