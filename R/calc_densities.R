#' Calculate densities
#'
#' Transpose quantitative variables to densitiy variables, which are often needed for choroplets. For example, the colors of a population density map should correspond population density counts rather than absolute population numbers.
#'
#' @param shp a shape object, i.e., an \code{\link[sf:sf]{sf}} object or a \code{SpatialPolygons(DataFrame)} from the \code{sp} package.
#' @param var name(s) of a qualtity variable name contained in the \code{shp} data
#' @param target the target unit, see \code{\link{approx_areas}}. Density values are calculated in \code{var/target^2}.
#' @param total.area total area size of \code{shp} in number of target units (defined by \code{unit}), \code{\link{approx_areas}}.
#' @param suffix character that is appended to the variable names. The resulting names are used as column names of the returned data.frame. By default, \code{_sq_<target>}, where target corresponds to the target unit, e.g. \code{_sq_km}
#' @param drop boolean that determines whether an one-column data-frame should be returned as a vector
#' @keywords densities
#' @return Vector or data.frame (depending on whether \code{length(var)==1} with density values.
#' @example ./examples/calc_densities.R
#' @export
calc_densities <- function(shp, var, target="metric", total.area=NULL, suffix=NA, drop=TRUE) {
	## calculate densities
    #if (inherits(shp, c("sf", "sfc"))) shp <- as(shp, "Spatial")

    if (inherits(shp, "Spatial")) shp <- as(shp, "sf")

	areas <- approx_areas(shp, target = target, total.area=total.area)

	areas_unit <- attr(areas, "units")

	if (is.na(suffix)) suffix <- paste("_", sub(" ", replacement = "_", areas_unit), sep = "")

	## calculate and return densities
	shp <- sf::st_set_geometry(shp, NULL)

    if (length(var)==1 && drop) return(shp[[var]] / areas)

    data <- as.data.frame(lapply(shp[, var, drop=FALSE], function(x)x/areas))
	names(data) <- paste(var, suffix, sep="")
	data
}

