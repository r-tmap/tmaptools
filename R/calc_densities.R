#' Calculate densities
#'
#' Transpose quantitative variables to densitiy variables, which are often needed for choroplets. For example, the colors of a population density map should correspond population density counts rather than absolute population numbers.
#'
#' @param shp a shape object, i.e., a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}, or a \code{sf} object that can be coerced as such.
#' @param var name(s) of a qualtity variable name contained in the \code{shp} data
#' @param target the target unit, see \code{\link{approx_areas}}. Density values are calculated in \code{var/target^2}.
#' @param orig original units, i.e. by which the coordinates are defined, see \code{\link{approx_areas}}.
#' @param to multiplier used as follows: \code{orig * to = target}. See \code{\link{approx_areas}}.
#' @param total.area total area size of \code{shp} in number of target units (defined by \code{unit}), \code{\link{approx_areas}}.
#' @param suffix character that is appended to the variable names. The resulting names are used as column names of the returned data.frame. By default, \code{_sq_<target>}, where target corresponds to the target unit, e.g. \code{_sq_km}
#' @param drop boolean that determines whether an one-column data-frame should be returned as a vector
#' @keywords densities
#' @return Vector or data.frame (depending on whether \code{length(var)==1} with density values. This can be appended directly to the shape file with \code{\link{append_data}} with \code{fixed.order=TRUE}.
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \href{https://doi.org/10.18637/jss.v084.i06}{DOI}
#' @example ./examples/calc_densities.R
#' @export
calc_densities <- function(shp, var, target="metric", orig=NA, to=NA, total.area=NA, suffix=NA, drop=TRUE) {
	## calculate densities
    if (inherits(shp, c("sf", "sfc"))) shp <- as(shp, "Spatial")

	areas <- approx_areas(shp, target = target, orig = orig, to=to, total.area=total.area)

	areas_unit <- attr(areas, "unit")

	if (is.na(suffix)) suffix <- paste("_", sub(" ", replacement = "_", areas_unit), sep = "")

	## calculate and return densities
    if (length(var)==1 && drop) return(shp@data[[var]] / areas)

    data <- as.data.frame(lapply(shp@data[, var, drop=FALSE], function(x)x/areas))
	names(data) <- paste(var, suffix, sep="")
	data
}

