#' Read shape file
#'
#' Read an ESRI shape file. Optionally, set the current projection if it is missing.
#'
#' This function is a convenient wrapper of rgdal's \code{\link[rgdal:readOGR]{readOGR}}. It is possible to set the current projection, if it is undefined in the shape file. If a reprojection is required, use \code{\link{set_projection}}.
#'
#' For the Netherlands: often, the Dutch Rijksdriehoekstelsel (Dutch National Grid) projection is provided in the shape file without proper datum shift parameters to wgs84. This functions automatically adds these parameters. See \url{http://www.qgis.nl/2011/12/05/epsg28992-of-rijksdriehoekstelsel-verschuiving/} (in Dutch) for details.
#'
#' @param file a shape file name (including directory)
#' @param current.projection the current projection of the shape object, if it is missing in the shape file. See \code{\link{get_proj4}} for options. Use \code{\link{set_projection}} to reproject the shape object.
#' @param as.sf should the shape be returned as an \code{sf} object?
#' @param ... other parameters, such as \code{stringsAsFactors}, are passed on to \code{\link[rgdal:readOGR]{readOGR}}
#' @return shape object from class \code{\link[sp:Spatial]{Spatial}} or \code{sf} if \code{as.sf = TRUE}
#' @importFrom rgdal readOGR
#' @import sp
#' @export
read_shape <- function(file, current.projection=NULL, as.sf=FALSE, ...){

	# determine region ID
	if (file.exists(file)) {
		fullfilename <- file
	} else stop("unknown filename", call. = FALSE)

	dir <- dirname(fullfilename)
	base <- basename(fullfilename)
	if (substr(base, nchar(base)-3, nchar(base))==".shp")
		base <- substr(base, 1, nchar(base)-4)

	shp <- readOGR(dir, base, verbose=FALSE, ...)

	prj <- proj4string(shp)

	if (is.na(prj)) {
		if (missing(current.projection)) {
			warning("Current projection missing. Set the parameter current.project, or use set_projection", call. = FALSE)
		} else {
			shp <- set_projection(shp, current.projection=current.projection)
		}
	} else {
		if (!missing(current.projection)) {
			warning("Projection already specified in shape file. Use set_projection for reprojection.", call. = FALSE)
		}

		## rd projection correction: add towgs84 parameter to frequently used rd projection strings
		if (prj %in% c("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs",
					   "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs",
					   "+proj=sterea +lat_0=52.156161 +lon_0=5.387639 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")) {
			shp <- suppressWarnings(set_projection(shp, current.projection="rd", overwrite.current.projection=TRUE))
		}
	}
	if (as.sf) as(shp, "sf") else shp
}

#' Write shape file
#'
#' Write a shape object to an ESRI shape file.
#'
#' This function is a convenient wrapper of rgdal's \code{\link[rgdal:writeOGR]{writeOGR}}.
#'
#' @param shp a shape object
#' @param file file name (including directory)
#' @import sp
#' @importFrom rgdal writeOGR
#' @export
write_shape <- function(shp, file) {
	shpname <- deparse(substitute(shp))
	dir <- dirname(file)
	base <- basename(file)
	if (!file.exists(dir)) stop("unknown directory", call. = FALSE)

	if (substr(base, nchar(base)-3, nchar(base))==".shp")
		base <- substr(base, 1, nchar(base)-4)
	if (inherits(shp, c("sf", "sfc"))) shp <- as(shp, "Spatial")
	if (!inherits(shp, "Spatial")) stop("shpname is not a Spatial object", call. = FALSE)
	writeOGR(shp, dir, base, driver = "ESRI Shapefile", overwrite_layer=TRUE)
}
