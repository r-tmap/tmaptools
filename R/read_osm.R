#' Read Open Street Map data
#'
#' Read Open Street Map data. OSM tiles are read and returned as a spatial raster. Vectorized OSM data is not supported anymore (see details).
#'
#' As of version 2.0, \code{read_osm} cannot be used to read vectorized OSM data anymore. The reason is that the package that was used under the hood, \code{osmar}, has some limitations and is not actively maintained anymore. Therefore, we recommend the package \code{osmdata}. Since this package is very user-friendly, there was no reason to use \code{read_osm} as a wrapper for reading vectorized OSM data.
#'
#' @param x object that can be coerced to a bounding box with \code{\link{bb}} (e.g. an existing bounding box or a shape), or an \code{\link[osmar:osmar]{osmar}} object. In the first case, other arguments can be passed on to \code{\link{bb}} (see \code{...}). If an existing bounding box is specified in projected coordinates, plesae specify \code{current.projection}.
#' @param zoom passed on to \code{\link[OpenStreetMap:openmap]{openmap}}. Only applicable when \code{raster=TRUE}.
#' @param type tile provider, by default \code{"osm"}, which corresponds to OpenStreetMap Mapnik. See \code{\link[OpenStreetMap:openmap]{openmap}} for options. Only applicable when \code{raster=TRUE}.
#' @param minNumTiles passed on to \code{\link[OpenStreetMap:openmap]{openmap}} Only applicable when \code{raster=TRUE}.
#' @param mergeTiles passed on to \code{\link[OpenStreetMap:openmap]{openmap}} Only applicable when \code{raster=TRUE}.
#' @param use.colortable should the colors of the returned raster object be stored in a \code{\link[raster:colortable]{colortable}}? If \code{FALSE}, a RasterStack is returned with three layers that correspond to the red, green and blue values betweeen 0 and 255.
#' @param raster deprecated
#' @param ... arguments passed on to \code{\link{bb}}.
#' @name read_osm
#' @rdname read_osm
#' @import sp
#' @importFrom raster raster
#' @export
#' @example ./examples/read_osm.R
#' @return The output of \code{read_osm} is a \code{\link[raster:raster]{raster}} object.
read_osm <- function(x, zoom=NULL, type="osm", minNumTiles=NULL, mergeTiles=NULL, use.colortable = TRUE, raster, ...) {
	if (!get(".internet", envir = .TMAPTOOLS_CACHE)) stop("No internet connection found.")

    if (!missing(raster)) {
        warning("The argument raster is deprecated, since read_osm only returns raster as of tmaptools version 2.0 (see details section in the documentation)")
        if (!raster) stop("Reading vectorized OSM data is not implemented anymore. Please use the osmdata package.")
    }

	# @importFrom OpenStreetMap openmap
	k <- v <- NULL
	args <- list(...)

	args_bb <- args[intersect(names(args), c("ext", "cx", "cy", "width", "height", "xlim", "ylim", "relative"))]
	args_other <- args[setdiff(names(args), names(args_bb))]
	if (!(inherits(x,  "osmar"))) x <- do.call("bb", c(list(x=x, projection = .crs_longlat), args_bb))

	if (!requireNamespace("OpenStreetMap", quietly = TRUE)) {
		stop("OpenStreetMap package needed for this function to work. Please install it.",
			 call. = FALSE)
	} else {
		openmap <- get("openmap", envir=asNamespace("OpenStreetMap"), mode="function")


		optionalArgs <- list(zoom=zoom, type=type, minNumTiles=minNumTiles, mergeTiles=mergeTiles)
		optionalArgs <- optionalArgs[!sapply(optionalArgs, is.null)]
		om <- suppressWarnings({do.call("openmap", args = c(list(upperLeft=x[c(4,1)], lowerRight=x[c(2,3)]), optionalArgs))})
		omr <- raster::raster(om)

		if (use.colortable) {
		    tab <- raster_colors(raster::values(omr))
		    omr <- raster::raster(omr)
		    omr <- raster::setValues(omr, as.integer(tab) - 1L)
		    raster::colortable(omr) <- levels(tab)
		}

		attr(omr, "leaflet.provider") <- unname(OSM2LP[type])
		attr(omr, "is.OSM") <- TRUE
		return(omr)
	}
}

# matching between types available at openmap and leaflet providers
OSM2LP <- c("osm"="OpenStreetMap.Mapnik",
       "osm-bw"="OpenStreetMap.BlackAndWhite",
       "maptoolkit-topo"=NA,
       "waze"=NA,
       "bing"=NA,
       "stamen-toner"="Stamen.Toner",
       "stamen-terrain"="Stamen.Terrain",
       "stamen-watercolor"="Stamen.Watercolor",
       "osm-german"="OpenStreetMap.DE",
       "osm-wanderreitkarte"=NA,
       "mapbox"="MapBox",
       "esri"="Esri.WorldStreetMap",
       "esri-topo"="Esri.WorldTopoMap",
       "nps"=NA,
       "apple-iphoto"=NA,
       "skobbler"=NA,
       "hillshade"="HikeBike.HillShading",
       "opencyclemap"="Thunderforest.OpenCycleMap",
       "osm-transport"="Thunderforest.Transport",
       "osm-public-transport"=NA,
       "osm-bbike"=NA,
       "osm-bbike-german"=NA)


