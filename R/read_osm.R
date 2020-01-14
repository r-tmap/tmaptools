#' Read Open Street Map data
#'
#' Read Open Street Map data. OSM tiles are read and returned as a spatial raster. Vectorized OSM data is not supported anymore (see details).
#'
#' As of version 2.0, \code{read_osm} cannot be used to read vectorized OSM data anymore. The reason is that the package that was used under the hood, \code{osmar}, has some limitations and is not actively maintained anymore. Therefore, we recommend the package \code{osmdata}. Since this package is very user-friendly, there was no reason to use \code{read_osm} as a wrapper for reading vectorized OSM data.
#'
#' @param x object that can be coerced to a bounding box with \code{\link{bb}} (e.g. an existing bounding box or a shape). In the first case, other arguments can be passed on to \code{\link{bb}} (see \code{...}). If an existing bounding box is specified in projected coordinates, plesae specify \code{current.projection}.
#' @param zoom passed on to \code{\link[OpenStreetMap:openmap]{openmap}}. Only applicable when \code{raster=TRUE}.
#' @param type tile provider, by default \code{"osm"}, which corresponds to OpenStreetMap Mapnik. See \code{\link[OpenStreetMap:openmap]{openmap}} for options. Only applicable when \code{raster=TRUE}.
#' @param minNumTiles passed on to \code{\link[OpenStreetMap:openmap]{openmap}} Only applicable when \code{raster=TRUE}.
#' @param mergeTiles passed on to \code{\link[OpenStreetMap:openmap]{openmap}} Only applicable when \code{raster=TRUE}.
#' @param use.colortable should the colors of the returned raster object be stored in a \code{\link[raster:colortable]{colortable}}? If \code{FALSE}, a RasterStack is returned with three layers that correspond to the red, green and blue values betweeen 0 and 255.
#' @param raster deprecated
#' @param ... arguments passed on to \code{\link{bb}}.
#' @name read_osm
#' @rdname read_osm
#' @importFrom raster raster
#' @export
#' @example ./examples/read_osm.R
#' @return The output of \code{read_osm} is a \code{\link[raster:raster]{raster}} object.
read_osm <- function(x, zoom=NULL, type="osm", minNumTiles=NULL, mergeTiles=NULL, use.colortable = FALSE, raster, ...) {
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

	x <- do.call("bb", c(list(x=x, projection = .crs_longlat), args_bb))

	if (!requireNamespace("OpenStreetMap", quietly = TRUE)) {
		stop("OpenStreetMap package needed for this function to work. Please install it.",
			 call. = FALSE)
	} else {
		openmap <- get("openmap", envir=asNamespace("OpenStreetMap"), mode="function")


		optionalArgs <- list(zoom=zoom, type=type, minNumTiles=minNumTiles, mergeTiles=mergeTiles)
		optionalArgs <- optionalArgs[!sapply(optionalArgs, is.null)]
		om <- suppressWarnings({do.call("openmap", args = c(list(upperLeft=x[c(4,1)], lowerRight=x[c(2,3)]), optionalArgs))})

		omr <- raster::raster(om)

# 		omr2 <- raster::raster(om)
#
#
#         plot(omr2)
#
#
#         all(omr2$layer.1[,] == cols[1,])
#         all(omr2$layer.2[,] == cols[2,])
#
# 		cols <- col2rgb(om$tiles[[1]]$colorData)
# 		dimy <- om$tiles[[1]]$xres
# 		dimx <- om$tiles[[1]]$yres
#
# 		bbox <- unlist(om$tiles[[1]]$bbox)[c(1,3,4,2)]
# 		names(bbox) <- c("xmin", "xmax", "ymin", "ymax")
#
# 		bbx <- st_bbox(bbox, crs = sf::st_crs(attr((om$tiles[[1]]$projection), "projargs")))
#
#
# 		stars::st_as_stars()
#
# 		red <- matrix(cols[1,], ncol = dimx, byrow = FALSE)
# 		dim(red) <- c(y = dimy, x = dimx)
#
# 		green <- matrix(cols[1,], ncol = dimx, byrow = FALSE)
# 		dim(green) <- c(y = dimy, x = dimx)
#
# 		blue <- matrix(cols[1,], ncol = dimx, byrow = FALSE)
# 		dim(blue) <- c(y = dimy, x = dimx)
#
# 		omr3 <- stars::st_as_stars(omr2)
# 		plot(omr3)
#
#
# 		a <- array(c(red, green, blue), dim = c(dim(red), 3))
# 		dim(a) <- c(y = unname(nrow(red)), x = unname(ncol(red)), band = 3)
#
# 		omr4 <- stars::st_as_stars(list(a))
# 		omr5 <- stars::st_as_stars(bbx, nx = dimx, ny = dimy)
#
# 		attr(omr4, "dimensions")[1:2] <- attr(omr5, "dimensions")
#
# 		attr(attr(omr4, "dimensions"), "raster") <- attr(attr(omr5, "dimensions"), "raster")
#
#
# 		plot(omr4)
#
# 		, dimensions = attr(stars::st_as_stars(bbx, nx = dimx, ny = dimy), "dimensions"))
#
# 		plot(omr4)
#
# 		plot(omr3)
#
#
#
# 		omr <- st_dimensions(stars::st_as_stars(bbx, nx = dimx, ny = dimy))
# 		#attr(omr, "dimensions")[[2]]$delta = -1
#
# 		plot(omr)
#
# 		omr <- st_as_stars(red)
#
# 		attr(omr, "dimensions") <- attr(omr3, "dimensions")
#
# 		plot(omr)
#
#
#
# 		st_dimensions(omr)
#
# 		stars::r
#
# 		str(attr(st_dimensions(omr), "raster"))
#
#         plot(omr)
#
# 		m = matrix(1:6, ncol = 2)
# 		dim(m) = c(x = 3, y = 2)
# 		omr <- stars::st_as_stars(bbx, nx = 3, ny = 2, values = m)
# 		plot(omr)
#
#
# 		omr <- stars::st_as_stars(bbx)


		# if (use.colortable) {
		#     tab <- raster_colors(raster::values(omr), use.colortable = TRUE)
		#     omr <- raster::raster(omr)
		#     omr <- raster::setValues(omr, as.integer(tab) - 1L)
		#     raster::colortable(omr) <- levels(tab)
		# }

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


