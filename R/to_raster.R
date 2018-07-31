#' Bin spatial points to a raster
#'
#' Bin spatial points to a raster. For each raster cell, the number of points are counted. Optionally, a factor variable can be specified by which the points are counts are split. Note that this function supports \code{sf} objects, but still uses sp-based methods (see details).
#'
#' This function is a wrapper around \code{\link[raster:rasterize]{rasterize}}.
#'
#' This function supports \code{\link[sf:sf]{sf}} objects, but still uses sp-based methods, from the packages sp, rgeos, and/or rgdal.
#'
#' @param shp shape object. a \code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}, a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}}, or an \code{\link[sf:sf]{sf}} object that can be coerced as such.
#' @param nrow number of raster rows. If \code{NA}, it is automatically determined by \code{N} and the aspect ratio of \code{shp}.
#' @param ncol number of raster columns. If \code{NA}, it is automatically determined by \code{N} and the aspect ratio of \code{shp}.
#' @param N preferred number of raster cells.
#' @param by name of a data variable which should be a factor. The points are split and counted according to the levels of this factor.
#' @param to.Raster not used anymore, since the output is always a \code{\link[raster:Raster-class]{raster}} as of version 2.0
#' @return a \code{RasterBrick} is returned when \code{by} is specified, and a \code{RasterLayer} when \code{by} is unspecified.
#' @export
#' @importFrom raster raster extent rasterize
#' @example  ./examples/points_to_raster.R
#' @seealso \code{\link{poly_to_raster}}
points_to_raster <- function(shp, nrow=NA, ncol=NA, N=250000, by=NULL, to.Raster=NULL) {
    if (!missing(to.Raster)) warning("to.Raster is not used anymore, since the output is always a raster object as of version 2.0")


    if (inherits(shp, c("sf", "sfc"))) shp <- as(shp, "Spatial")

	if (!inherits(shp, "SpatialPoints")) stop("shp should be a SpatialPoints/Pixels(DataFrame)")

	# get shp metadata
	bbx <- bb(shp)
	prj <- shp@proj4string
	np <- length(shp)
	asp <- get_asp_ratio(shp)
	hasData <- "data" %in% names(attributes(shp))

	# convert to points if necessary
	if (inherits(shp, "SpatialPixels")) {
		shp <- as(shp, ifelse(hasData, "SpatialPointsDataFrame", "SpatialPoints"))
	}

	# determine grid size
	if (is.na(nrow) || is.na(ncol)) {
		nrow <- round(sqrt(N/asp))
		ncol <- round(N / nrow)
	}
	N <- nrow * ncol

	# create empty raster
	r <- raster(extent(bbx), nrows=nrow, ncols=ncol, crs=prj)

	# process by variable
	if (missing(by)) {
		var <- factor(rep(1L, np), labels="count")
	} else {
		var <- shp[[by]]
		if (!is.factor(var)) stop("by variable is not a factor")
	}
	lvls <- make.names(levels(var))
	names(lvls) <- lvls
	levels(var) <- lvls

	shps <- split_shape(shp, f=var)

	res <- as.data.frame(lapply(shps, function(s) {
		if ("data" %in% names(attributes(s))) {
			s$ones <- 1
		} else s <- SpatialPointsDataFrame(s, data=data.frame(ones=rep(1, length(s))), match.ID=FALSE)
		rst <- suppressWarnings({ # may have warnings about deprecated embedding lists with S4
			rasterize(s, r, field="ones", fun='count')
		})
		rst@data@values
	}))
	rshp <- SpatialGridDataFrame(as(r, "SpatialGrid"), data=res)

	# return Raster object or SGDF
	if (ncol(rshp@data)==1) {
		as(rshp, "RasterLayer")
	} else {
		as(rshp, "RasterBrick")
	}
}

#' Convert spatial polygons to a raster
#'
#' Convert spatial polygons to a raster. The value of each raster cell will be the polygon ID number. Alternatively, if \code{copy.data}, the polygon data is appended to each raster cell.
#'
#' @param shp shape object. A \code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}, a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}}, or an \code{sf} object that can be coerced as such.
#' @param r \code{\link[raster:raster]{Raster}} object. If not specified, it will be created from the bounding box of \code{shp} and the arguments \code{N}, \code{nrow}, and \code{ncol}.
#' @param nrow number of raster rows. If \code{NA}, it is automatically determined by \code{N} and the aspect ratio of \code{shp}.
#' @param ncol number of raster columns. If \code{NA}, it is automatically determined by \code{N} and the aspect ratio of \code{shp}.
#' @param N preferred number of raster cells.
#' @param use.cover logical; should the cover method be used? This method determines per raster cell which polygon has the highest cover fraction. This method is better, but very slow, since N times the number of polygons combinations are processed (using the \code{getCover} argument of \code{\link[raster:rasterize]{rasterize}}). By default, when a raster cell is covered by multiple polygons, the last polygon is taken (see \code{fun} argment of \code{\link[raster:rasterize]{rasterize}}))
#' @param copy.data should the polygon data be appended to the raster? Only recommended when \code{N} is small.
#' @param to.Raster not used anymore, since the "raster" output is always a \code{\link[raster:Raster-class]{RasterLayer}} as of version 2.0
#' @param ... arguments passed on to \code{\link[raster:rasterize]{rasterize}}
#' @return a \code{RasterBrick} is returned when \code{by} is specified, and a \code{RasterLayer} when \code{by} is unspecified
#' @export
#' @importFrom raster raster extent rasterize getValues
#' @example  ./examples/poly_to_raster.R
#' @seealso \code{\link{points_to_raster}}
poly_to_raster <- function(shp, r=NULL, nrow=NA, ncol=NA, N=250000, use.cover=FALSE, copy.data=FALSE,  to.Raster=NULL, ...) {
    if (!missing(to.Raster)) warning("to.Raster is not used anymore, since the output is always a raster object as of version 2.0")

    if (inherits(shp, c("sf", "sfc"))) shp <- as(shp, "Spatial")

	if (!inherits(shp, "SpatialPolygons")) stop("shp should be a SpatialPolygons(DataFrame)")

    np <- length(shp)
    hasData <- "data" %in% names(attributes(shp))

    if (missing(r)) {
        # get shp metadata
        bbx <- bb(shp, output = "matrix")
        asp <- get_asp_ratio(shp)
        # determine grid size
        if (is.na(nrow) || is.na(ncol)) {
            nrow <- round(sqrt(N/asp))
            ncol <- round(N / nrow)
        }
        N <- nrow * ncol
        # create empty raster
        r <- raster(extent(bbx), nrows=nrow, ncols=ncol)
    }

	# add ID data variable
	if (!hasData) {
		shp <- SpatialPolygonsDataFrame(shp, data=data.frame(ID__UNITS = 1:np), match.ID=FALSE)
	} else {
		shp$ID__UNITS <- 1:np
	}

	# get shape data (including ID variable)
	d <- shp@data

	# create raster of ID values
	if (use.cover) {
		res <- do.call("cbind", lapply(1:np, function(i) {
			s <- shp[i, ]
			rst <- suppressWarnings({ # may have warnings about deprecated embedding lists with S4
				rasterize(s, r, field="ID__UNITS", getCover=TRUE, ...)
			})
			raster::getValues(rst)
		}))
		IDs <- apply(res, MARGIN=1, which.max)
	} else {
		rst <- suppressWarnings({ # may have warnings about deprecated embedding lists with S4
			rasterize(shp, r, field="ID__UNITS", getCover=FALSE, ...)
		})
		if (copy.data) IDs <- getValues(rst)
	}

	if (!copy.data) {
        return(rst)
	}


	# convert to SGDF and append data
	rshp <- as(rst, "SpatialGridDataFrame")
	rshp@data <- d[match(IDs, d$ID__UNITS),,drop=FALSE]

	# remove temp variable, or rename it to ID
	if (hasData) {
		rshp$ID__UNITS <- NULL
	} else {
		names(rshp) <- "ID"
	}

	# return Raster object or SGDF
	if (to.Raster) {
		if (ncol(rshp@data)==1) {
			as(rshp, "RasterLayer")
		} else {
			as(rshp, "RasterBrick")
		}
	} else rshp
}
