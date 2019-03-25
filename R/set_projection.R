#' Set and get the map projection
#'
#' The function \code{set_projection} sets the projection of a shape file. It is
#' a convenient wrapper of \code{\link[sf:st_transform]{st_transform}} (or \code{\link[lwgeom:st_transform_proj]{st_transform_proj}}, see details) and
#' \code{\link[raster:projectRaster]{projectRaster}} with shortcuts for commonly
#' used projections. The projection can also be set directly in the plot call
#' with \code{\link[tmap:tm_shape]{tm_shape}}. This function is also used to set the current
#' projection information if this is missing. The function \code{get_projection}
#' is used to get the projection information.
#'
#' For \code{\link[sf:sf]{sf}} objects, \code{set_projection} first tries to use \code{\link[sf:st_transform]{sf::st_transform}}, which uses the GDAL API. For some projections, most notably Winkel Tripel (\code{"wintri"}), is doesn't work. In these cases, \code{set_projection} will use \code{\link[lwgeom:st_transform_proj]{lwgeom::st_transform_proj}}, which uses the PROJ.4 API.
#'
#' For raster objects, the projection method is based on the type of data. For numeric layers, the bilinear method is used, and for categorical layers the nearest neighbor. See \code{\link[raster:projectRaster]{projectRaster}} for details.
#'
#' @param shp shape object, which is an object from a class defined by the \code{\link[sf:sf]{sf}}, \code{\link[sp:sp]{sp}}, or \code{\link[raster:raster-package]{raster}} package.
#' @param projection new projection. See \code{\link{get_proj4}} for options. This argument is only used to transform the \code{shp}. Use \code{current.projection} to specify the current projection of \code{shp}.
#' @param current.projection the current projection of \code{shp}. See \code{\link{get_proj4}} for possible options. Only use this if the current projection is missing or wrong.
#' @param overwrite.current.projection logical that determines whether the current projection is overwritten if it already has a projection that is different.
#' @param output output format of the projection. One of \code{"character"}, \code{"crs"} (from \code{sf} package), \code{"epsg"} or \code{"CRS"} (from \code{sp}/\code{rgdal} package)
#' @param guess.longlat if \code{TRUE}, it checks if the coordinates are within -180/180 and -90/90, and if so, it returns the WGS84 longlat projection (which is \code{get_proj4("longlat")}).
#' @name set_projection
#' @rdname set_projection
#' @import sp
#' @importFrom raster projectRaster
#' @importFrom rgdal getPROJ4VersionInfo
#' @return \code{set_projection} returns a (transformed) shape object with updated projection information. \code{get_projection} returns the \code{PROJ.4} character string of \code{shp}.
#' @export
set_projection <- function(shp, projection=NA, current.projection=NA, overwrite.current.projection=FALSE) {
	shp.name <- deparse(substitute(shp))

	cls <- class(shp)
	is_sp <- inherits(shp, "Spatial")
	is_sp_raster <- inherits(shp, c("SpatialGrid", "SpatialPixels"))
	if (is_sp) shp <- (if (is_sp_raster) brick(shp) else as(shp, "sf"))

	shp.crs <- get_projection(shp, output="crs")
	current.crs <- get_proj4(current.projection, output = "crs")
	proj.crs <- get_proj4(projection, output = "crs")

	if (is.na(shp.crs)) {
		if (is.na(current.crs)) {
			stop("Currect projection of shape object unknown. Please specify the argument current.projection. The value \"longlat\", which stands for Longitude-latitude (WGS84), is most commonly used.")
		} else {
			if (inherits(shp, "sf")) {
				st_crs(shp) <- current.crs
			} else {
				shp@crs <- get_proj4(current.crs, output = "CRS")
			}
			#current.projection <- current.proj4
		}
	} else {
		if (!is.na(current.crs)) {
			if (identical(current.crs, shp.crs)) {
				warning("Current projection of ", shp.name, " already known.", call. = FALSE)
			} else {
				if (overwrite.current.projection) {
					warning("Current projection of ", shp.name, " differs from ", current.crs$proj4string, ", but is overwritten.", call. = FALSE)
					if (inherits(shp, "sf")) {
					    st_crs(shp) <- current.crs
					} else {
						shp@crs <- get_proj4(current.crs, output = "CRS")
					}

				} else {
					stop(shp.name, " already has projection: ", shp.crs$proj4string, ". This is different from the specified current projection ", current.crs$proj4string, ". If the specified projection is correct, use overwrite.current.projection=TRUE.", call. = FALSE)
				}
			}
		} else {
			current.crs <- shp.crs
		}
	}


	if (!is.na(proj.crs)) {
		PROJ4_version_nr <- get_proj4_version()

		if (length(grep("+proj=wintri", current.crs$proj4string, fixed = TRUE)) && PROJ4_version_nr < 491) {
			stop("Unable to reproject a shape from the Winkel Tripel projection with PROJ.4 version < 4.9.1")
		}


		if (inherits(shp, "Raster")) {
		    proj.CRS <- get_proj4(proj.crs, output = "CRS")

			#raster_data <- get_raster_data(shp)
			has_color_table <- (length(colortable(shp))>0)

			# get factor levels (to be restored later)
			lvls <- get_raster_levels(shp, 1:nlayers(shp))
			# override factor levels with colortable values
			if (has_color_table) {
				lvls <- list(colortable(shp))
#				raster_data <- data.frame(PIXEL__COLOR=getValues(shp[[1]])+1L)
			}
			isnum <- sapply(lvls, is.null)
			new_ext <- suppressWarnings(projectExtent(shp, crs = proj.CRS))
			if (any(isnum) && !all(isnum)) {
				shp_num <- raster::subset(shp, subset=which(isnum))
				shp_cat <- raster::subset(shp, subset=which(!isnum))
				shp_num2 <- suppressWarnings(projectRaster(shp_num, to=new_ext, crs=proj.CRS, method="bilinear"))
				shp_cat2 <- suppressWarnings(projectRaster(shp_cat, to=new_ext, crs=proj.CRS, method="ngb"))

				# restore order
				o <- order(c(which(isnum), which(!isnum)))
				rlayers <- c(lapply(1:nlayers(shp_num), function(i) raster(shp_num2, layer=i)),
							 lapply(1:nlayers(shp_cat), function(i) raster(shp_cat2, layer=i)))[o]
				shp <- do.call("brick", rlayers)
			} else if (all(isnum)) {
				shp <- suppressWarnings(projectRaster(shp, to=new_ext, crs=proj.CRS, method="bilinear"))
			} else {
				shp <- suppressWarnings(projectRaster(shp, to=new_ext, crs=proj.CRS, method="ngb"))
			}

			# new_raster_data <- as.data.frame(mapply(function(d, l) {
			# 	if (!is.null(l) && !is.factor(d)) factor(d, levels=1:length(l), labels=l) else d
			# }, get_raster_data(shp), lvls, SIMPLIFY=FALSE))

			if (any(!isnum)) shp <- set_raster_levels(shp, lvls)
			# shp@data@isfactor <- !isnum
			# dfs <- mapply(function(nm, lv) {
			# 	df <- data.frame(ID=1:length(lv), levels=factor(lv, levels=lv))
			# 	if (cls=="RasterBrick") names(df)[2] <- nm
			# 	df
			# }, names(which(!isnum)), lvls[!isnum], SIMPLIFY=FALSE)
			# shp@data@attributes <- dfs
		} else {
			shp <- st_transform2(shp, proj.crs)
		}
		if (is_sp_raster) {
			shp <- as(shp, cls)
			names(shp) <- names(isnum)
		}
	}

	shp

	#if (is_sp && !is_sp_raster) as(shp, cls) else shp
}

st_transform2 <- function(x, crs, ...) {
    args <- list(...)
    y <- tryCatch(do.call(sf::st_transform, c(list(x=x, crs=crs), args)),
             error = function(e) NULL,
             warning = function(w) NULL)
    if (is.null(y)) {
        y <- tryCatch(do.call(lwgeom::st_transform_proj, c(list(x=x, crs=crs), args)),
                 error = function(e) {
                      stop("Unable to set the projection to ", crs$proj4string, ".", call. = FALSE)
                 } )
    }
    y
}






#' @name get_projection
#' @rdname set_projection
#' @export
get_projection <- function(shp, guess.longlat=FALSE,
                           output = c("character", "crs", "epsg", "CRS")) {
    p <- if (inherits(shp, c("sf", "sfc"))) {
        st_crs(shp)
    } else if (inherits(shp, "Spatial")) {
        st_crs(attr(attr(shp, "proj4string"), "projargs"))
    } else if (inherits(shp, "Raster")) {
        st_crs(attr(attr(shp, "crs"), "projargs"))
    } else {
        stop("shp is neither a sf, sp, nor a raster object")
    }

    output <- match.arg(output)

    switch(output,
           character = p$proj4string,
           crs = p,
           epsg = p$epsg,
           CRS = CRS(ifelse(is.na(p$proj4string), "", p$proj4string))
    )
}


