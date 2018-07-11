#' Set and get the map projection
#'
#' The function \code{set_projection} sets the projection of a shape file. It is
#' a convenient wrapper of \code{\link[sp:spTransform]{spTransform}} and
#' \code{\link[raster:projectRaster]{projectRaster}} with shortcuts for commonly
#' used projections. The projection can also be set directly in the plot call
#' with \code{\link[tmap:tm_shape]{tm_shape}}. This function is also used to set the current
#' projection information if this is missing. The function \code{get_projection}
#' is used to get the projection information.
#'
#' For raster objects, the projection method is based on the type of data. For numeric layers, the bilinear method is used, and for categorical layers the nearest neighbor. See \code{\link[raster:projectRaster]{projectRaster}} for details.
#'
#' @param shp shape object of class \code{\link[sp:Spatial]{Spatial}},
#'   \code{\link[raster:Raster-class]{Raster}}, or \code{sf} (see details).
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
			shp <- tryCatch({
			    lwgeom::st_transform_proj(shp, proj.crs)
			}, error=function(e) {
				stop("Unable to set the projection to ", proj.crs$proj4string, ".", call.=FALSE)
			}, warning=function(w){
				NULL
			})
		}
		if (is_sp_raster) {
			shp <- as(shp, cls)
			names(shp) <- names(isnum)
		}
	}

	shp

	#if (is_sp && !is_sp_raster) as(shp, cls) else shp
}



set_raster_levels <- function(shp, lvls) {
	isf <- !vapply(lvls, is.null, logical(1))
	cls <- class(shp)
	if (any(isf)) {
		shp@data@isfactor <- isf
		dfs <- mapply(function(nm, lv) {
			df <- data.frame(ID=1:length(lv), levels=factor(lv, levels=lv))
			if (cls=="RasterBrick") names(df)[2] <- nm
			df
		}, names(which(isf)), lvls[isf], SIMPLIFY=FALSE)
		shp@data@attributes <- dfs
	}
	shp
}

get_RasterLayer_levels <- function(r) {
	if (r@data@isfactor) {
		dt <- r@data@attributes[[1]]
		levelsID <- ncol(dt)
		as.character(dt[[levelsID]])
	} else {
		NULL
	}
}

get_raster_names <- function(shp) {
    nms <- names(shp)

    # overwrite unknown first names with FILE__VALUES
    if (inherits(shp, "RasterStack")) {
        if (shp@layers[[1]]@data@names[1]=="") nms[1] <- "FILE__VALUES"
    } else {
        if (shp@data@names[1]=="") nms[1] <- "FILE__VALUES"
    }
    nms
}

get_raster_levels <- function(shp, layerIDs) {
	if (missing(layerIDs)) layerIDs <- 1L:nlayers(shp)

	if (inherits(shp, "Spatial")) {
		return(lapply(attr(shp, "data")[,layerIDs], levels))
	}

	shpnames <- get_raster_names(shp)[layerIDs]
	if (inherits(shp, "RasterLayer")) {
		lvls <- list(get_RasterLayer_levels(shp))
	} else if (inherits(shp, "RasterStack")) {
		lvls <- lapply(shp@layers[layerIDs], get_RasterLayer_levels)
	} else if (inherits(shp, "RasterBrick")) {
		isfactor <- shp@data@isfactor
		if (all(!isfactor)) {
			lvls <- lapply(shpnames, function(sn) NULL)
		} else {
			atb <- shp@data@attributes
			atb <- atb[vapply(atb, length, integer(1))!=0]
			stopifnot(sum(isfactor)==length(atb))
			isfactor2 <- isfactor[layerIDs]

			lvls <- rep(list(NULL), length(layerIDs))
			if (any(isfactor2)) {
				atb2 <- atb[match(layerIDs[isfactor2], which(isfactor))]

				lvls[isfactor2] <- lapply(atb2, function(a) {
					if (class(a)=="list") a <- a[[1]]
					levelsID <- ncol(a) # levels is always the last column of the attributes data.frame (?)
					as.character(a[[levelsID]])
				})
			}
		}
	}
	names(lvls) <- shpnames
	lvls
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


