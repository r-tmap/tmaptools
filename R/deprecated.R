#' Deprecated tmaptools functions
#'
#' The following functions are not used anymore or deprecated as of tmaptools version 3.0. These functions are based on the \code{sp} package and are not supported anymore. They have been migrated to \url{https://github.com/mtennekes/oldtmaptools}
#'
#' @param x see documentation in tmaptools 2.x for details
#' @param shp see documentation in tmaptools 2.x for details
#' @param output see documentation in tmaptools 2.x for details
#' @param projection see documentation in tmaptools 2.x for details
#' @param current.projection see documentation in tmaptools 2.x for details
#' @param overwrite.current.projection see documentation in tmaptools 2.x for details
#' @param guess.longlat see documentation in tmaptools 2.x for details
#'
#' Deprecated as of version 2.0
#' \itemize{
#'  \item \code{append_data}, \code{aggregate_map}, \code{double_line}, \code{points_to_raster}, \code{poly_to_raster}, \code{sample_dots}, \code{sbind}, \code{smooth_map}, \code{smooth_raster_cover}, \code{read_shape}, \code{write_shape}. These functions are based on the \code{sp} package and are not supported anymore. They have been migrated to \url{https://github.com/mtennekes/oldtmaptools}
#'  \item \code{osm_poly}, \code{osm_line}, \code{osm_point}. Please use the package \code{osmdata}
#' }
#'
#' Deprecated as of version 3.0
#' \itemize{
#'  \item \code{get_proj4}: for projections, please use the function \code{\link[sf:st_crs]{st_crs}}
#'  \item \code{set_projection} for setting map projections, use \code{\link[sf:st_crs]{st_crs}} or \code{\link[sf:st_transform]{st_transform}}
#'  \item \code{get_projection} for getting map projections, use \code{\link[sf:st_crs]{st_crs}}
#'  \item \code{is_projected} for checking if projections are long lat coordinates, use \code{\link[sf:st_is_longlat]{st_is_longlat}}
#' }
#'
#' @rdname deprecated_functions
#' @name deprecated_functions
NULL

#' @rdname deprecated_functions
#' @name get_proj4
#' @keywords internal
#' @export
get_proj4 <- function(x, output = c("crs", "character", "epsg", "CRS")) {
    if (sys.nframe()==1L) warning("get_proj4 is deprecated; for projections, please use the function st_crs from the sf package")

    output <- match.arg(output)
    y <- if (is.null(x)) {
        return(NULL)
    } else if (is.na(x)) {
        sf::st_crs()
    } else if (inherits(x, "crs")) {
        x
    } else if (inherits(x, "CRS")) {
        sf::st_crs(attr(x, "projargs"))
    } else if (!is.numeric(x) && !is.character(x)) {
        stop("x is not a character, crs object, CRS object, nor a number", call.=FALSE)
    } else {
        if (x %in% names(.proj_epsg)) {
            create_crs(unname(.proj_epsg[x]))
        } else if (x %in% names(.proj_sc)) {
            create_crs(unname(.proj_sc[x]))
        } else if (is_num_string(x)) {
            sf::st_crs(x)
        } else if (substr(x, 1, 3)=="utm") {
            if (!(nchar(x) %in% c(5,6))) stop("\"utm\" shortcut code should be utmXX or utmXXs where XX refers to the utm zone")
            sf::st_crs(paste("+proj=utm +zone=", substr(x, 4, 5), ifelse(substr(x, 6, 6)=="s", " +south", ""), " +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0", sep=""))
        } else {
            sf::st_crs(x)
        }
    }

    if (output == "CRS") stop("output CRS not supported anymore")

    switch(output,
           character = y$proj4string,
           crs = y,
           epsg = y$epsg)
}

create_crs <- function(x) {
    if (is.numeric(x)) {
        sf::st_crs(x)
    } else {
        structure(list(epsg = as.integer(NA), proj4string = x), class = "crs")
    }
}

is_num_string <- function(x) {
    suppressWarnings(!is.na(as.numeric(x)))
}

.proj_epsg <- c(longlat = 4326,
                latlong = 4326,
                WGS84 = 4326,
                NAD83 = 4269,
                NAD27 = 4267,
                merc = 3857,
                laea_Eur = 3035,
                laea_NA = 2163,
                rd = 28992)

.proj_sc <- c(wintri="+proj=wintri +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
              robin="+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
              eck4="+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
              hd="+proj=cea +lat_ts=37.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
              gall="+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
              mill="+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +R_A +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
              eqc0="+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
              eqc30="+proj=eqc +lat_ts=30 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
              eqc45="+proj=eqc +lat_ts=45 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")



#' @rdname deprecated_functions
#' @name set_projection
#' @keywords internal
#' @export
set_projection <- function(shp, projection=NA, current.projection=NA, overwrite.current.projection=FALSE) {
    if (sys.nframe()==1L) warning("set_projection is deprecated; for projections, please use the functions st_crs or st_transform from the sf package")

    if (inherits(shp, "Raster")) shp <- stars::st_as_stars(shp)

    if (!is.na(current.projection)) {
        if (!is.na(sf::st_crs(shp)) && !overwrite.current.projection) stop("Current projection already known. Use overwrite.current.projection = TRUE")
        sf::st_crs(shp) <- sf::st_crs(current.projection)
    }


    if (!is.na(projection)) {
        shp <- sf::st_transform(shp, crs = sf::st_crs(projection))
    }
    shp
}

#' @rdname deprecated_functions
#' @name get_projection
#' @keywords internal
#' @export
get_projection <- function(shp, guess.longlat=FALSE,
                           output = c("character", "crs", "epsg", "CRS")) {
    if (sys.nframe()==1L) warning("get_projection is deprecated; for projections, please use the function st_crs from the sf package")

    p <- if (inherits(shp, c("sf", "sfc", "stars"))) {
        sf::st_crs(shp)
    } else if (inherits(shp, "Spatial")) {
        sf::st_crs(attr(attr(shp, "proj4string"), "projargs"))
    } else if (inherits(shp, "Raster")) {
        sf::st_crs(attr(attr(shp, "crs"), "projargs"))
    } else {
        stop("shp is neither an sf, sp, raster, nor a stars object")
    }

    output <- match.arg(output)
    if (output == "CRS") stop("output CRS not supported anymore")
    switch(output,
           character = p$proj4string,
           crs = p,
           epsg = p$epsg)
}

#' @rdname deprecated_functions
#' @name is_projected
#' @keywords internal
#' @export
is_projected <- function(x) {
    if (sys.nframe()==1L) warning("is_projected is deprecated; for projections, please use the function st_is_longlat from the sf package")
    !sf::st_is_longlat(x)
}
