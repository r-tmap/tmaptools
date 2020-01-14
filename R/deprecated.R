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
    warning("get_proj4 is deprecated; for projections, please use the function st_crs from the sf package")
    output <- match.arg(output)
    crs <- sf::st_crs(x)

    if (output == "crs") {
        crs
    } else if (output == "character") {
        crs$proj4string
    } else if (output == "epsg") {
        crs$epsg
    } else if (output == "CRS") {
        stop("output CRS not supported anymore")
    } else {
        stop("Unknown output")
    }
}


#' @rdname deprecated_functions
#' @name set_projection
#' @keywords internal
#' @export
set_projection <- function(shp, projection=NA, current.projection=NA, overwrite.current.projection=FALSE) {
    warning("set_projection is deprecated; for projections, please use the functions st_crs or st_transform from the sf package")
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
    warning("get_projection is deprecated; for projections, please use the function st_crs from the sf package")

    p <- if (inherits(shp, c("sf", "sfc", "stars"))) {
        st_crs(shp)
    } else if (inherits(shp, "Spatial")) {
        st_crs(attr(attr(shp, "proj4string"), "projargs"))
    } else if (inherits(shp, "Raster")) {
        st_crs(attr(attr(shp, "crs"), "projargs"))
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
    warning("is_projected is deprecated; for projections, please use the function st_is_longlat from the sf package")
    !sf::st_is_longlat(x)
}
