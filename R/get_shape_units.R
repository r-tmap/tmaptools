#' Get shape units
#'
#' Get the units in which the shape coordinates are specified, and determine the multiplier to transpose it in meters or other units. For unprojected shapes, the conversion of coordinates to meters hold for a specific latitude.
#'
#' @importFrom geosphere distGeo
#' @import sp
#' @export
#' @param x a shape object from class Spatial, Raster or sf
#' @param projection projection string
#' @param latitude latitude, in case \code{x} is unprojected
#' @param target.unit target unit, one of "km", "m", "mi", and "ft"
#' @return list with the following items, \code{unit} according to the coordinates/projection and \code{to_meter} which is the multiplier to get meters. If \code{target.unit} is specified, the list also contains \code{target} and \code{to_target}, which are respectively the target.unit and the multipler to get these units.
#' @seealso  \code{\link{approx_distances}} for distances between two points
#' @example ./examples/get_shape_units.R
get_shape_units <- function(x=NULL, projection=NULL, latitude=NULL, target.unit=NULL) {
    if (!missing(x)) {
        if (inherits(x, c("Spatial", "Raster", "sf"))) {
            if (!missing(projection)) warning("projection already defined in shape")
            projection <- get_projection(x, as.CRS=FALSE)
            if (missing(latitude)) {
                latitude <- mean(bb(x)[c(2,4)])
            }
        } else {
            stop("x is neither a Spatial, Raster nor sf object")
        }
        bbox <- bb(x)
    } else {
        if (missing(projection)) stop("Please specify x or projection")
        projection <- get_proj4(projection)
    }

    isprj <- (length(grep("longlat", projection, fixed=TRUE))==0)

    if (isprj) {
        pat <- '^.*\\+units ?= ?(\\S*)(.*)$'
        unit <- sub(pat, '\\1', projection[grepl(pat,projection)])
        to_meter <- to_m[unit]

        if (length(unit)==0) {
            unit <- NA
            to_meter <- NA
        }
    } else {
        if (missing(latitude)) stop("latitude is required")
        longs <- c(0, 1) # take arbitrary longitudes
        unit <- paste("long@lat", round(latitude), sep="")
        to_meter <- geosphere::distGeo(c(0, latitude), c(1, latitude))
    }

    res <- list(unit=unit, to_meter=unname(to_meter))

    if (!missing(target.unit)) {
        if (target.unit=="metric") target.unit <- "km"
        if (target.unit=="imperial") target.unit <- "mi"

        to_target <- unname(to_meter / convert_shape_units("m", target.unit)$to)
        res$target <- target.unit
        res$to_target <- to_target
    }

    res
}
