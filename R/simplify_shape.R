#' Simplify shape
#'
#' Simplify a shape consisting of polygons or lines. This can be useful for shapes that are too detailed for visualization, especially along natural borders such as coastlines and rivers. The number of coordinates is reduced.
#'
#' This function is a wrapper of \code{\link[rmapshaper:ms_simplify]{ms_simplify}}. In addition, the data is preserved. Also \code{sf} objects are supported.
#'
#' @param shp a \code{\link[sp:SpatialPolygons]{SpatialPolygons(DataFrame)}} or a \code{\link[sp:SpatialLines]{SpatialLines(DataFrame)}}, or an \code{sf} object that can be coerced to one of them.
#' @param fact simplification factor, number between 0 and 1 (default is 0.1)
#' @param keep.units d
#' @param keep.subunits d
#' @param ... other arguments passed on to the underlying function \code{\link[rmapshaper:ms_simplify]{ms_simplify}} (except for the arguments \code{input}, \code{keep}, \code{keep_shapes} and \code{explode})
#' @example ./examples/simplify_shape.R
#' @importFrom rmapshaper ms_simplify
#' @importFrom rgeos gIsValid gBuffer
#' @return shape in the smae class as \code{shp}
#' @export
simplify_shape <- function(shp, fact = 0.1, keep.units=FALSE, keep.subunits=FALSE, ...) {
    is_sf <- inherits(shp, c("sf", "sfc"))

    if (is_sf) shp <- as(shp, "Spatial")
    if (!inherits(shp, c("SpatialLines", "SpatialPolygons"))) stop("shp is not a SpatialPolygons or SpatialLines object")

    hasData <- "data" %in% names(attributes(shp))

    if (!hasData) {
        if (inherits(shp, "SpatialLines")) shp <- SpatialLinesDataFrame(shp, data.frame(UNIT__NR = 1L:length(shp)), match.ID = FALSE)
        if (inherits(shp, "SpatialPolygons")) shp <- SpatialPolygonsDataFrame(shp, data.frame(UNIT__NR = 1L:length(shp)), match.ID = FALSE)
    } else {
        # shape names are stored, because ms_simplify does not differentiate between upper- and lowercase
        dataNames <- names(shp)
        names(shp) <- paste(dataNames, 1L:length(dataNames), sep ="__")

        shp$UNIT__NR <- 1L:length(shp)
    }

    keep_shapes <- keep.units
    explode <- keep_shapes && keep.subunits
    x <- rmapshaper::ms_simplify(shp, keep=fact, keep_shapes=keep_shapes, explode=explode, ...)
    if (explode) x <- aggregate_map(x, by="UNIT__NR", agg.fun = first)

    if (hasData) {
        x@data[, c("rmapshaperid", "UNIT__NR")] <- list()
        names(x) <- dataNames
    } else {
        if (inherits(x, "SpatialLinesDataFrame")) x <- as(x, "SpatialLines")
        if (inherits(x, "SpatialPolygonsDataFrame")) x <- as(x, "SpatialPolygons")
    }
    x2 <- if (suppressWarnings(!rgeos::gIsValid(x))) {
        suppressWarnings(rgeos::gBuffer(x, byid = TRUE, width = 0))
    } else x
    if (is_sf) as(x2, "sf") else x2
}
