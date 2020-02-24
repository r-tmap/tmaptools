#' Simplify shape
#'
#' Simplify a shape consisting of polygons or lines. This can be useful for shapes that are too detailed for visualization, especially along natural borders such as coastlines and rivers. The number of coordinates is reduced.
#'
#' This function is a wrapper of \code{\link[rmapshaper:ms_simplify]{ms_simplify}}. In addition, the data is preserved. Also \code{\link[sf:sf]{sf}} objects are supported.
#'
#' @param shp an \code{\link[sf:sf]{sf}} object.
#' @param fact simplification factor, number between 0 and 1 (default is 0.1)
#' @param keep.units d
#' @param keep.subunits d
#' @param ... other arguments passed on to the underlying function \code{\link[rmapshaper:ms_simplify]{ms_simplify}} (except for the arguments \code{input}, \code{keep}, \code{keep_shapes} and \code{explode})
#' @example ./examples/simplify_shape.R
#' @return \code{\link[sf:sf]{sf}} object
#' @export
simplify_shape <- function(shp, fact = 0.1, keep.units=FALSE, keep.subunits=FALSE, ...) {
    if (!requireNamespace("rmapshaper", quietly = TRUE)) {
        stop("rmapshaper package is needed for simplify_shape", call. = FALSE)
    } else {
        sfcol <- attr(shp, "sf_column")

        dataNames <- setdiff(names(shp), sfcol)

        dataNames_new <- paste(dataNames, 1L:length(dataNames), sep ="__")

        names(shp)[match(dataNames, names(shp))] <- dataNames_new

        unitCols <- which(sapply(shp, inherits, "units"))
        if (length(unitCols) > 0) {
            units <- lapply(sf::st_drop_geometry(shp[, unitCols]), attr, "units")
            shp[, unitCols] <- lapply(sf::st_drop_geometry(shp[, unitCols]), as.numeric)
        }

        shp$UNIT__NR <- 1L:nrow(shp)

        keep_shapes <- keep.units
        explode <- keep_shapes && keep.subunits
        x <- suppressWarnings(rmapshaper::ms_simplify(shp, keep=fact, keep_shapes=keep_shapes, explode=explode, ...))

        if (explode) x <- stats::aggregate(x, by = list(x$UNIT__NR), FUN = function(x)x[1])

        x[, c("rmapshaperid", "UNIT__NR")] <- list()
        names(x)[match(dataNames_new, names(x))] <- dataNames

        if (length(unitCols) > 0) {
            shp[, unitCols] <- mapply(units::as_units, sf::st_drop_geometry(shp[, unitCols]), units, SIMPLIFY = FALSE)
        }

        if (!all(sf::st_is_valid(x))) {
            if (!requireNamespace("lwgeom", quietly = TRUE)) {
                stop("simplified shape is not valid and needs to be fixed; please install the lwgeom package and rerun this function", call. = FALSE)
            }
            sf::st_make_valid(x)
        } else {
            x
        }
    }
}
