#' Approximate distances
#'
#' Approximate distances between two points or across the horizontal and vertical centerlines of a bounding box.
#'
#' @param x object that can be coerced to a bounding box with \code{\link{bb}}, or a pair of coordintes (vector of two). In the former case, the distance across the horizontal and vertical centerlines of the bounding box are approximated. In the latter case, \code{y} is also required; the distance between points \code{x} amd \code{y} is approximated.
#' @param y a pair of coordintes, vector of two. Only required when \code{x} is also a pair of coordintes.
#' @param projection projection code, needed in case \code{x} is a bounding box or when \code{x} and \code{y} are pairs of coordinates. See \code{\link{get_proj4}}
#' @return list of two: the horizontal and vertical distances in meters
#' @importFrom geosphere distGeo
#' @example ./examples/approx_distances.R
#' @export
approx_distances <- function(x, y = NULL, projection = NULL) {
    if (inherits(x, c("Spatial", "Raster", "sf"))) {
        prj <- get_projection(x, as.CRS=FALSE, guess.longlat = TRUE)
        if (is.na(prj) && missing(projection)) stop("shape projection unknown; please specify it")
        if (!is.na(prj) && !missing(projection)) warning("projection already defined in shape")

        if (is.na(prj)) prj <- get_projection(projection, as.CRS = FALSE)
        bbx <- bb(x)
        if (!missing(y)) {
            warning("y is only used if x is a pair of coordinates")
            y <- NULL
        }
    } else {
        if (is.vector(x) && length(x)==2) {
            if (missing(y)) stop("y is required")
            if (!is.vector(y) || length(y)!=2) stop("y is not a vector of 2")
            bbx <- matrix(c(x, y), ncol=2, byrow = FALSE)
        } else {
            bbx <- bb(x)
            if (!missing(y)) {
                warning("y is only used if x is a pair of coordinates")
                y <- NULL
            }
        }
        if (missing(projection)) {
            if (maybe_longlat(bbx)) {
                prj <- get_proj4("longlat")
            } else {
                stop("projection unknown")
            }
        } else {
            prj <- get_proj4(projection, as.CRS = FALSE)
        }
    }

    isprj <- is_projected(prj)

    if (is.null(y)) {
        if (isprj) {
            to_meter <- get_shape_units(projection = prj)$to_meter

            vdist <- (bbx[4] - bbx[2]) * to_meter
            hdist <- (bbx[3] - bbx[1]) * to_meter
        } else {
            # also add middle values to prevent the shortest route is reverse
            h <- c(bbx[1], mean(c(bbx[1], bbx[3])), bbx[3])
            v <- c(bbx[2], mean(c(bbx[2], bbx[4])), bbx[4])
            hdist <- geosphere::distGeo(c(h[1], v[2]), c(h[2], v[2])) + geosphere::distGeo(c(h[2], v[2]), c(h[3], v[2]))
            vdist <- geosphere::distGeo(c(h[2], v[1]), c(h[2], v[2])) + geosphere::distGeo(c(h[2], v[2]), c(h[2], v[3]))
        }
        list(hdist=hdist,
             vdist=vdist)

    } else {
        if (isprj) {
            to_meter <- get_shape_units(projection = prj)$to_meter
            xd <- y[1] - x[1]
            yd <- y[2] - x[2]
            unname(sqrt(xd^2+yd^2) * to_meter)
        } else {
            geosphere::distGeo(x, y)
        }
    }
}
