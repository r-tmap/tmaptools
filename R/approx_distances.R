#' Approximate distances
#'
#' Approximate distances between two points or across the horizontal and vertical center lines of a bounding box.
#'
#' @param x object that can be coerced to a bounding box with \code{\link{bb}}, or a pair of coordinates (vector of two). In the former case, the distance across the horizontal and vertical centerlines of the bounding box are approximated. In the latter case, \code{y} is also required; the distance between points \code{x} and \code{y} is approximated.
#' @param y a pair of coordintes, vector of two. Only required when \code{x} is also a pair of coordintes.
#' @param projection projection code, needed in case \code{x} is a bounding box or when \code{x} and \code{y} are pairs of coordinates. See \code{\link{get_proj4}}
#' @param target target unit, one of:  \code{"m"}, \code{"km"}, \code{"mi"}, and \code{"ft"}.
#' @param orig original unit, i.e. by which \code{x} is defined. Only needed if this information is missing from \code{x} and \code{x} is projected. Options:  \code{"m"}, \code{"km"}, \code{"mi"}, and \code{"ft"}.
#' @param to multiplier used as follows: \code{orig * to = target}. Only needed when \code{orig} or \code{target} is unknown. For instance, if \code{target} is set to \code{"hm"} (hectometer), and \code{orig} is \code{"m"}, then \code{to} should be 100, meaning 1 hectameter equals 100 meters.
#' @param show.warnings should warnings be shown?
#' @return If \code{y} is specifyed, a list of two: unit and dist. Else, a list of three: unit, hdist (horizontal distance) and vdist (vertical distance).
#' @importFrom geosphere distGeo
#' @example ./examples/approx_distances.R
#' @seealso \code{\link{projection_units}} and \code{\link{approx_areas}}
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \href{https://doi.org/10.18637/jss.v084.i06}{DOI}
#' @export
approx_distances <- function(x, y = NULL, projection = NULL, target="metric", orig=NA, to=NA, show.warnings=TRUE) {
    ## set metric and imperial to defaults: km and mi
    is_metric <- target=="metric"
    is_imperial <- target=="imperial"

    if (is_metric) target <- "km"
    if (is_imperial) target <- "mi"

    if (inherits(x, c("Spatial", "Raster", "sf", "sfc"))) {
        ## get projection and bounding box for spatial objects
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
        ## get projection and bounding box for points and bounding boxes. Guess projection
        if (is.vector(x) && length(x)==2) {
            if (missing(y)) stop("y is required")
            if (!is.vector(y) || length(y)!=2) stop("y is not a vector of 2")
            bbx <- matrix(c(x, y), ncol=2, byrow = FALSE)
        } else {
            bbx <- bb(x)
            if (!missing(y)) {
                if (show.warnings) warning("y is only used if x is a pair of coordinates")
                y <- NULL
            }
        }
        if (missing(projection)) {
            if (maybe_longlat(bbx)) {
                prj <- get_proj4("longlat")
            } else {
                if (show.warnings) warning("projection unknown")
                prj <- NA
            }
        } else {
            prj <- get_proj4(projection, as.CRS = FALSE)
        }
    }

    ## Get projection info
    res <- projection_units(x=prj, target = target, orig=orig, to=to)
    projected <- res$projected
    target <- res$target
    orig <- res$orig
    to <- res$to

    ## For non-projected case, units of coordinates will be meters (distGeo)
    if (!projected) {
        orig <- "m"
        to <- to_m["m"] / to_m[target]
    } else if (is.na(to)) {
        if (show.warnings) warning("Target unit or original unit unknown. Please specify valid the arguments target and orig, or the argument to")
        target <- "abs"
        to <- 1
        is_metric <- FALSE
        is_imperial <- FALSE
    }


    if (is.null(y)) {
        if (projected) {
            vdist <- (bbx[4] - bbx[2]) * to
            hdist <- (bbx[3] - bbx[1]) * to
        } else {
            # also add middle values to prevent the shortest route is reverse
            h <- c(bbx[1], mean(c(bbx[1], bbx[3])), bbx[3])
            v <- c(bbx[2], mean(c(bbx[2], bbx[4])), bbx[4])
            hdist <- (geosphere::distGeo(c(h[1], v[2]), c(h[2], v[2])) + geosphere::distGeo(c(h[2], v[2]), c(h[3], v[2]))) * to
            vdist <- (geosphere::distGeo(c(h[2], v[1]), c(h[2], v[2])) + geosphere::distGeo(c(h[2], v[2]), c(h[2], v[3]))) * to
        }
        if (is_metric) {
            if (hdist < 1 || vdist < 1) {
                hdist <- hdist * 1000
                vdist <- vdist * 1000
                target <- "m"
            }
        } else if (is_imperial) {
            if (hdist < 1 || vdist < 1) {
                hdist <- hdist * 5280
                vdist <- vdist * 5280
                target <- "ft"
            }
        }

        list(unit=target,
             hdist=hdist,
             vdist=vdist)

    } else {
        if (projected) {
            #to_meter <- get_shape_units(projection = prj)$to_meter
            xd <- y[1] - x[1]
            yd <- y[2] - x[2]
            dist <- unname(sqrt(xd^2+yd^2)) * to
        } else {
            dist <- geosphere::distGeo(x, y) * to
        }

        if (is_metric) {
            if (dist < 1) {
                dist <- dist * 1000
                target <- "m"
            }
        } else if (is_imperial) {
            if (dist < 1) {
                dist <- dist * 5280
                target <- "ft"
            }
        }

        list(unit=target,
             dist=dist)
    }
}
