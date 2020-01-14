#' Approximate distances
#'
#' Approximate distances between two points or across the horizontal and vertical centerlines of a bounding box.
#'
#' @param x object that can be coerced to a bounding box with \code{\link{bb}}, or a pair of coordintes (vector of two). In the former case, the distance across the horizontal and vertical centerlines of the bounding box are approximated. In the latter case, \code{y} is also required; the distance between points \code{x} amd \code{y} is approximated.
#' @param y a pair of coordintes, vector of two. Only required when \code{x} is also a pair of coordintes.
#' @param projection projection code, needed in case \code{x} is a bounding box or when \code{x} and \code{y} are pairs of coordinates. See \code{\link{get_proj4}}
#' @param target target unit, one of:  \code{"m"}, \code{"km"}, \code{"mi"}, and \code{"ft"}.
#' @return If \code{y} is specifyed, a list of two: unit and dist. Else, a list of three: unit, hdist (horizontal distance) and vdist (vertical distance).
#' @importFrom units set_units
#' @example ./examples/approx_distances.R
#' @seealso \code{\link{approx_areas}}
#' @export
approx_distances <- function(x, y = NULL, projection = NULL, target = NULL) {
        ## set metric and imperial to defaults: km and mi
    if (!missing(target)) {
        is_metric <- target=="metric"
        is_imperial <- target=="imperial"

        if (is_metric) target <- "km"
        if (is_imperial) target <- "mi"
    }


    if (!inherits(x, c("sf", "Spatial", "raster"))) {
        if (missing(projection)) {
            projection <- st_crs(NA)
            #stop("Please specify projection")
        }
    } else {
        projection <- st_crs(x)
    }

    if (is.na(projection)) {
        warning("unknown projection", call. = FALSE)
    }


    if (missing(y)) {

        tryCatch({
            bbx <- bb(x)
        }, error = function(e) {
            stop("x cannot be coerced to a bounding box with bb", call. = FALSE)
        })

        pW <- st_sfc(st_point(c(bbx[1], (bbx[2]+bbx[4])/2)), crs=projection)
        pE <- st_sfc(st_point(c(bbx[3], (bbx[2]+bbx[4])/2)), crs=projection)
        pS <- st_sfc(st_point(c((bbx[1]+bbx[3])/2, bbx[2])), crs=projection)
        pN <- st_sfc(st_point(c((bbx[1]+bbx[3])/2, bbx[4])), crs=projection)

        if (missing(target)) {
            list(hdist = get_distance(pW, pE),       #st_distance(pW, pE)[1,1],
                 vdist = get_distance(pS, pN))       #st_distance(pS, pN)[1,1])
        } else {
            list(hdist = set_units(get_distance(pW, pE), target, mode = "standard"),  #st_distance(pW, pE)[1,1]
                 vdist = set_units(get_distance(pS, pN), target, mode = "standard"))  #st_distance(pS, pN)[1,1]
        }

    } else {
        p1 <- st_sfc(st_point(x), crs=projection)
        p2 <- st_sfc(st_point(y), crs=projection)

        if (missing(target) || is.na(projection)) {
            st_distance(p1, p2)[1,1]
        } else {
            set_units(st_distance(p1, p2), target, mode = "standard")[1,1]
        }

    }
}


get_distance <- function(p1, p2) {
    tryCatch(st_distance(p1, p2)[1,1], error = function(e) {
        p1ll <- lwgeom::st_transform_proj(p1, crs = 4326)
        p2ll <- lwgeom::st_transform_proj(p2, crs = 4326)
        lwgeom::st_geod_distance(p1ll, p2ll)[1,1]
    })
}
