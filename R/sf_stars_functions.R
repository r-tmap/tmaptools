to_sf_stars <- function(x) {
    if (inherits(x, c("sf", "sfc", "stars"))) {
        x
    } else if (inherits(x, c("Raster", "SpatialGrid", "SpatialPixels"))) {
        stars::st_as_stars(x)
    } else if (inherits(x, "sp")) {
        as(x, "sf")
    } else {
        stop("unknown spatial object", call. = FALSE)
    }
}
