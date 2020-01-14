st_transform2 <- function(x, crs, ...) {
    args <- list(...)
    y <- tryCatch(do.call(sf::st_transform, c(list(x=x, crs=crs), args)),
             error = function(e) NULL,
             warning = function(w) NULL)
    if (is.null(y)) {
        y <- tryCatch(do.call(lwgeom::st_transform_proj, c(list(x=x, crs=crs), args)),
                 error = function(e) {
                      stop("Unable to set the projection to ", crs$proj4string, ".", call. = FALSE)
                 } )
    }
    y
}
