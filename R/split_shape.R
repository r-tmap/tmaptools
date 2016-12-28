# similar to sp::split, but works different for SpatialPoints
split_shape <- function(x, f, drop=TRUE, ...) {
    if (!is.factor(f)) {
        warning("f is not a factor", call. = FALSE)
        f <- as.factor(f)
    }
    lev <- if (drop) {
        intersect(levels(f), f)
    } else levels(f)
    xlist <- lapply(lev, function(l) {
        ids <- which(f==l)
        if (length(ids)==0L) NULL else x[ids,]
    })
    names(xlist) <- lev
    xlist
}
