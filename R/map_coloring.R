#' Map coloring
#'
#' Color the polygons of a map such that adjacent polygons have different colors. This function returns the color indices
#'
#' As of tmaptools 3.3, the deprecated color functions \code{get_brewer_pal} and \code{palette_explorer}, have been removed. These have been replaced \code{c4a} and \code{c4a_gui} respectively from the package cols4all. Therefore, \code{map_coloring} will return color indices and will ignore the input arguments \code{palette} and \code{contrast}. See example.
#'
#' @param x Either a shape (i.e. a \code{\link[sf:sf]{sf}} or \code{SpatialPolygons(DataFrame)} (\code{sp} package) object), or an adjacency list.
#' @param algorithm currently, only "greedy" is implemented.
#' @param ncols number of colors. By default it is 8 when \code{palette} is undefined. Else, it is set to the length of \code{palette}
#' @param minimize logical that determines whether \code{algorithm} will search for a minimal number of colors. If \code{FALSE}, the \code{ncols} colors will be picked by a random procedure.
#' @param ... to catch deprecated arguments \code{palette} and \code{contrast}. See details.
#' @return A vector of color indices.
#' @example ./examples/map_coloring.R
#' @import RColorBrewer
#' @export
map_coloring <- function(x, algorithm="greedy", ncols=NA, minimize=FALSE, ...) {
    if (inherits(x, "Spatial")) x <- as(x, "sf")
	if (inherits(x, "sf")) {
		# get adjacency list
		adj <- get_neighbours(x)
	} else if (is.list(x)) {
		adj <- x
	} else stop("Unknown x argument")

    args= list(...)
    if (any(c("palette", "contrast") %in% names(args))) warning("palette and contrast are not used anymore as of tmaptools 3.3. Please use cols4all: see manual")

	if (is.na(ncols)) ncols <- 8

	k <- length(adj)

	if (algorithm=="greedy") {
		# list of occupied colors
		occ <- as.list(rep(0, k))

		# vector of output colors
		cols <- rep(NA, k)

		# order of degree (starting with the highest)
		ord <- order(sapply(adj, length), decreasing = TRUE)

		showWarn <- FALSE
		for (i in ord) {
			sel <- setdiff(1:ncols, occ[[i]])

			if (!length(sel)) {
				sel <- 1
				showWarn <- TRUE
			}
			z <- if (minimize) sel[1] else sample(sel, 1)

			for (j in ord) if (i %in% adj[[j]]) occ[[j]] <- c(occ[[j]], z)
			cols[i] <- z
		}
	} else stop("Unknown algorithm")

	if (showWarn) warning("Unable to color with ", ncols, " colors. Adjacent polygons may have the same color.", call. = FALSE)

	if (!is.null(palette)) {
		palette2[cols]
	} else {
		cols
	}
}

#' Get neighbours list from spatial objects
#'
#' Get neighbours list from spatial objects. The output is similar to the function \code{poly2nb} of the \code{spdep} package, but uses \code{sf} instead of \code{sp}.
#'
#' @param x a shape object, i.e., a \code{\link[sf:sf]{sf}} object or a \code{SpatialPolygons(DataFrame)} (\code{sp} package).
#' @return A list where the items correspond to the features. Each item is a vector of neighbours.
#' @export
get_neighbours <- function(x) {
    y <- sf::st_intersects(x, x)
    n <- length(y)

    mapply(function(yi, i) {
        setdiff(yi, i)
    }, y, 1L:n, SIMPLIFY = FALSE)

}
