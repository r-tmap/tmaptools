#' Read GPX file
#'
#' Read a GPX file. By default, it reads all possible GPX layers, and only returns shapes for layers that have any features.
#'
#' Note that this function returns \code{\link[sf:sf]{sf}} objects, but still uses methods from sp and rgdal internally.
#'
#' @param file a GPX filename (including directory)
#' @param layers vector of GPX layers. Possible options are \code{"waypoints"}, \code{"tracks"}, \code{"routes"}, \code{"track_points"}, \code{"route_points"}. By dedault, all those layers are read.
#' @param remove.empty.layers should empty layers (i.e. with 0 features) be removed from the list?
#' @param as.sf not used anymore
#' @return a list of sf objects, one for each layer
#' @export
read_GPX <- function(file, layers=c("waypoints", "routes", "tracks", "route_points", "track_points"), remove.empty.layers = TRUE, as.sf = TRUE) {
	if (!all(layers %in% c("waypoints", "routes", "tracks", "route_points", "track_points"))) stop("Incorrect layer(s)", call. = FALSE)

    layers_data <- sf::st_layers(file)

    if (!all(layers %in% layers_data$name)) stop("layers not found in GPX file")

    res <- lapply(layers, function(l) {
        sf::st_read(file, layer = l, quiet = TRUE)
    })
    names(res) <- layers

    if (remove.empty.layers) {
        res <- res[layers_data$features[match(layers, layers_data$name)] > 0]
    }

	res
}
