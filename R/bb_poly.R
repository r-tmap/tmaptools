#' Convert bounding box to a spatial polygon
#'
#' Convert bounding box to a spatial (\code{\link[sf:st_sfc]{sfc}}) object . Useful for plotting (see example). The function \code{bb_earth} returns a spatial polygon of the 'boundaries' of the earth, which can also be done in other projections (if a feasible solution exists).
#'
#' @param x object that can be coerced to a bounding box with \code{\link{bb}}
#' @param projection projection in which the coordinates of \code{x} are provided. For \code{bb_earth}, \code{projection} is the projection in which the bounding box is returned (if possible).
#' @param steps number of intermediate points along the shortest edge of the bounding box. The number of intermediate points along the longest edge scales with the aspect ratio. These intermediate points are needed if the bounding box is plotted in another projection.
#' @param stepsize stepsize in terms of coordinates (usually meters when the shape is projected and degrees of longlat coordinates are used). If specified, it overrules \code{steps}
#' @param earth.datum Geodetic datum to determine the earth boundary. By default EPSG 4326.
#' @param bbx boundig box of the earth in a vector of 4 values: min longitude, max longitude, min latitude, max latitude. By default \code{c(-180, 180, -90, 90)}. If for some \code{projection}, a feasible solution does not exist, it may be wise to choose a smaller bbx, e.g. \code{c(-180, 180, -88, 88)}. However, this is also automatically done with the next argument, \code{buffer}.
#' @param buffer In order to determine feasible earth bounding boxes in other projections, a buffer is used to decrease the bounding box by a small margin (default \code{1e-06}). This value is subtracted from each the bounding box coordinates. If it still does not result in a feasible bounding box, this procedure is repeated 5 times, where each time the buffer is multiplied by 10. Set \code{buffer=0} to disable this procedure.
#' @return \code{\link[sf:st_sfc]{sfc}} object
#' @example ./examples/bb_poly.R
#' @name bb_poly
#' @rdname bb_poly
#' @export
bb_poly <- function(x, steps=100, stepsize=NA, projection=NULL) {
    bbx <- get_bb(x)$b
    create_sf_rect(bbx, steps=steps, stepsize=stepsize, projection=projection)
}

create_sf_rect <- function(bbx, steps=100, stepsize=NA, projection=NULL) {
    if (is.null(projection)) projection <- st_crs(bbx)

    x1 <- bbx[1]
    x2 <- bbx[3]
    y1 <- bbx[2]
    y2 <- bbx[4]

    dx <- x2-x1
    dy <- y2-y1

    if (is.na(stepsize)) stepsize <- min(dx,dy) / steps

    ny <- round(dy / stepsize + 1)
    nx <- round(dx / stepsize + 1)

    crds <- matrix(c(
        rep(x1, ny),
        seq(x1+stepsize, x2-stepsize, length.out=nx-2),
        rep(x2, ny),
        seq(x2-stepsize, x1+stepsize, length.out=nx-2),
        seq(y1, y2, length.out=ny),
        rep(y2, nx-2),
        seq(y2, y1, length.out=ny),
        rep(y1, nx-2)),
        ncol=2)

    #x <- SpatialPolygons(list(Polygons(list(Polygon(coords=crds)), ID=id)), proj4string=get_proj4(projection, as.CRS = TRUE))

    sf::st_sfc(sf::st_polygon(x=list(rbind(crds, crds[1,]))), crs=projection)
}




#' @name bb_earth
#' @rdname bb_poly
#' @export
bb_earth <- function(projection=NULL, stepsize=1, earth.datum=4326, bbx=c(-180, -90, 180, 90), buffer=1e-6) {
    crs_datum <- sf::st_crs(earth.datum)
    if (missing(projection)) {
        projection <- sf::st_crs(NA)
    } else {
        projection <- sf::st_crs(projection)
    }

    if (buffer==0) {
        bs <- 0
    } else {
        bs <- buffer*(10^(0:6))
    }

    for (b in bs) {
        bbxb <- bbx + c(b, b, -b, -b)
        world_bb_sf <- create_sf_rect(bbx=bbxb, stepsize = stepsize, projection=crs_datum)

        res <- if (is.na(projection)) {
            world_bb_sf
        } else {
			st_transform2(world_bb_sf, crs=projection)
        }
        if (!is.na(sf::st_is_valid(res))) break
    }

    isV <- sf::st_is_valid(res)

    if (is.na(isV) || !isV) {
    	warning("Unable to determine bounding box of the earth in projection \"", projection, "\"", call. = FALSE)
    	return(NULL)
    }

    res
}
