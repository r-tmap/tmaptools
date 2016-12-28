#' Convert bounding box to a spatial polygon
#'
#' Convert bounding box to a \code{\link[sp:SpatialPolygons]{SpatialPolygons}}. Useful for plotting (see example). The function \code{bb_earth} returns a spatial polygon of the 'boundaries' of the earth, which can also be done in other projections (if a feasible solution exists).
#'
#' @param x object that can be coerced to a bounding box with \code{\link{bb}}
#' @param projection projection in which the coordinates of \code{x} are provided, see \code{\link{get_proj4}}. For \code{bb_earth}, \code{projection} is the projection in which the bounding box is returned (if possible).
#' @param steps number of intermediate points along the shortest edge of the bounding box. The number of intermediate points along the longest edge scales with the aspect ratio. These intermediate points are needed if the bounding box is plotted in another projection.
#' @param stepsize stepsize in terms of coordinates (usually meters when the shape is projected and degrees of longlat coordinates are used). If specified, it overrules \code{steps}
#' @param earth.datum Geodetic datum to determine the earth boundary. By default \code{"WGS84"}, other frequently used datums are \code{"NAD83"} and \code{"NAD27"}. Any other \code{PROJ.4} character string can be used. See \code{\link{get_proj4}}.
#' @param bbx boundig box of the earth in a vector of 4 values: min longitude, max longitude, min latitude, max latitude. By default \code{c(-180, 180, -90, 90)}. If for some \code{projection}, a feasible solution does not exist, it may be wise to choose a smaller bbx, e.g. \code{c(-180, 180, -88, 88)}. However, this is also automatically done with the next argument, \code{buffer}.
#' @param buffer In order to determine feasible earth bounding boxes in other projections, a buffer is used to decrease the bounding box by a small margin (default \code{1e-06}). This value is subtracted from each the bounding box coordinates. If it still does not result in a feasible bounding box, this procedure is repeated 5 times, where each time the buffer is multiplied by 10. Set \code{buffer=0} to disable this procedure.
#' @return \code{\link[sp:SpatialPolygons]{SpatialPolygons}}
#' @example ./examples/bb_sp.R
#' @name bb_sp
#' @rdname bb_sp
#' @export
bb_sp <- function(x, projection=NULL, steps=100, stepsize=NA) {
    bbx <- bb(x)
    bbe <- bb(bbx, as.extent = TRUE)

    if (is.na(stepsize)) {
        stepsize <- min((bbe[2] - bbe[1])/steps,
                        (bbe[4] - bbe[3])/steps)
    }

    if (missing(projection)) {
        if (inherits(x, c("Spatial", "Raster", "sf"))) {
            projection <- get_projection(x, as.CRS=FALSE, guess.longlat = TRUE)
        } else if (maybe_longlat(bbx)) {
            projection <- get_proj4("longlat")
        } else {
            stop("projection is missing and bbox do no seem to be long lat coordinates")
        }
    }
    s <- create_sp_rect(bbe, stepsize=stepsize, projection=projection)
}

#' @name bb_earth
#' @rdname bb_sp
#' @export
bb_earth <- function(projection=NULL, stepsize=1, earth.datum="WGS84", bbx=c(-180, 180, -90, 90), buffer=1e-6) {
    CRS_datum <- get_proj4(earth.datum, as.CRS = TRUE)
    if (missing(projection))
        projection <- NA
    else
        projection <- get_proj4(projection, as.CRS = TRUE)

    if (buffer==0) {
        bs <- 0
    } else {
        bs <- buffer*(10^(0:6))
    }

    for (b in bs) {
        bbxb <- bbx + c(b, -b, b, -b)
        world_bb_sp <- create_sp_rect(bbx=bbxb, stepsize = stepsize, id="world_bb", projection=CRS_datum)

        res <- if (is.na(projection)) {
            world_bb_sp
        } else {
            tryCatch({
                suppressMessages({
                    spTransform(world_bb_sp, projection)
                })
            }, error=function(e){
                NULL
            }, warning=function(w){
                NULL
            })
        }
        if (!is.null(res)) break
    }

    if (is.null(res)) warning("Unable to determine bounding box of the earth in projection \"", projection, "\"", call. = FALSE)
    res

}

create_sp_rect <- function(bbx, stepsize, id="rect", projection) {
    x1 <- bbx[1]
    x2 <- bbx[2]
    y1 <- bbx[3]
    y2 <- bbx[4]

    dx <- x2-x1
    dy <- y2-y1

    ny <- dy / stepsize + 1
    nx <- dx / stepsize + 1
    step <- stepsize

    crds <- matrix(c(
        rep(x1, ny), rep(seq(x1+step, x2-step, by=step), length.out=nx-2), rep(x2, ny), rep(seq(x2-step, x1+step, by=-step), length.out=nx-2),
        rep(seq(y1, y2, by=step), length.out=ny), rep(y2, nx-2), rep(seq(y2, y1, by=-step), length.out=ny), rep(y1, nx-2)), ncol=2)

    SpatialPolygons(list(Polygons(list(Polygon(coords=crds)), ID=id)), proj4string=get_proj4(projection, as.CRS = TRUE))
}
