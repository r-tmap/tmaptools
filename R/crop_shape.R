#' Crop shape object
#'
#' Crop a shape object (from class \code{\link[sp:Spatial]{Spatial}}, \code{\link[raster:Raster-class]{Raster}}, or \code{sf}). A shape file \code{x} is cropped, either by the bounding box of another shape \code{y}, or by \code{y} itself if it is a SpatialPolygons object and \code{polygon = TRUE}.
#'
#' This function is similar to \code{\link[raster:crop]{crop}} from the \code{raster} package. The main difference is that \code{crop_shape} also allows to crop using a polygon instead of a rectangle.
#'
#' @param x shape object, i.e. an object from class \code{\link[sp:Spatial]{Spatial-class}}, \code{\link[raster:Raster-class]{Raster}}, or \code{sf}.
#' @param y bounding box, an \code{\link[raster:extent]{extent}}, or a shape object from which the bounding box is extracted (unless \code{polygon} is \code{TRUE} and \code{x} is a \code{SpatialPolygons} object).
#' @param polygon should \code{x} be cropped by the polygon defined by \code{y}? If \code{FALSE} (default), \code{x} is cropped by the bounding box of \code{x}. Polygon cropping only works when \code{x} is a spatial object and \code{y} is a \code{SpatialPolygons} object.
#' @param ... arguments passed on to \code{\link[raster:crop]{crop}}
#' @export
#' @seealso \code{\link{bb}}
#' @importFrom raster trim mask brick
#' @example ./examples/crop_shape.R
#' @return cropped shape, in the same class as \code{x}
crop_shape <- function(x, y, polygon = FALSE, ...) {

    # get original names
    xname <- deparse(substitute(x))
    yname <- deparse(substitute(y))

    # check and convert x (to sf or brick)
    is_sp <- inherits(x, "Spatial")
    is_sp_raster <- inherits(x, c("SpatialGrid", "SpatialPixels"))
    if (is_sp) x <- (if (is_sp_raster) brick(x) else as(x, "sf"))
    israsterx <- inherits(x, "Raster")

	if (!inherits(x, c("sf", "sfc", "Raster"))) stop(xname, " is not a sf/Spatial/Raster object.", call.=FALSE)

	px <- get_projection(x)

	# check and convert y (to sf or brick)
	is_sp_y <- inherits(y, "Spatial")
	is_sp_raster_y <- inherits(y, c("SpatialGrid", "SpatialPixels"))
	if (is_sp_y) y <- (if (is_sp_raster_y) brick(y) else as(y, "sf"))
	israstery <- inherits(y, "Raster")

	polycut <- polygon && !israstery # && !inherits(x, c("Raster", "SpatialGrid"))


	if (inherits(y, c("Raster", "sf"))) {
		py <- get_projection(y)

		# align projections
		if (!is.na(px) && !is.na(py)) {
			if (px!=py) {
				y <- set_projection(y, projection = px)
			}
		}
		if (!polycut) {
			y <- bb(y)
		}
	} else {
	    y <- tryCatch({
	        bb(y)
	    }, error=function(e){
	        stop(yname, " is not a shape and cannot be coerced by bb")
	    })
	}

	if (polycut) {
	    if (inherits(y, "bbox")) y <- create_sf_rect(y)

        yunion <- st_union(y)
        ## REQUIRE SP
        if (israsterx) {
            yunion <- as(yunion, "Spatial")
            x2 <- raster::trim(raster::mask(x, yunion))
        } else {
            x2 <- st_intersection(x, yunion)
        }

	} else {
	  # bounding box crop (suppress warnings, because projections may not be perfectly identical)
	    y <- bb(y, output = "extent")
        x2 <- suppressWarnings(crop(x, y, ...))
	}

	x2
}
