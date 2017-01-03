#' Crop shape object
#'
#' Crop a shape object (from class \code{\link[sp:Spatial]{Spatial-class}}, \code{\link[raster:Raster-class]{Raster}}, or \code{sf}). A shape file \code{x} is cropped, either by the bounding box of another shape \code{y}, or by \code{y} itself if it is a SpatialPolygons object and \code{polygon=TRUE}.
#'
#' @param x shape object, i.e. an object from class \code{\link[sp:Spatial]{Spatial-class}}, \code{\link[raster:Raster-class]{Raster}}, or \code{sf}.
#' @param y bounding box (2 by 2 matrix), an \code{\link[raster:extent]{extent}}, or a shape object from which the bounding box is extracted (unless \code{polygon} is \code{TRUE} and \code{x} is a \code{SpatialPolygons} object).
#' @param polygon should \code{x} be cropped by the polygon defined by \code{y}. If \code{FALSE} (default), \code{x} is cropped by the bounding box of \code{x}. Polygon cropping only works when \code{x} is a spatial object and \code{y} is a \code{SpatialPolygons} object.
#' @param ... arguments passed on to \code{\link[raster:crop]{crop}}
#' @export
#' @seealso \code{\link{bb}}
#' @importFrom raster trim mask brick
#' @example ./examples/crop_shape.R
#' @return cropped shape, in the same class as \code{x}
crop_shape <- function(x, y, polygon = FALSE, ...) {
    is_sf <- inherits(x, "sf")

	xname <- deparse(substitute(x))
	yname <- deparse(substitute(y))

	if (is_sf) x <- as(x, "Spatial")

	if (!inherits(x, c("Spatial", "Raster"))) stop(xname, " is not a Spatial/Raster/sf object.", call.=FALSE)

	px <- get_projection(x)

	polycut <- polygon && inherits(y, "SpatialPolygons") # && !inherits(x, c("Raster", "SpatialGrid"))

	israsterx <- inherits(x, c("Raster", "SpatialGrid"))

	if (inherits(y, c("Spatial", "Raster", "sf"))) {
	    if (inherits(y, "sf")) y <- as(y, "Spatial")
		py <- get_projection(y)

		# align projections
		if (!is.na(px) && !is.na(py)) {
			if (px!=py) {
				y <- set_projection(y, projection = px)
			}
		}
		if (!polycut) {
			y <- bb(y, as.extent = TRUE)
		}
	} else {
	    y <- tryCatch({
	        bb(y, as.extent = TRUE)
	    }, error=function(e){
	        stop(yname, " is not a shape and cannot be coerced by bb")
	    })
	}

	# sp objects are cast to rasters for fast crop method
	sp2r2sp <- inherits(x, "SpatialGrid")# && !polycut

	hasData <- ("data" %in% slotNames(x))

	if (sp2r2sp) x <- brick(x)

	if (polycut) {
        yunion <- gUnaryUnion(y)

        if (inherits(x, "SpatialPoints")) {
          ids <- over(x, yunion)
          x2 <- x[!is.na(ids), ]
        } else if (inherits(x, "Spatial")) {
          x2 <- gIntersection(x, yunion, byid = TRUE, id=as.character(1e9 + 1:length(x)))
          if (hasData) {
            ids <- as.integer(get_IDs(x2))
            if (inherits(x, "SpatialPolygons")) {
              x2 <- SpatialPolygonsDataFrame(x2, x@data[ids-1e9, ], match.ID = FALSE)
            } else if (inherits(x, "SpatialLines")) {
              x2 <- SpatialLinesDataFrame(x2, x@data[ids-1e9, ], match.ID = FALSE)
            }
          }
        } else {
            # x has to be raster
            x2 <- raster::trim(raster::mask(x, y))
        }
	} else {
	  # bounding box crop
	  x2 <- crop(x, y, ...)

	  if (sp2r2sp) {
	    if (hasData) data <- get_raster_data(x2)
	    x2 <- as(x2, "SpatialGrid")
	    if (hasData) x2 <- SpatialGridDataFrame(x2, data=data)
	  }
	}

	if (is_sf) as(x2, "sf") else x2
}
