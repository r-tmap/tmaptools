#' Crop shape object
#'
#' Crop a shape object (from class \code{\link[sp:Spatial]{Spatial}}, \code{\link[raster:Raster-class]{Raster}}, or \code{\link[sf:sf]{sf}}). A shape file \code{x} is cropped, either by the bounding box of another shape \code{y}, or by \code{y} itself if it is a SpatialPolygons object and \code{polygon = TRUE}.
#'
#' This function is similar to \code{\link[raster:crop]{crop}} from the \code{raster} package. The main difference is that \code{crop_shape} also allows to crop using a polygon instead of a rectangle.
#'
#' @param x shape object, i.e. an object from class \code{\link[sp:Spatial]{Spatial-class}}, \code{\link[raster:Raster-class]{Raster}}, or \code{\link[sf:sf]{sf}}.
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

    x <- to_sf_stars(x)
    israsterx <- inherits(x, "stars")
	px <- sf::st_crs(x)

    if (inherits(y, c("sf", "sfc", "stars", "Spatial", "Raster"))) {
        y <- to_sf_stars(y)
        israstery <- inherits(y, "stars")
        py <- sf::st_crs(y)
        polycut <- polygon && !israstery

        # align projections
        if (!is.na(px) && !is.na(py)) {
            if (px!=py) {
                y <- sf::st_transform(y, crs = px)
            }
        }
        if (!polycut) {
            y <- bb(y)
        }

    } else {
        israstery <- FALSE
        polycut <- FALSE

        y <- tryCatch({
            bb(y)
        }, error=function(e){
            stop(yname, " is not a shape and cannot be coerced by bb")
        })

    }

	if (polycut) {
	    if (inherits(y, "bbox")) y <- create_sf_rect(y)

        yunion <- sf::st_union(y)
        ## REQUIRE SP
        if (israsterx) {
            stop("Rasterized method not implemented yet")
            #yunion <- as(yunion, "Spatial")
            #x2 <- raster::trim(raster::mask(x, yunion))
        } else {
            x2 <-  suppressMessages(suppressWarnings(sf::st_intersection(x, yunion)))
        }

	} else {
	  # bounding box crop (suppress warnings, because projections may not be perfectly identical)

	    if (israsterx) {
	        stop("Rasterized method not implemented yet")
	        # y <- bb(y, output = "extent")
	        # x2 <- suppressWarnings(crop(x, y, ...))
	    } else {
	        y <- create_sf_rect(y)
	        x2 <- suppressMessages(suppressWarnings(sf::st_intersection(x, y)))
	    }
	}
	x2
}
