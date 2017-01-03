#' Get aspect ratio
#'
#' Get the aspect ratio of a shape object, a \code{\link[tmap:tmap-element]{tmap}} object, or a bounding box
#'
#' The arguments \code{width}, \code{height}, and \code{res} are passed on to \code{\link[grDevices:png]{png}}. If \code{x} is a tmap object, a temporarily png image is created to calculate the aspect ratio of a tmap object. The default size of this image is 700 by 700 pixels at 100 dpi.
#'
#' @param x shape object (either \code{\link[sp:Spatial]{Spatial}}, a \code{\link[raster:Raster-class]{Raster}}, or a \code{sf}), a bounding box (that can be coerced by \code{\link{bb}}), or a \code{\link[tmap:tmap-element]{tmap}} object.
#' @param is.projected Logical that determined wether the coordinates of \code{x} are projected (\code{TRUE}) or longitude latitude coordinates (\code{FALSE}). By deafult, it is determined by the coordinates of \code{x}.
#' @param width See details; only applicable if \code{x} is a \code{\link[tmap:tmap-element]{tmap}} object.
#' @param height See details; only applicable if \code{x} is a \code{\link[tmap:tmap-element]{tmap}} object.
#' @param res See details; only applicable if \code{x} is a \code{\link[tmap:tmap-element]{tmap}} object.
#' @return aspect ratio
#' @import sp
#' @importFrom raster couldBeLonLat
#' @example ./examples/get_asp_ratio.R
#' @export
get_asp_ratio <- function(x, is.projected = NA, width=700, height=700, res=100) {
	if (inherits(x, "tmap")) {
		tmp <- tempfile(fileext = ".png")
		png(tmp, width=width, height=height, res = res)
		asp <- print(x, return.asp = TRUE)
		dev.off()
	} else {
	    if (inherits(x, c("Spatial", "Raster", "sf"))) {
	        bbx <- bb(x)
	        if (is.na(is.projected)) is.projected <- is_projected(x)
	    } else {
	        bbx <- bb(x)
	        if (is.na(is.projected)) is.projected <- !maybe_longlat(bbx)
	    }

	    xlim <- bbx[1, ]
	    ylim <- bbx[2, ]

	    asp <- if (diff(xlim)==0 || diff(ylim)==0) {
	        1
	    } else unname((diff(xlim)/diff(ylim)) * ifelse(is.projected, 1, cos((mean(ylim) * pi)/180)))
	}
	asp
}
