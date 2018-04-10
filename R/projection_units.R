#' Get or translate units of a projection
#'
#' Get the units from a projection (CRS) or a shape and determine the multiplier to transpose it in other units.
#'
#' @importFrom geosphere distGeo
#' @import sp
#' @export
#' @param x A projection (see \code{\link{get_proj4}}) or a shape (from class \code{\link[sp:Spatial]{Spatial}}, \code{\link[raster:Raster-class]{Raster}}), or \code{sf}.
#' @param target target unit, one of:  \code{"m"}, \code{"km"}, \code{"mi"}, and \code{"ft"}.
#' @param orig units by the \code{x} is defined. Only needed if this information is missing from \code{x} and \code{x} is projected. Options:  \code{"m"}, \code{"km"}, \code{"mi"}, and \code{"ft"}.
#' @param to multiplier used as follows: \code{orig * to = target}. Only needed when \code{orig} or \code{target} is unknown. For instance, if \code{target} is set to \code{"hm"} (hectometer), and \code{orig} is \code{"m"}, then \code{to} should be 100, meaning 1 hectometer equals 100 meters.
#' @param latitude latitude. Needed if the projection coordinates are in latitude longitude units. In that case the \code{orig} will be defined as longitude coordinates at latitude \code{latitude}.
#' @param show.warnings should warnings be shown?
#' @return list with the following items, \code{orig}, \code{target}, and \code{to} either according the specified argument, or determined determined. The value is \code{NA} for items that cannot be determined. The final item is \code{projected}, which is determined using \code{\link{is_projected}}
#' @seealso  \code{\link{approx_distances}} for distances between two points and \code{\link{approx_areas}} for areas.
#' @example ./examples/projection_units.R
projection_units <-function(x=NA, target="m", orig=NA, to=NA, latitude=0, show.warnings=TRUE) {
    if (target=="metric") target <- "km"
    if (target=="imperial") target <- "mi"


    # get projection (if available)
    if (inherits(x, c("Spatial", "Raster", "sf", "sfc"))) {
        projection <- get_projection(x)
    } else {
        projection <- get_proj4(x)
    }

    isprj <- (length(grep("longlat", projection, fixed=TRUE))==0)


    # subtract unit
    if (is.na(orig)) {
        pat <- '^.*\\+units ?= ?(\\S*)(.*)$'
        orig <- sub(pat, '\\1', projection[grepl(pat, projection)])
        if (length(orig)==0) orig <- NA
    }

    # insufficient info
    if (!isprj) {
        orig <- paste("long@lat", round(latitude), sep="")
        if (!target %in% names(to_m)) {
            if (show.warnings) warning("Unknown target unit")
            return(list(unit=orig, target=target, to=NA, projected=isprj))
        } else {
            to <- unname(geosphere::distGeo(c(0, latitude), c(1, latitude)) / to_m[target])
            return(list(unit=orig, target=target, to=to, projected=isprj))
        }
    } else {
        if (!is.na(to)) {
            return(list(unit=orig, target=target, to=to, projected=isprj))
        } else if (is.na(orig)) {
            return(list(unit=NA, target=target, to=NA, projected=isprj))
        } else if (!(orig %in% names(to_m))) {
            if (show.warnings) warning("Unknown original unit")
            return(list(unit=orig, target=target, to=NA, projected=isprj))
        } else if (!target %in% names(to_m)) {
            if (show.warnings) warning("Unknown target unit")
            return(list(unit=orig, target=target, to=NA, projected=isprj))
        } else {
            to <- unname(to_m[orig] / to_m[target])
            return(list(unit=orig, target=target, to=to, projected=isprj))
        }
    }
}

# conversion vector (for now assume metric and imperial are in km and mi, check later)
to_m <- c(m=1, km=1000, mi=1609.344, miles=1609.344, ft=0.304800609601219, 'us-ft'=0.304800609601219, metric=1000, imperial=1609.344,NA)

