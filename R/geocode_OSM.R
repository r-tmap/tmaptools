#' Geocodes a location using OpenStreetMap Nominatim
#'
#' Geocodes a location (based on a search query) to coordinates and a bounding box. Similar to geocode from the ggmap package. It uses OpenStreetMap Nominatim. For processing large amount of queries, please read the usage policy (\url{https://operations.osmfoundation.org/policies/nominatim/}).
#'
#' @param q a character (vector) that specifies a search query. For instance \code{"India"} or \code{"CBS Weg 11, Heerlen, Netherlands"}.
#' @param projection projection in which the coordinates and bounding box are returned. See \code{\link[sf:st_crs]{st_crs}} for details. By default latitude longitude coordinates (EPSG 4326).
#' @param return.first.only Only return the first result
#' @param keep.unfound Keep list items / data.frame rows with \code{NA}s for unfound search terms. By default \code{FALSE}
#' @param details provide output details, other than the point coordinates and bounding box
#' @param as.data.frame Return the output as a \code{data.frame}. If \code{FALSE}, a list is returned with at least two items: \code{"coords"}, a vector containing the coordinates, and \code{"bbox"}, the corresponding bounding box. By default false, unless \code{q} contains multiple queries. If \code{as.sf = TRUE} (see below), \code{as.data.frame} will set to \code{TRUE}.
#' @param as.sf Return the output as \code{\link[sf:sf]{sf}} object. If \code{TRUE}, \code{return.first.only} will be set to \code{TRUE}. Two geometry columns are added: \code{bbox} and \code{point}. The argument \code{geometry} determines which of them is set to the default geometry.
#' @param geometry When \code{as.sf}, this argument determines which column (\code{bbox} or \code{point}) is set as geometry column. Note that the geometry can be changed afterwards with \code{\link[sf:st_geometry]{st_set_geometry}}.
#' @param server OpenStreetMap Nominatim server name. Could also be a local OSM Nominatim server.
#' @return If \code{as.sf} then a \code{\link[sf:sf]{sf}} object is returned. Else, if \code{as.data.frame}, then a \code{data.frame} is returned, else a list.
#' @export
#' @importFrom XML xmlChildren xmlRoot xmlAttrs xmlTreeParse xmlValue
#' @importFrom stars st_as_stars
#' @example ./examples/geocode_OSM.R
#' @seealso \code{\link{rev_geocode_OSM}}, \code{\link{bb}}
geocode_OSM <- function(q, projection=NULL, return.first.only=TRUE, keep.unfound = FALSE, details=FALSE, as.data.frame=NA, as.sf=FALSE, geometry=c("point", "bbox"), server="https://nominatim.openstreetmap.org") {
    n <- length(q)
	q2 <- gsub(" ", "+", enc2utf8(q), fixed = TRUE)
	addr <- paste0(server, "/search?q=", q2, "&format=xml&polygon=0&addressdetails=0")

	geometry <- match.arg(geometry)

	project <- !missing(projection)


	if (is.na(as.data.frame)) as.data.frame <- (n>1)
	if (as.sf) {
		as.data.frame <- TRUE
		return.first.only <- TRUE
	}
	sn_names <- c("place_id", "osm_type", "osm_id", "place_rank", "display_name", "class", "type", "importance", "icon")



	output2 <- lapply(1:n, function(k) {
		tmpfile <- tempfile()
		suppressWarnings(download.file(addr[k], destfile = tmpfile, mode= "wb", quiet = TRUE))

		doc <- xmlTreeParse(tmpfile, encoding="UTF-8")
		unlink(tmpfile)

		res <- xmlChildren(xmlRoot(doc))

		if (length(res)==0) {
			message(paste("No results found for \"", q[k], "\".", sep="")) #if (n==1)
		    if (keep.unfound) {
		        if (as.data.frame) {
		            res = if (project) {
		                list(query = q[k], x = as.numeric(as.numeric(NA)), y = as.numeric(NA), y_min = as.numeric(NA),
		                     y_max = as.numeric(NA), x_min = as.numeric(NA), x_max = as.numeric(NA))
		            } else {
		                list(query = q[k], lat = as.numeric(NA), lon = as.numeric(NA), lat_min = as.numeric(NA),
		                     lat_max = as.numeric(NA), lon_min = as.numeric(NA), lon_max = as.numeric(NA))
		            }
		        } else {
		            res = list(queue = q[k], coords = c(x=NA, y = NA), bbox = sf::st_bbox())
		        }
		        if (details) res[sn_names] = as.character(NA)
		        if (as.sf) res$bbox = sf::st_sfc(sf::st_polygon())

		        if (as.data.frame) res <- as.data.frame(res, stringsAsFactors=FALSE)
		        return(list(res))
		    } else return(NULL)
		}

		idx <- if (return.first.only) 1 else 1:length(res)

		output <- lapply(idx, function(i) {
			search_result <- xmlAttrs(res[[i]])

			search_result_id <- search_result[sn_names]
			names(search_result_id) <- sn_names # in case of missings
			Encoding(search_result_id) <- "UTF-8"

			search_result_loc <- as.numeric(search_result[c("lat", "lon")])
			names(search_result_loc) <- c("lat", "lon")

			search_result_bb <- as.numeric(unlist(strsplit(search_result["boundingbox"], ",")))

			if (!project) {
				names(search_result_bb) <- c("lat_min", "lat_max", "lon_min", "lon_max")
				b <- bb(xlim=search_result_bb[3:4], ylim=search_result_bb[1:2])

				coords <- search_result_loc[c("lon", "lat")]
				names(coords) <- c("x", "y")

			} else {
				b <- bb(xlim=search_result_bb[3:4], ylim=search_result_bb[1:2], current.projection = .crs_longlat, projection=projection)

				search_result_bb <- b[c(2,4,1,3)]
				names(search_result_bb) <- c("y_min", "y_max", "x_min", "x_max")

                p <- sf::st_sf(sf::st_sfc(sf::st_point(search_result_loc[2:1]), crs = .crs_longlat))

				p <- sf::st_transform(p, crs=projection)

				coords <- as.vector(sf::st_coordinates(p))
				names(coords) <- c("x", "y")

				search_result_loc <- as.list(coords)
				names(search_result_loc) <- c("x", "y")
			}

			if (as.sf) {
			    bbpoly <- bb_poly(b)
			}

			res <- if (as.data.frame) {
				c(list(query=q[k]),
				  search_result_loc,
				  search_result_bb)
			} else {
				c(list(query=q[k],
					   coords=coords,
					   bbox=b))
			}

			if (as.sf) {
			    res <- c(res, list(bbox=bbpoly))
			}

			if (details) res <- c(res, search_result_id)
			if (as.data.frame) res <- as.data.frame(res, stringsAsFactors=FALSE)
			res
		})
	})

	output3 <- do.call(c, output2)

	if (is.null(output3)) return(NULL)

	if (as.data.frame) {
		df <- do.call(rbind, output3)

		if (as.sf) {
		    names(df)[names(df) == "geometry"] <- "bbox"
			if (!project) {
			    df$x <- df$lon
			    df$y <- df$lat
			    res <- suppressWarnings(sf::st_as_sf(df, coords = c("x","y"), crs=.crs_longlat, na.fail = FALSE))
			} else {
			    df$x2 <- df$x
			    df$y2 <- df$y
			    res <- suppressWarnings(sf::st_as_sf(df, coords = c("x2","y2"), crs=.crs_longlat, na.fail = FALSE))
			}
		    names(res)[names(res) == "geometry"] <- "point"

            if (geometry == "point") res <- sf::st_set_geometry(res, "point")
		    sf::st_set_crs(res, .crs_longlat)
		} else {
			df
		}
	} else {
		if (length(output3)==1) {
			output3[[1]]
		} else output3
	}
}


#' Reverse geocodes a location using OpenStreetMap Nominatim
#'
#' Reverse geocodes a location (based on spatial coordinates) to an address. It uses OpenStreetMap Nominatim. For processing large amount of queries, please read the usage policy (\url{https://operations.osmfoundation.org/policies/nominatim/}).
#'
#' @param x x coordinate(s), or a spatial points object (\code{\link[sf:sf]{sf}} or \code{\link[sp:SpatialPoints]{SpatialPoints}})
#' @param y y coordinate(s)
#' @param zoom zoom level
#' @param projection projection in which the coordinates \code{x} and \code{y} are provided.
#' @param as.data.frame return as data.frame (\code{TRUE}) or list (\code{FALSE}). By default a list, unless multiple coordinates are provided.
#' @param server OpenStreetMap Nominatim server name. Could also be a local OSM Nominatim server.
#' @export
#' @importFrom XML xmlChildren xmlRoot xmlAttrs xmlTreeParse xmlValue
#' @return A data frame or a list with all attributes that are contained in the search result
#' @example ./examples/rev_geocode_OSM.R
#' @seealso \code{\link{geocode_OSM}}
rev_geocode_OSM <- function(x, y=NULL, zoom=NULL, projection=4326, as.data.frame=NA, server="https://nominatim.openstreetmap.org") {

	project <- !missing(projection)



	if (inherits(x, "Spatial")) x <- as(x, "sf")

	if (inherits(x, "sf")) {
	    if (!all(sf::st_geometry_type(x) == "POINT")) stop("sf object should only contain POINT geometries")

        x <- sf::st_transform(x, crs = .crs_longlat)

		n <- nrow(x)
		co <- sf::st_coordinates(x)
		lon <- x <- co[,1]
		lat <- y <- co[,2]
	} else {
	    n <- 1
		if (length(x) > 1 || length(y) > 1) {
			n <- max(length(x), length(y))
			x <- rep(x, length.out=n)
			y <- rep(y, length.out=n)
		}
		if (!project) {
			lon <- x
			lat <- y
		} else {
			projection <- sf::st_crs(projection)
			coords <- data.frame(x=x, y=y)
			single_point <- sf::st_as_sf(x = coords, coords = c("x", "y"), crs = projection)
			coords <- sf::st_transform(single_point, crs = .crs_longlat)
			lon <- sf::st_coordinates(coords)[,1]
			lat <- sf::st_coordinates(coords)[,2]
		}
	}

	if (is.na(as.data.frame)) as.data.frame <- (n>1)

	if (missing(zoom)) {
		qzoom <- ""
		strzoom <- ""
	} else {
		qzoom <- paste0("&zoom=", zoom)
		strzoom <- paste0(", zoom = ", zoom)
	}

	addr <- paste0(server, "/reverse?format=xml&lat=", lat, "&lon=", lon, qzoom, "&addressdetails=1")


	dfs <- lapply(1:n, function(i) {
		# download query
		tmpfile <- tempfile()
		suppressWarnings(download.file(addr[i], destfile = tmpfile, mode= "wb", quiet = TRUE))
		doc <- xmlTreeParse(tmpfile, encoding="UTF-8")
		unlink(tmpfile)

		# read xml document
		res <- xmlChildren(xmlRoot(doc))

		# get name
		result_name <- xmlValue(res[[1]])
		Encoding(result_name) <- "UTF-8"

		# get osm id, location, bbox
		search_result <- xmlAttrs(res[[1]])
		search_result_id <- search_result[c("place_id", "osm_type", "osm_id", "ref")]
		names(search_result_id) <- c("place_id", "osm_type", "osm_id", "ref") # in case of missings
		Encoding(search_result_id) <- "UTF-8"
		search_result_ll <- as.numeric(search_result[c("lat", "lon")])
		names(search_result_ll) <- c("lat", "lon")
		search_result_bb <- as.numeric(unlist(strsplit(search_result["boundingbox"], ",")))
		names(search_result_bb) <- c("lat_min", "lat_max", "lon_min", "lon_max")

		# get address
		addr_result <- xmlChildren(res[[2]])
		dfnames <- names(addr_result)
		dfvalues <- lapply(1:length(addr_result), function(j) {
			v <- xmlValue(addr_result[[j]])
			Encoding(v) <- "UTF-8"
			v
		})
		names(dfvalues) <- dfnames

		c(list(x=x[i],
			 y=y[i],
			 name=result_name),
		  search_result_id,
		  search_result_ll,
		  search_result_bb,
		  dfvalues)
	})

	# cast to data.frame
	if (as.data.frame) {
		addrnames <- sort(unique(unlist(lapply(dfs, function(df) {
			names(df)[14:length(df)]
		}))))

		addrlist <- lapply(addrnames, function(a) NA)
		names(addrlist) <- addrnames

		do.call(rbind, c(lapply(dfs, function(df) {
			sel <- 14:length(df)
			addrlist[names(df)[sel]] <- df[sel]
			as.data.frame(c(df[1:13], addrlist), stringsAsFactors=FALSE)
		}), list(stringsAsFactors=FALSE)))
	} else {
		dfs
	}
}
