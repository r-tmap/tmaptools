#' Get a PROJ.4 character string
#'
#' Get full PROJ.4 string from an existing PROJ.4 string, a shortcut, or a \code{\link[sp:CRS]{CRS}} object.
#'
#' @param x a projection. One of:
#' \enumerate{
#'    \item{a \code{PROJ.4} character string}
#'    \item{a \code{\link[sf:st_crs]{crs}} object}
#'    \item{a \code{\link[sp:CRS]{CRS}} object}
#'    \item{an EPSG code}
#'    \item{one the following shortcuts codes:
#'      \describe{
#'    	\item{\code{"longlat"}}{Not really a projection, but a plot of the longitude-latitude coordinates (WGS84 datum).}
#'    	\item{\code{"wintri"}}{Winkel Tripel (1921). Popular projection that is useful in world maps. It is the standard of world maps made by the National Geographic Society. Type: compromise}
#'    	\item{\code{"robin"}}{Robinson (1963). Another popular projection for world maps. Type: compromise}
#'    	\item{\code{"eck4"}}{Eckert IV (1906). Projection useful for world maps. Area sizes are preserved, which makes it particularly useful for truthful choropleths. Type: equal-area}
#'    	\item{\code{"hd"}}{Hobo-Dyer (2002). Another projection useful for world maps in which area sizes are preserved. Type: equal-area}
#'    	\item{\code{"gall"}}{Gall (Peters) (1855). Another projection useful for world maps in which area sizes are preserved. Type: equal-area}
#'    	\item{\code{"merc"}}{Web Mercator. Projection in which shapes are locally preserved, a variant of the original Mercator (1569), used by Google Maps, Bing Maps, and OpenStreetMap. Areas close to the poles are inflated. Type: conformal}
#'    	\item{\code{"utmXX(s)"}}{Universal Transverse Mercator. Set of 60 projections where each projection is a traverse mercator optimized for a 6 degree longitude range. These ranges are called UTM zones. Zone \code{01} covers -180 to -174 degrees (West) and zone \code{60} 174 to 180 east. Replace XX in the character string with the zone number. For southern hemisphere, add \code{"s"}. So, for instance, the Netherlands is \code{"utm31"} and New Zealand is \code{"utm59s"}}
#'    	\item{\code{"mill"}}{Miller (1942). Projetion based on Mercator, in which poles are displayed. Type: compromise}
#'    	\item{\code{"eqc0"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The equator is the standard parallel. Also known as Plate Carr\'ee. Type: equidistant}
#'    	\item{\code{"eqc30"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 30 is the standard parallel. Type: equidistant}
#'    	\item{\code{"eqc45"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 45 is the standard parallel. Also known as Gall isographic. Type: equidistant}
#'    	\item{\code{"laea_Eur"}}{European Lambert Azimuthal Equal Area Projection. Similar to EPSG code 3035.}
#'    	\item{\code{"laea_NA"}}{North American Lambert Azimuthal Equal Area Projection. Known as SR-ORG:7314.}
#'    	\item{\code{"rd"}}{Rijksdriehoekstelsel. Triangulation coordinate system used in the Netherlands.}
#'    }}
#' }
#' @param output the output format of the projection, one of \code{"character"}, \code{"crs"},\code{"epsg"}, or \code{"CRS"}
#' @param as.CRS should a CRS object be returned instead of a PROJ.4 character string? Default is \code{FALSE}.
#'	@return validated PROJ.4 character string, or, if \code{as.CRS=TRUE} a \code{\link[sp:CRS]{CRS}} object.
#'	@importFrom rgdal CRSargs make_EPSG checkCRSArgs
#'	@import sp
#'	@import sf
#'	@seealso \url{http://en.wikipedia.org/wiki/List_of_map_projections} for a overview of projections. \url{http://trac.osgeo.org/proj/} for the \code{PROJ.4} project home page. An extensive list of \code{PROJ.4} codes can be created with rgdal's \code{\link[rgdal:make_EPSG]{make_EPSG}}.
#'	@export
get_proj4 <- function(x, as.CRS=FALSE, output = c("crs", "character", "epsg", "CRS")) {
    output <- match.arg(output)
	y <- if (is.null(x)) {
		return(NULL)
	} else if (is.na(x)) {
		sf::st_crs()
	} else if (inherits(x, "crs")) {
	    x
	} else if (inherits(x, "CRS")) {
	    sf::st_crs(attr(x, "projargs"))
	} else if (!is.numeric(x) && !is.character(x)) {
		stop("x is not a character, crs object, CRS object, nor a number", call.=FALSE)
	} else {
		if (x %in% names(.proj_sc)) {
			sf::st_crs(unname(.proj_sc[x]))
		} else if (is_num_string(x)) {
		    sf::st_crs(x)
		} else if (substr(x, 1, 3)=="utm") {
		    if (!(nchar(x) %in% c(5,6))) stop("\"utm\" shortcut code should be utmXX or utmXXs where XX refers to the utm zone")
			sf::st_crs(paste("+proj=utm +zone=", substr(x, 4, 5), ifelse(substr(x, 6, 6)=="s", " +south", ""), " +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0", sep=""))
		} else {
			sf::st_crs(x)
		}
	}

	switch(output,
	       character = y$proj4string,
	       crs = y,
	       epsg = y$epsg,
	       CRS = CRS(ifelse(is.na(y$proj4string), "", y$proj4string)))
}

is_num_string <- function(x) {
    suppressWarnings(!is.na(as.numeric(x)))
}

.proj_sc <- c(longlat="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0",
			  latlong="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0",
			  WGS84="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0",
			  NAD83="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0",
			  NAD27="+proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat",
			  wintri="+proj=wintri +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  robin="+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  eck4="+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  hd="+proj=cea +lat_ts=37.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  gall="+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  merc="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs",
			  mill="+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +R_A +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  eqc0="+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  eqc30="+proj=eqc +lat_ts=30 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  eqc45="+proj=eqc +lat_ts=45 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  laea_Eur="+init=epsg:3035 +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
			  laea_NA="+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  rd="+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957388243134,0.343987817378283,-1.87740163998045,4.0725 +units=m +no_defs")
