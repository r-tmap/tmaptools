.onLoad <- function(...) {
	options(tmap.style="white", tmap.mode="plot")
	internet <- working_internet()
	assign(".internet", internet, envir = .TMAPTOOLS_CACHE)
	assign(".underCoverage", NULL, envir = .TMAPTOOLS_CACHE)
	assign(".overCoverage", NULL, envir = .TMAPTOOLS_CACHE)
}


working_internet <- function(url = "http://www.google.com") {
    # test the http capabilities of the current R build
    if (!capabilities(what = "http/ftp")) return(FALSE)

    # test connection by trying to read first line of url
    test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)

    # return FALSE if test inherits 'try-error' class
    !inherits(test, "try-error")
}


.TMAPTOOLS_CACHE <- new.env(FALSE, parent=globalenv())

.crs_longlat <- st_crs(4326)
.crs_merc <- st_crs(3857)

.CRS_longlat <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0", doCheckCRSArgs = FALSE)
.CRS_merc <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs", doCheckCRSArgs = FALSE)

get_proj4_version <- function() {
    PROJ4_version <- rgdal::getPROJ4VersionInfo()
    vid <- gregexpr("PJ_VERSION: ", PROJ4_version, fixed = TRUE)[[1]][1] + 12
    as.integer(substr(PROJ4_version, vid, nchar(PROJ4_version)-1))
}
