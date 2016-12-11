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
