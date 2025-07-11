% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocode_OSM.R
\name{rev_geocode_OSM}
\alias{rev_geocode_OSM}
\title{Reverse geocodes a location using OpenStreetMap Nominatim}
\usage{
rev_geocode_OSM(
  x,
  y = NULL,
  zoom = NULL,
  projection = 4326,
  as.data.frame = NA,
  server = "https://nominatim.openstreetmap.org",
  params = NULL
)
}
\arguments{
\item{x}{x coordinate(s), or a spatial points object (\code{\link[sf:sf]{sf}} or \code{\link[sp:SpatialPoints]{SpatialPoints}})}

\item{y}{y coordinate(s)}

\item{zoom}{zoom level}

\item{projection}{projection in which the coordinates \code{x} and \code{y} are provided.}

\item{as.data.frame}{return as data.frame (\code{TRUE}) or list (\code{FALSE}). By default a list, unless multiple coordinates are provided.}

\item{server}{OpenStreetMap Nominatim server name. Could also be a local OSM Nominatim server.}

\item{params}{Additional parameters to pass to server.  (must start with &), ex: "&accept-language=en" to return english rather than local language results.}
}
\value{
A data frame or a list with all attributes that are contained in the search result
}
\description{
Reverse geocodes a location (based on spatial coordinates) to an address. It uses OpenStreetMap Nominatim. For processing large amount of queries, please read the usage policy (\url{https://operations.osmfoundation.org/policies/nominatim/}).
}
\examples{
\dontrun{
if (require(tmap)) {
    data(metro)

    # sample five cities from metro dataset
    set.seed(1234)
    five_cities <- metro[sample(length(metro), 5), ]

    # obtain reverse geocode address information
    addresses <- rev_geocode_OSM(five_cities, zoom = 6)
    five_cities <- sf::st_sf(data.frame(five_cities, addresses))

    # change to interactive mode
    current.mode <- tmap_mode("view")
    tm_shape(five_cities) +
    	tm_markers(text="name")

    # restore current mode
    tmap_mode(current.mode)
}
}
}
\seealso{
\code{\link{geocode_OSM}}
}
