# Reverse geocodes a location using OpenStreetMap Nominatim

Reverse geocodes a location (based on spatial coordinates) to an
address. It uses OpenStreetMap Nominatim. For processing large amount of
queries, please read the usage policy
(<https://operations.osmfoundation.org/policies/nominatim/>).

## Usage

``` r
rev_geocode_OSM(
  x,
  y = NULL,
  zoom = NULL,
  projection = 4326,
  as.data.frame = NA,
  server = "https://nominatim.openstreetmap.org",
  params = NULL
)
```

## Arguments

- x:

  x coordinate(s), or a spatial points object
  ([`sf`](https://r-spatial.github.io/sf/reference/sf.html) or
  [`SpatialPoints`](https://edzer.github.io/sp/reference/SpatialPoints.html))

- y:

  y coordinate(s)

- zoom:

  zoom level

- projection:

  projection in which the coordinates `x` and `y` are provided.

- as.data.frame:

  return as data.frame (`TRUE`) or list (`FALSE`). By default a list,
  unless multiple coordinates are provided.

- server:

  OpenStreetMap Nominatim server name. Could also be a local OSM
  Nominatim server.

- params:

  Additional parameters to pass to server. (must start with &), ex:
  "&accept-language=en" to return english rather than local language
  results.

## Value

A data frame or a list with all attributes that are contained in the
search result

## See also

[`geocode_OSM`](https://r-tmap.github.io/tmaptools/reference/geocode_OSM.md)

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
