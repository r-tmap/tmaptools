# Geocodes a location using OpenStreetMap Nominatim

Geocodes a location (based on a search query) to coordinates and a
bounding box. Similar to geocode from the ggmap package. It uses
OpenStreetMap Nominatim. For processing large amount of queries, please
read the usage policy
(<https://operations.osmfoundation.org/policies/nominatim/>).

## Usage

``` r
geocode_OSM(
  q,
  projection = NULL,
  return.first.only = TRUE,
  keep.unfound = FALSE,
  details = FALSE,
  as.data.frame = NA,
  as.sf = FALSE,
  geometry = c("point", "bbox"),
  server = "https://nominatim.openstreetmap.org"
)
```

## Arguments

- q:

  a character (vector) that specifies a search query. For instance
  `"India"` or `"CBS Weg 11, Heerlen, Netherlands"`.

- projection:

  projection in which the coordinates and bounding box are returned. See
  [`st_crs`](https://r-spatial.github.io/sf/reference/st_crs.html) for
  details. By default latitude longitude coordinates (EPSG 4326).

- return.first.only:

  Only return the first result

- keep.unfound:

  Keep list items / data.frame rows with `NA`s for unfound search terms.
  By default `FALSE`

- details:

  provide output details, other than the point coordinates and bounding
  box

- as.data.frame:

  Return the output as a `data.frame`. If `FALSE`, a list is returned
  with at least two items: `"coords"`, a vector containing the
  coordinates, and `"bbox"`, the corresponding bounding box. By default
  false, unless `q` contains multiple queries. If `as.sf = TRUE` (see
  below), `as.data.frame` will set to `TRUE`.

- as.sf:

  Return the output as
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object. If
  `TRUE`, `return.first.only` will be set to `TRUE`. Two geometry
  columns are added: `bbox` and `point`. The argument `geometry`
  determines which of them is set to the default geometry.

- geometry:

  When `as.sf`, this argument determines which column (`bbox` or
  `point`) is set as geometry column. Note that the geometry can be
  changed afterwards with
  [`st_set_geometry`](https://r-spatial.github.io/sf/reference/st_geometry.html).

- server:

  OpenStreetMap Nominatim server name. Could also be a local OSM
  Nominatim server.

## Value

If `as.sf` then a
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) object is
returned. Else, if `as.data.frame`, then a `data.frame` is returned,
else a list.

## See also

[`rev_geocode_OSM`](https://r-tmap.github.io/tmaptools/reference/rev_geocode_OSM.md),
[`bb`](https://r-tmap.github.io/tmaptools/reference/bb.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if (require(tmap)) {
    geocode_OSM("India")
    geocode_OSM("CBS Weg 1, Heerlen")
    geocode_OSM("CBS Weg 1, Heerlen", projection = 28992)

    data(metro)

    # sample 5 cities from the metro dataset
    five_cities <- metro[sample(length(metro), 5), ]

    # obtain geocode locations from their long names
    five_cities_geocode <- geocode_OSM(five_cities$name_long, as.sf = TRUE)

    # change to interactive mode
    current.mode <- tmap_mode("view")

    # plot metro coordinates in red and geocode coordinates in blue
    # zoom in to see the differences
    tm_shape(five_cities) +
      tm_dots(col = "blue") +
    tm_shape(five_cities_geocode) +
      tm_dots(col = "red")

    # restore current mode
    tmap_mode(current.mode)
}
} # }
```
