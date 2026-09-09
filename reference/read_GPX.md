# Read GPX file

Read a GPX file. By default, it reads all possible GPX layers, and only
returns shapes for layers that have any features.

## Usage

``` r
read_GPX(
  file,
  layers = c("waypoints", "routes", "tracks", "route_points", "track_points"),
  remove.empty.layers = TRUE,
  as.sf = TRUE
)
```

## Arguments

- file:

  a GPX filename (including directory)

- layers:

  vector of GPX layers. Possible options are `"waypoints"`, `"tracks"`,
  `"routes"`, `"track_points"`, `"route_points"`. By dedault, all those
  layers are read.

- remove.empty.layers:

  should empty layers (i.e. with 0 features) be removed from the list?

- as.sf:

  not used anymore

## Value

a list of sf objects, one for each layer

## Details

Note that this function returns
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) objects, but
still uses methods from sp and rgdal internally.
