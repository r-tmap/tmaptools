# Get neighbours list from spatial objects

Get neighbours list from spatial objects. The output is similar to the
function `poly2nb` of the `spdep` package with `queen = FALSE`, but uses
`sf` instead of `sp`. Two polygons are considered neighbours only when
their boundaries share a line segment (an edge), not merely a point.

## Usage

``` r
get_neighbours(x)
```

## Arguments

- x:

  a shape object, i.e., a
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object or a
  `SpatialPolygons(DataFrame)` (`sp` package).

## Value

A list where the items correspond to the features. Each item is a vector
of neighbours.
