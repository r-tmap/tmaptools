# Approximate distances

Approximate distances between two points or across the horizontal and
vertical centerlines of a bounding box.

## Usage

``` r
approx_distances(x, y = NULL, projection = NULL, target = NULL)
```

## Arguments

- x:

  object that can be coerced to a bounding box with
  [`bb`](https://r-tmap.github.io/tmaptools/reference/bb.md), or a pair
  of coordintes (vector of two). In the former case, the distance across
  the horizontal and vertical centerlines of the bounding box are
  approximated. In the latter case, `y` is also required; the distance
  between points `x` and `y` is approximated.

- y:

  a pair of coordintes, vector of two. Only required when `x` is also a
  pair of coordintes.

- projection:

  projection code, needed in case `x` is a bounding box or when `x` and
  `y` are pairs of coordinates.

- target:

  target unit, one of: `"m"`, `"km"`, `"mi"`, and `"ft"`.

## Value

If `y` is specifyed, a list of two: unit and dist. Else, a list of
three: unit, hdist (horizontal distance) and vdist (vertical distance).

## See also

[`approx_areas`](https://r-tmap.github.io/tmaptools/reference/approx_areas.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if (require(tmap)) {
    data(NLD_prov)

    # North-South and East-West distances of the Netherlands
    approx_distances(NLD_prov)

    # Distance between Maastricht and Groningen
    p_maastricht <- geocode_OSM("Maastricht")$coords
    p_groningen <- geocode_OSM("Groningen")$coords
    approx_distances(p_maastricht, p_groningen, projection = 4326, target = "km")

    # Check distances in several projections
    sapply(c(3035, 28992, 4326), function(projection) {
        p_maastricht <- geocode_OSM("Maastricht", projection = projection)$coords
        p_groningen <- geocode_OSM("Groningen", projection = projection)$coords
        approx_distances(p_maastricht, p_groningen, projection = projection)
    })
}
} # }
```
