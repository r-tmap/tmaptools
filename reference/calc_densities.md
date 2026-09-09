# Calculate densities

Transpose quantitative variables to densitiy variables, which are often
needed for choroplets. For example, the colors of a population density
map should correspond population density counts rather than absolute
population numbers.

## Usage

``` r
calc_densities(
  shp,
  var,
  target = "metric",
  total.area = NULL,
  suffix = NA,
  drop = TRUE
)
```

## Arguments

- shp:

  a shape object, i.e., an
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object.

- var:

  name(s) of a qualtity variable name contained in the `shp` data

- target:

  the target unit, see
  [`approx_areas`](https://r-tmap.github.io/tmaptools/reference/approx_areas.md).
  Density values are calculated in `var/target^2`.

- total.area:

  total area size of `shp` in number of target units (defined by
  `unit`),
  [`approx_areas`](https://r-tmap.github.io/tmaptools/reference/approx_areas.md).

- suffix:

  character that is appended to the variable names. The resulting names
  are used as column names of the returned data.frame. By default,
  `_sq_<target>`, where target corresponds to the target unit, e.g.
  `_sq_km`

- drop:

  boolean that determines whether an one-column data-frame should be
  returned as a vector

## Value

Vector or data.frame (depending on whether `length(var)==1` with density
values.

## Examples

``` r
if (require(tmap) && packageVersion("tmap") >= "3.99") {
data(NLD_muni)

NLD_muni_pop_per_km2 <- calc_densities(NLD_muni,
  target = "km km", var = c("population", "dwelling_total"))
NLD_muni <- sf::st_sf(data.frame(NLD_muni, NLD_muni_pop_per_km2))

tm_shape(NLD_muni) +
  tm_polygons(
    fill = c("population_km.2", "dwelling_total_km.2"),
    fill.legend =
      list(
        tm_legend(expression("Population per " * km^2)),
        tm_legend(expression("Dwellings per " * km^2)))) +
tm_facets(free.scales = TRUE) +
  tm_layout(panel.show = FALSE)
}
#> tm_facets(): the argument free.scales is deprecated. Specify this via the layer functions (e.g. fill.free in tm_polygons)
```
