# Approximate area sizes of the shapes

Approximate the area sizes of the polygons in real-world area units
(such as sq km or sq mi), proportional numbers, or normalized numbers.
Also, the areas can be calibrated to a prespecified area total. This
function is a convenient wrapper around
[`st_area`](https://r-spatial.github.io/sf/reference/geos_measures.html).

## Usage

``` r
approx_areas(shp, target = "metric", total.area = NULL)
```

## Arguments

- shp:

  shape object, i.e., an
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) or `sp`
  object.

- target:

  target unit, one of

  `"prop"`:

  :   Proportional numbers. In other words, the sum of the area sizes
      equals one.

  `"norm"`:

  :   Normalized numbers. All area sizes are normalized to the largest
      area, of which the area size equals one.

  `"metric"` (default):

  :   Output area sizes will be either `"km"` (kilometer) or `"m"`
      (meter) depending on the map scale

  `"imperial"`:

  :   Output area sizes will be either `"mi"` (miles) or `"ft"` (feet)
      depending on the map scale

  other:

  :   Predefined values are "km^2", "m^2", "mi^2", and "ft^2". Other
      values can be specified as well, in which case `to` is required).

  These units are the output units. See `orig` for the coordinate units
  used by the shape `shp`.

- total.area:

  total area size of `shp` in number of target units (defined by
  `target`). Useful if the total area of the `shp` differs from a
  reference total area value. For `"metric"` and `"imperial"` units,
  please provide the total area in squared kilometers respectively
  miles.

## Value

Numeric vector of area sizes (class
[`units`](https://r-quantities.github.io/units/reference/units.html)).

## Details

Note that the method of determining areas is an approximation, since it
depends on the used projection and the level of detail of the shape
object. Projections with equal-area property are highly recommended. See
<https://en.wikipedia.org/wiki/List_of_map_projections> for equal area
world map projections.

## See also

[`approx_distances`](https://r-tmap.github.io/tmaptools/reference/approx_distances.md)

## Examples

``` r
if (require(tmap)) {
    data(NLD_muni)

    NLD_muni$area <- approx_areas(NLD_muni, total.area = 33893)

    tm_shape(NLD_muni) +
        tm_bubbles(size="area",
                   size.legend = tm_legend(title = expression("Area in " * km^2)))


    # function that returns min, max, mean and sum of area values
    summary_areas <- function(x) {
        list(min_area=min(x),
             max_area=max(x),
             mean_area=mean(x),
             sum_area=sum(x))
    }

    # area of the polygons
    summary_areas(approx_areas(NLD_muni))

    # area of the polygons, adjusted corrected for a specified total area size
    summary_areas(approx_areas(NLD_muni, total.area=33893))

    # proportional area of the polygons
    summary_areas(approx_areas(NLD_muni, target = "prop"))

    # area in squared miles
    summary_areas(approx_areas(NLD_muni, target = "mi mi"))

    # area of the polygons when unprojected
    summary_areas(approx_areas(sf::st_transform(NLD_muni, crs = 4326)))
}
#> Loading required package: tmap
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
#> $min_area
#> 7.796249 [km^2]
#> 
#> $max_area
#> 575.2348 [km^2]
#> 
#> $mean_area
#> 101.7129 [km^2]
#> 
#> $sum_area
#> 35090.95 [km^2]
#> 
```
