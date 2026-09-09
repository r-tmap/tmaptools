# Convert bounding box to a spatial polygon

Convert bounding box to a spatial
([`sfc`](https://r-spatial.github.io/sf/reference/sfc.html)) object .
Useful for plotting (see example). The function `bb_earth` returns a
spatial polygon of the 'boundaries' of the earth, which can also be done
in other projections (if a feasible solution exists).

## Usage

``` r
bb_poly(x, steps = 100, stepsize = NA, projection = NULL)

bb_earth(
  projection = NULL,
  stepsize = 1,
  earth.datum = 4326,
  bbx = c(-180, -90, 180, 90),
  buffer = 1e-06
)
```

## Arguments

- x:

  object that can be coerced to a bounding box with
  [`bb`](https://r-tmap.github.io/tmaptools/reference/bb.md)

- steps:

  number of intermediate points along the shortest edge of the bounding
  box. The number of intermediate points along the longest edge scales
  with the aspect ratio. These intermediate points are needed if the
  bounding box is plotted in another projection.

- stepsize:

  stepsize in terms of coordinates (usually meters when the shape is
  projected and degrees of longlat coordinates are used). If specified,
  it overrules `steps`

- projection:

  projection in which the coordinates of `x` are provided. For
  `bb_earth`, `projection` is the projection in which the bounding box
  is returned (if possible).

- earth.datum:

  Geodetic datum to determine the earth boundary. By default EPSG 4326.

- bbx:

  boundig box of the earth in a vector of 4 values: min longitude, max
  longitude, min latitude, max latitude. By default
  `c(-180, 180, -90, 90)`. If for some `projection`, a feasible solution
  does not exist, it may be wise to choose a smaller bbx, e.g.
  `c(-180, 180, -88, 88)`. However, this is also automatically done with
  the next argument, `buffer`.

- buffer:

  In order to determine feasible earth bounding boxes in other
  projections, a buffer is used to decrease the bounding box by a small
  margin (default `1e-06`). This value is subtracted from each the
  bounding box coordinates. If it still does not result in a feasible
  bounding box, this procedure is repeated 5 times, where each time the
  buffer is multiplied by 10. Set `buffer=0` to disable this procedure.

## Value

[`sfc`](https://r-spatial.github.io/sf/reference/sfc.html) object

## Examples

``` r
if (require(tmap) && packageVersion("tmap") >= "2.0") {
    data(NLD_muni)

    current.mode <- tmap_mode("view")
    qtm(bb_poly(NLD_muni))

    # restore mode
    tmap_mode(current.mode)
}
#> ℹ tmap modes "plot" - "view"
#> ℹ toggle with `tmap::ttm()`
#> This message is displayed once per session.
#> ℹ tmap modes "plot" - "view"
```
