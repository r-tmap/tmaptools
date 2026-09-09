# Crop shape object

Crop a shape object (from class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html),
[`stars`](https://r-spatial.github.io/stars/reference/st_as_stars.html),
`sp`, or `raster`). A shape file `x` is cropped, either by the bounding
box of another shape `y`, or by `y` itself if it is a SpatialPolygons
object and `polygon = TRUE`.

## Usage

``` r
crop_shape(x, y, polygon = FALSE, ...)
```

## Arguments

- x:

  shape object, i.e. an object from class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html),
  [`stars`](https://r-spatial.github.io/stars/reference/st_as_stars.html),
  `sp`, or `raster`.

- y:

  bounding box, an
  [`st_bbox`](https://r-spatial.github.io/sf/reference/st_bbox.html),
  `extent` (`raster` package), or a shape object from which the bounding
  box is extracted (unless `polygon` is `TRUE` and `x` is an `sf`
  object).

- polygon:

  should `x` be cropped by the polygon defined by `y`? If `FALSE`
  (default), `x` is cropped by the bounding box of `x`. Polygon cropping
  only works when `x` is a spatial object and `y` is a `SpatialPolygons`
  object.

- ...:

  not used anymore

## Value

cropped shape, in the same class as `x`

## Details

This function is similar to `crop` from the `raster` package. The main
difference is that `crop_shape` also allows to crop using a polygon
instead of a rectangle.

## See also

[`bb`](https://r-tmap.github.io/tmaptools/reference/bb.md)

## Examples

``` r
if (require(tmap) && packageVersion("tmap") >= "3.99") {
    data(World, NLD_muni, land, metro)

    #land_NLD <- crop_shape(land, NLD_muni)

    #qtm(land_NLD, raster="trees", style="natural")

    metro_Europe <- crop_shape(metro, World[World$continent == "Europe", ], polygon = TRUE)

    qtm(World) +
    tm_shape(metro_Europe) +
      tm_bubbles("pop2010",
                 col="red",
                 size.legend = tm_legend("European cities")) +
      tm_legend(frame=TRUE)
}
#> 
#> ── tmap v3 code detected ───────────────────────────────────────────────────────
#> [v3->v4] `tm_legend()`: use 'tm_legend()' inside a layer function, e.g.
#> 'tm_polygons(..., fill.legend = tm_legend())'
#> This message is displayed once every 8 hours.
#> [tip] Consider a suitable map projection, e.g. by adding `+ tm_crs("auto")`.
#> This message is displayed once per session.
```
