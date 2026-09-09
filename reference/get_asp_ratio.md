# Get aspect ratio

Get the aspect ratio of a shape object, a
[`tmap`](https://r-tmap.github.io/tmap/reference/tmap-element.html)
object, or a bounding box

## Usage

``` r
get_asp_ratio(x, is.projected = NA, width = 700, height = 700, res = 100)
```

## Arguments

- x:

  A shape from class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html),
  [`stars`](https://r-spatial.github.io/stars/reference/st_as_stars.html),
  `sp`, or `Raster`, a bounding box (that can be coerced by
  [`bb`](https://r-tmap.github.io/tmaptools/reference/bb.md)), or a
  [`tmap`](https://r-tmap.github.io/tmap/reference/tmap-element.html)
  object.

- is.projected:

  Logical that determined wether the coordinates of `x` are projected
  (`TRUE`) or longitude latitude coordinates (`FALSE`). By deafult, it
  is determined by the coordinates of `x`.

- width:

  See details; only applicable if `x` is a
  [`tmap`](https://r-tmap.github.io/tmap/reference/tmap-element.html)
  object.

- height:

  See details; only applicable if `x` is a
  [`tmap`](https://r-tmap.github.io/tmap/reference/tmap-element.html)
  object.

- res:

  See details; only applicable if `x` is a
  [`tmap`](https://r-tmap.github.io/tmap/reference/tmap-element.html)
  object.

## Value

aspect ratio

## Details

The arguments `width`, `height`, and `res` are passed on to
[`png`](https://rdrr.io/r/grDevices/png.html). If `x` is a tmap object,
a temporarily png image is created to calculate the aspect ratio of a
tmap object. The default size of this image is 700 by 700 pixels at 100
dpi.

## Examples

``` r
if (require(tmap) && packageVersion("tmap") >= "2.0") {
    data(World)

    get_asp_ratio(World)

    get_asp_ratio(bb(World))

    tm <- qtm(World)
    get_asp_ratio(tm)
}
#> [1] 2.070008

if (FALSE) { # \dontrun{
    get_asp_ratio("Germany") #note: bb("Germany") uses geocode_OSM("Germany")
} # }
```
