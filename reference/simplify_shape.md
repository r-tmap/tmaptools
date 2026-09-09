# Simplify shape

Simplify a shape consisting of polygons or lines. This can be useful for
shapes that are too detailed for visualization, especially along natural
borders such as coastlines and rivers. The number of coordinates is
reduced.

## Usage

``` r
simplify_shape(shp, fact = 0.1, keep.units = FALSE, keep.subunits = FALSE, ...)
```

## Arguments

- shp:

  an [`sf`](https://r-spatial.github.io/sf/reference/sf.html) or
  [`sfc`](https://r-spatial.github.io/sf/reference/sfc.html) object.

- fact:

  simplification factor, number between 0 and 1 (default is 0.1)

- keep.units:

  prevent small polygon features from disappearing at high
  simplification (default FALSE)

- keep.subunits:

  should multipart polygons be converted to singlepart polygons? This
  prevents small shapes from disappearing during simplification if
  keep.units = TRUE. Default FALSE

- ...:

  other arguments passed on to the underlying function
  [`ms_simplify`](http://andyteucher.ca/rmapshaper/reference/ms_simplify.md)
  (except for the arguments `input`, `keep`, `keep_shapes` and
  `explode`)

## Value

[`sf`](https://r-spatial.github.io/sf/reference/sf.html) object

## Details

This function is a wrapper of
[`ms_simplify`](http://andyteucher.ca/rmapshaper/reference/ms_simplify.md).
In addition, the data is preserved. Also
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) objects are
supported.

## Examples

``` r
if (FALSE) { # \dontrun{
if (require(tmap)) {
    data(World)

    # show different simplification factors
    tm1 <- qtm(simplify_shape(World, fact = 0.05), title="Simplify 0.05")
    tm2 <- qtm(simplify_shape(World, fact = 0.1), title="Simplify 0.1")
    tm3 <- qtm(simplify_shape(World, fact = 0.2), title="Simplify 0.2")
    tm4 <- qtm(simplify_shape(World, fact = 0.5), title="Simplify 0.5")
    tmap_arrange(tm1, tm2, tm3, tm4)

    # show different options for keeping smaller (sub)units
    tm5 <- qtm(simplify_shape(World, keep.units = TRUE, keep.subunits = TRUE),
        title="Keep units and subunits")
    tm6 <- qtm(simplify_shape(World, keep.units = TRUE, keep.subunits = FALSE),
        title="Keep units, ignore small subunits")
    tm7 <- qtm(simplify_shape(World, keep.units = FALSE),
        title="Ignore small units and subunits")
    tmap_arrange(tm5, tm6, tm7)
}
} # }
```
