library(stars)
library(OpenStreetMap)

# read osm data
x <- openmap(upperLeft = c(53.699950, 3.046742), lowerRight = c(50.590457, 7.448777), type = "osm")

# get ingredients from x (ncols, nrows, a, bbx)
ncols <- x$tiles[[1]]$yres # not sure why OpenStreetMap has swapped y and x...
nrows <- x$tiles[[1]]$xres

rgb_values <- col2rgb(x$tiles[[1]]$colorData)
a <- array(t(rgb_values), dim = c(x = ncols, y = nrows, band = 3))

bbx_coor <- unname(unlist(x$bbox))[c(1,3,4,2)]
names(bbx_coor) <- c("xmin", "xmax", "ymin", "ymax")

crs <- st_crs(x$tiles[[1]]$projection)

bbx <- st_bbox(bbx_coor, crs = crs)


# methode 1: via raster package
x2 <- raster::raster(x)
x3 <- st_as_stars(x2)

st_dimensions(x3)

plot(x3)


# methode 2: array method
x4 <- st_as_stars(a)

plot(x4)

st_dimensions(x4)

x5 <- x4 %>%
    st_set_crs(crs) %>%
    st_set_dimensions("x",
                      offset = bbx["xmin"],
                      delta = (bbx["xmax"] - bbx["xmin"]) / ncols) %>%
    st_set_dimensions("y",
                      offset = bbx["ymin"],
                      delta = (bbx["ymin"] - bbx["ymax"]) / nrows)





attr(x4, "dimensions")[[2]]$delta <- -1

plot(x4)
# is there an easy way to add the bbox and crs to x4?

# methode 3: bbox method
x5 <- st_as_stars(bbx, nx = ncols, ny = nrows, values = a[,,1])
plot(x5)

x6 <- st_as_stars(bbx, nx = ncols, ny = nrows, values = a)
# it only takes the first 799*920 slice
# how to add values for multiple bands?

# are there other methods to convert x to a stars object?
