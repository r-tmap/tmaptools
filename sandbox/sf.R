library(sf)
library(sp)
library(raster)
library(mapview)

devtools::load_all(".")
devtools::load_all("../tmap")


data(World, rivers, metro, land, NLD_prov)

W <- as(World, "sf")
r <- as(rivers, "sf")
m <- as(metro, "sf")
landr <- as(land, "RasterBrick")




approx_distan



approx_areas(World)
approx_areas(W)

st_area(W)


x <- st_graticule(N)
qtm(x)


x <- st_graticule(crs=st_crs(get_proj4("rds")))

mapview(x)

plot(x)


qtm(World)

sf::st_proj_info(World)

W2 <- st_transform(World, crs=get_proj4("robin"))

W2$geometry

st_crs(World)


st_crs(W2)

bb = st_bbox(W)
bbox = st_linestring(rbind(c( bb[1],bb[2]),c( bb[3],bb[2]),
                           c( bb[3],bb[4]),c( bb[1],bb[4]),c( bb[1],bb[2])))

g = st_graticule(st_bbox(W), crs=st_crs(W))

a = st_sf(a = 1:2, geom = st_sfc(st_point(0:1), st_point(1:2)), crs = 4326)
st_bbox(a)
st_as_sfc(st_bbox(a))

bbn <- bb(N, ext = .5)

st_as_sfc(bbn)


    st_bbox()

st_intersection(nc, st_set_crs(st_as_sf(as(raster::extent(-82, -80, 35, 36), "SpatialPolygons")), st_crs(nc)))


N <- as(tmap::NLD_prov, "sf")

bbx <- bb(N)

bb_to_sfc <- function(bbx) {
    m <- matrix(c(bbx[1], bbx[3], bbx[3], bbx[1], bbx[1],
                  bbx[2], bbx[2], bbx[4], bbx[4], bbx[2]), ncol=2)
    st_sfc(st_polygon(list(m)), crs = st_crs(bbx))
}


qtm(create_sf_rect(bbx))

st_coordinates(create_sf_rect(bbx))

st_polygon(list(matrix(c(1,2,3,4), ncol=2)))


# --- preprocess_shapes
# set_projection (set only, and transform)
# approx_areas
# is_projected


# --- process_shapes
# split
# crop


land_europe <- crop_shape(land, Europe)
qtm(land_europe)

