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


N <- as(tmap::NLD_prov, "sf")


approx_distan



approx_areas(World)
approx_areas(W)

st_area(W)


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




# --- preprocess_shapes
# set_projection (set only, and transform)
# approx_areas
# is_projected


# --- process_shapes
# split
# crop
