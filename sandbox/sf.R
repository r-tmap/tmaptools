library(sf)
library(tmaptools)
devtools::load_all("../tmap")


data(World, rivers, metro, land)

World@proj4string <- sp::CRS()


W <- as(World, "sf")
r <- as(rivers, "sf")
m <- as(metro, "sf")
landr <- as(land, "RasterBrick")


qtm(World)

sf::st_proj_info(World)

W2 <- st_transform(World, crs=get_proj4("robin"))

W2$geometry

st_crs(World)


st_crs(W2)



# --- preprocess_shapes
# set_projection (set only, and transform)
# approx_areas
# is_projected


# --- process_shapes
# split
# crop
