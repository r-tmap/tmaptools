library(devtools)
dev_mode()
load_all(".")
load_all("../tmap")

#library(tmap)
library(spData)
library(sf)
library(lwgeom)

data(World)

#w2 = sf::st_transform(world, crs = "+proj=wintri")
w3 = lwgeom::st_transform_proj(world, crs = "+proj=wintri")
#w4 = lwgeom::st_transform_proj(world, crs = "+proj=merc")
w5 = sf::st_transform(world, crs = "+proj=merc")

w6 = sf::st_transform(world, crs = 4326)
w7 = lwgeom::st_transform_proj(world, crs = 4326)


w8 <- st_transform2(world, crs = "+proj=wintri")
w9 <- st_transform2(world, crs = "+proj=merc")

w10 <- st_transform2(w8, crs = 4326)


tm_shape(World, projection="merc", ylim=c(.1, 1), relative = TRUE) +
    tm_polygons("economy")

set_projection(World, projection = "merc")
