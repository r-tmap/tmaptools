if (require(tmap)) {
    data(NLD_muni, land, metro)

    land_NLD <- crop_shape(land, NLD_muni)

    qtm(land_NLD, raster="trees", style="natural")

    metro_NLD <- crop_shape(metro, NLD_muni, polygon = TRUE)

    qtm(NLD_muni) +
    tm_shape(metro_NLD) +
    	tm_bubbles("pop2010", col="red", title.size="European cities") +
    	tm_legend(frame=TRUE)
}
