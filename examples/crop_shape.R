if (require(tmap) && packageVersion("tmap") >= "2.0") {
    data(World, NLD_muni, land, metro)

    land_NLD <- crop_shape(land, NLD_muni)

    qtm(land_NLD, raster="trees", style="natural")

    metro_Europe <- crop_shape(metro, World[World$continent == "Europe", ], polygon = TRUE)

    qtm(World) +
    tm_shape(metro_Europe) +
    	tm_bubbles("pop2010", col="red", title.size="European cities") +
    	tm_legend(frame=TRUE)
}
