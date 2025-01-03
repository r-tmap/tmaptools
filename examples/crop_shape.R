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
