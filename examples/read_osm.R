\dontrun{
if (require(tmap)) {
    #### Choropleth with OSM background

    # load Netherlands shape
    data(NLD_muni)

    # read OSM raster data
    osm_NLD <- read_osm(NLD_muni, ext=1.1)

    # plot with regular tmap functions
    tm_shape(osm_NLD) +
    	tm_rgb() +
    tm_shape(NLD_muni) +
    	tm_polygons("population", convert2density=TRUE, style="kmeans", alpha=.7, palette="Purples")

    #### A close look at the building of Statistics Netherlands in Heerlen

    # create a bounding box around the CBS (Statistics Netherlands) building
    CBS_bb <- bb("CBS Weg 11, Heerlen", width=.003, height=.002)

    # read Microsoft Bing satellite and OpenCycleMap OSM layers
    CBS_osm1 <- read_osm(CBS_bb, type="bing")
    CBS_osm2 <- read_osm(CBS_bb, type="opencyclemap")

    # plot OSM raster data
    qtm(CBS_osm1)
    qtm(CBS_osm2)

}
}
