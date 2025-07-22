if (require(tmap)) {

    ## load shapes
    data(NLD_muni)
    data(World)

    ## get bounding box (similar to sp's function bbox)
    bb(NLD_muni)

    ## extent it by factor 1.10
    bb(NLD_muni, ext=1.10)

    ## double the width
    bb(NLD_muni, width=2, relative = TRUE)

    ## crop both dimensions from 0.25 to 0.75
    bb(NLD_muni, xlim=c(.25, .75), ylim=c(.25, .75), relative = TRUE)

    ## extent it such that aspect ratio is 1
    bb(NLD_muni, asp.target = 1)

    ## convert to longlat (EPSG 4326)
    bb(NLD_muni, projection=4326)
}

\dontrun{
if (require(tmap)) {
    bb("Limburg", projection = 28992)
    bb_italy <- bb("Italy", projection = "+proj=eck4")

    tm_shape(World, bbox=bb_italy) + tm_polygons()
    # shorter alternative: tm_shape(World, bbox="Italy") + tm_polygons()
}}
