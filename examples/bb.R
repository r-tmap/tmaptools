if (require(tmap) && packageVersion("tmap") >= "2.0") {

    ## load shapes
    data(NLD_muni)
    data(World)

    ## get bounding box (similar to sp's function bbox)
    bb(NLD_muni)

    ## extent it by factor 1.10
    bb(NLD_muni, ext=1.10)

    ## convert to longlat
    bb(NLD_muni, projection=4326)

    ## change existing bounding box
    bb(NLD_muni, ext=1.5)
    bb(NLD_muni, width=2, relative = TRUE)
    bb(NLD_muni, xlim=c(.25, .75), ylim=c(.25, .75), relative = TRUE)

}

\dontrun{
if (require(tmap)) {
    bb("Limburg", projection = "rd")
    bb_italy <- bb("Italy", projection = "eck4")

    tm_shape(World, bbox=bb_italy) + tm_polygons()
    # shorter alternative: tm_shape(World, bbox="Italy") + tm_polygons()
}}
