if (require(tmap)) {
    data(land, World)

    current.mode <- tmap_mode("view")

    land %>%
        crop_shape(World[World$name=="China",], polygon = TRUE) %>%
        tm_shape() +
        tm_raster("cover_cls")

    tmap_mode(current.mode)
}
