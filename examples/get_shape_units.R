if (require(tmap)) {
    data(World, land)
    get_shape_units(World, target.unit = "miles")

    get_shape_units(land)
    get_shape_units(land, latitude = 45)
    get_shape_units(land, latitude = 89)
}
