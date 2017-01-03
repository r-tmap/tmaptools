\dontrun{
if (require(tmap)) {
    data(World, land)
    projection_units(World, target = "miles")

    projection_units(land)
    projection_units(land, latitude = 45)
    projection_units(land, latitude = 89)
}
}
