if (require(tmap) && packageVersion("tmap") >= "2.0") {
    data(World)

    get_asp_ratio(World)

    get_asp_ratio(bb(World))

    tm <- qtm(World)
    get_asp_ratio(tm)
}

\dontrun{
    get_asp_ratio("Germany") #note: bb("Germany") uses geocode_OSM("Germany")
}
