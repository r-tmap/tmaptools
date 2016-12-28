if (require(tmap)) {
    geocode_OSM("India")
    geocode_OSM("CBS Weg 1, Heerlen")
    geocode_OSM("CBS Weg 1, Heerlen", projection = "rd")

    \dontrun{
    data(metro)

    # sample 5 cities from the metro dataset
    five_cities <- metro[sample(length(metro), 5), ]

    # obtain geocode locations from their long names
    five_cities_geocode <- geocode_OSM(five_cities$name_long)
    sp::coordinates(five_cities_geocode) <- ~lon+lat

    # change to interactive mode
    current.mode <- tmap_mode("view")

    # plot metro coordinates in red and geocode coordinates in blue
    # zoom in to see the differences
    tm_shape(five_cities) +
    	tm_dots(col = "blue") +
    tm_shape(five_cities_geocode) +
    	tm_dots(col = "red")

    # restore current mode
    tmap_mode(current.mode)
    }
}
