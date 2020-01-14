\dontrun{
if (require(tmap)) {
    data(metro)

    # sample five cities from metro dataset
    set.seed(1234)
    five_cities <- metro[sample(length(metro), 5), ]

    # obtain reverse geocode address information
    addresses <- rev_geocode_OSM(five_cities, zoom = 6)
    five_cities <- sf::st_sf(data.frame(five_cities, addresses))

    # change to interactive mode
    current.mode <- tmap_mode("view")
    tm_shape(five_cities) +
    	tm_markers(text="name")

    # restore current mode
    tmap_mode(current.mode)
}
}
