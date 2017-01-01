if (require(tmap)) {
    data(NLD_prov)

    # North-South and East-West distances of the Netherlands
    approx_distances(NLD_prov)

    # Distance between Maastricht and Groningen
    p_maastricht <- geocode_OSM("Maastricht")$coords
    p_groningen <- geocode_OSM("Groningen")$coords
    approx_distances(p_maastricht, p_groningen)

    # Check distances in several projections
    sapply(c("wintri", "utm31", "laea_Eur", "rd", "longlat"), function(projection) {
        p_maastricht <- geocode_OSM("Maastricht", projection = projection)$coords
        p_groningen <- geocode_OSM("Groningen", projection = projection)$coords
        approx_distances(p_maastricht, p_groningen, projection = projection)
    })
}
