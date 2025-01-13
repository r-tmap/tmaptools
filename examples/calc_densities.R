if (require(tmap) && packageVersion("tmap") >= "3.99") {
data(NLD_muni)

NLD_muni_pop_per_km2 <- calc_densities(NLD_muni,
  target = "km km", var = c("population", "dwelling_total"))
NLD_muni <- sf::st_sf(data.frame(NLD_muni, NLD_muni_pop_per_km2))

tm_shape(NLD_muni) +
  tm_polygons(
    fill = c("population_km.2", "dwelling_total_km.2"),
    fill.legend =
      list(
        tm_legend(expression("Population per " * km^2)),
        tm_legend(expression("Dwellings per " * km^2)))) +
tm_facets(free.scales = TRUE) +
  tm_layout(panel.show = FALSE)
}
