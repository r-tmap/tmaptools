if (require(tmap)) {
    data(NLD_muni)

    NLD_muni_pop_per_km2 <- calc_densities(NLD_muni,
        target = "km km", var = c("pop_men", "pop_women"))
    NLD_muni <- append_data(NLD_muni, NLD_muni_pop_per_km2, fixed=TRUE)

    tm_shape(NLD_muni) +
    	tm_polygons(c("pop_women_km^2", "pop_women_km^2"),
            title=expression("Population per " * km^2), style="quantile") +
    tm_facets(free.scales = FALSE) +
    tm_layout(panel.show = TRUE, panel.labels=c("Men", "Women"))
}
