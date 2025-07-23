if (require(tmap)) {
    data(NLD_muni)

    NLD_muni$area <- approx_areas(NLD_muni, total.area = 33893)

    tm_shape(NLD_muni) +
        tm_bubbles(size="area",
                   size.legend = tm_legend(title = expression("Area in " * km^2)))


    # function that returns min, max, mean and sum of area values
    summary_areas <- function(x) {
        list(min_area=min(x),
             max_area=max(x),
             mean_area=mean(x),
             sum_area=sum(x))
    }

    # area of the polygons
    summary_areas(approx_areas(NLD_muni))

    # area of the polygons, adjusted corrected for a specified total area size
    summary_areas(approx_areas(NLD_muni, total.area=33893))

    # proportional area of the polygons
    summary_areas(approx_areas(NLD_muni, target = "prop"))

    # area in squared miles
    summary_areas(approx_areas(NLD_muni, target = "mi mi"))

    # area of the polygons when unprojected
    summary_areas(approx_areas(sf::st_transform(NLD_muni, crs = 4326)))
}
