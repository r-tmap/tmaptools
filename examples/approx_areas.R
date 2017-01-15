if (require(tmap)) {
    data(NLD_muni)

    NLD_muni$area <- approx_areas(NLD_muni, total.area = 33893)

    tm_shape(NLD_muni) +
        tm_bubbles(size="area", title.size=expression("Area in " * km^2))


    # function that returns min, max, mean and sum of area values
    summary_areas <- function(x) {
        list(min_area=min(x),
             max_area=max(x),
             mean_area=mean(x),
             sum_area=sum(x),
             units=paste0(attr(x, "unit")))
    }

    # area of the polygons
    approx_areas(NLD_muni) %>% summary_areas()

    # area of the polygons, adjusted corrected for a specified total area size
    approx_areas(NLD_muni, total.area=33893) %>% summary_areas()

    # proportional area of the polygons
    approx_areas(NLD_muni, target = "prop") %>% summary_areas()

    # area in squared miles
    approx_areas(NLD_muni, target = "mi") %>% summary_areas()

    # area of the polygons when unprojected
    approx_areas(NLD_muni %>% set_projection(projection="longlat")) %>% summary_areas()
}
