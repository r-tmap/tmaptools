load("~/Dropbox/projects/tmap_extra/data/crimes.rdata")
devtools::load_all("../tmap")

crime_densities <- smooth_map(crimes_london, bandwidth = 0.5, cover = london)

crime_densities <- smooth_map(crimes_london, bandwidth = 0.5, breaks = c(0, 50, 100, 250, 500, 1000), cover = london)

library(tmap)

qtm(crime_densities$polygons)
