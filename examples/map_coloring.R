if (require(tmap) && require(cols4all)) {
    data(World)

    ## using cols4all directly
    indices <- map_coloring(World)
    pal <- c4a("brewer.set2", n = max(indices))
    World$color = pal[indices]
    tm_shape(World) +
        tm_polygons("color", fill.scale = tm_scale_asis()) +
        tm_crs("auto")

    # using map_coloring via "MAP_COLORS" in tmap
    tm_shape(World) +
        tm_polygons("MAP_COLORS", tm_scale(values = "brewer.set2")) +
        tm_crs("auto")

    # other example
    data(NLD_prov, NLD_muni)
    tm_shape(NLD_prov) +
    	tm_fill("name",
    	        fill.legend = tm_legend_hide()) +
    tm_shape(NLD_muni) +
    	tm_polygons("MAP_COLORS",
    	            fill_alpha = .25,
    	            fill.scale = tm_scale(values = "brewer.greys")) +
    tm_shape(NLD_prov) +
    	tm_borders(lwd=2) +
    	tm_text("name", options = opt_tm_text(shadow = TRUE)) +
    tm_title("Dutch provinces and\nmunicipalities", bg.color="white")
}

