if (require(tmap) && packageVersion("tmap") >= "3.99") {
    data(World, metro)

    World$color <- map_coloring(World, palette="Pastel2")
    qtm(World, fill = "color")

    # map_coloring used indirectly: qtm(World, fill = "MAP_COLORS")

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

