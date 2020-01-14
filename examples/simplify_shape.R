\dontrun{
if (require(tmap)) {
    data(World)

    # show different simplification factors
    tm1 <- qtm(World %>% simplify_shape(fact = 0.05), title="Simplify 0.05")
    tm2 <- qtm(World %>% simplify_shape(fact = 0.1), title="Simplify 0.1")
    tm3 <- qtm(World %>% simplify_shape(fact = 0.2), title="Simplify 0.2")
    tm4 <- qtm(World %>% simplify_shape(fact = 0.5), title="Simplify 0.5")
    tmap_arrange(tm1, tm2, tm3, tm4)

    # show different options for keeping smaller (sub)units
    tm5 <- qtm(World %>% simplify_shape(keep.units = TRUE, keep.subunits = TRUE),
        title="Keep units and subunits")
    tm6 <- qtm(World %>% simplify_shape(keep.units = TRUE, keep.subunits = FALSE),
        title="Keep units, ignore small subunits")
    tm7 <- qtm(World %>% simplify_shape(keep.units = FALSE),
        title="Ignore small units and subunits")
    tmap_arrange(tm5, tm6, tm7)
}
}

