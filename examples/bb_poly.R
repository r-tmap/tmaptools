if (require(tmap) && packageVersion("tmap") >= "2.0") {
    data(NLD_muni)

    current.mode <- tmap_mode("view")
    qtm(bb_poly(NLD_muni))

    # restore mode
    tmap_mode(current.mode)
}
