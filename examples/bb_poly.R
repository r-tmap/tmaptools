if (require(tmap)) {
    data(Europe)

    current.mode <- tmap_mode("view")
    qtm(bb_poly(Europe))

    # restore mode
    tmap_mode(current.mode)
}

