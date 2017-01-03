\dontrun{
if (require(tmap)) {
    data(Europe)

    current.mode <- tmap_mode("view")
    qtm(bb_sp(Europe))

    # restore mode
    tmap_mode(current.mode)
}
}
