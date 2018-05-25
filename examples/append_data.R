\dontrun{
if (require(tmap)) {
    data(World)

    f <- tempfile()
    download.file("http://kejser.org/wp-content/uploads/2014/06/Country.csv", destfile = f)
    domain_codes <- read.table(f, header=TRUE, sep="|")
    unlink(f)

    domain_codes <- subset(domain_codes, select = c("Alpha3Code", "TopLevelDomain"))
    domain_codes$Alpha3Code <- toupper(domain_codes$Alpha3Code)

    World <- append_data(World, domain_codes, key.shp = "iso_a3", key.data = "Alpha3Code",
    					 ignore.na = TRUE)

    # codes in the data, but not in Europe:
    oc <- over_coverage()
    oc$value

    # Countries without appended data:
    uc <- under_coverage()

    current_mode <- tmap_mode("view")
    qtm(World[uc$id,], text="name")

    # plot the result
    qtm(World, text="TopLevelDomain")
    tmap_mode(current_mode)
}
}
