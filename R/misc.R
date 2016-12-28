# conversion vector (for now assume metric and imperial are in km and mi, check later)
to_m <- c(m=1, km=1000, mi=1609.344, miles=1609.344, ft=0.304800609601219, 'us-ft'=0.304800609601219, metric=1000, imperial=1609.344,NA)


convert_shape_units <- function(orig.unit, target.unit) {
    unit.size <- to_m[target.unit] / to_m[orig.unit]
    list(orig=orig.unit,
         target=target.unit,
         to=unname(unit.size))
}

