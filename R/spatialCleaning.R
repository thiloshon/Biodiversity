coordinatesDecimalMismatch <- function(GBIF_DataFrame) {
    t <- Sys.time()
    lat <- sapply(GBIF_DataFrame$decimalLatitude, function(lat) {
        list <- strsplit(sub('0+$', '', as.character.numeric_version(lat)),
                         ".", fixed = TRUE)

        if (length(list[[1]]) < 2) {
            return (0)
        } else {
            return (nchar(list[[1]][[2]]))
        }
    })

    long <- sapply(GBIF_DataFrame$decimalLongitude, function(long) {
        list <- strsplit(sub('0+$', '', as.character.numeric_version(long)),
                         ".", fixed = TRUE)
        if (length(list[[1]]) < 2) {
            return (0)
        } else {
            return (nchar(list[[1]][[2]]))
        }
    })

    print(Sys.time() - t)

    GBIF_DataFrame$decimalPointDifference <- abs(lat - long)
    return(GBIF_DataFrame)
}
