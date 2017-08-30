#' Resolve coordinates
#'
#' Resolve coordinates which are not pointed at the correct country.
#'
#' @import tm
#' @param GBIF_Data Dataframe from GBIF or object of type gbif.
#' @seealso [resolve_taxon_inspect()]
#' @return The original dataframe with added columns.
resolveCoordinates <- function(GBIF_Data) {
    GBIF_Data$projectedLatitude <- NA
    GBIF_Data$projectedLongitude <- NA
    GBIF_Data$coordinatesProjected <- 0

    localities <-
        names(sort(table(GBIF_Data$locality), decreasing = T))

    stopwords = c(
        "CAPTIVE",
        "BRED",
        "Captive",
        "Bred",
        "captive",
        "bred",
        "-",
        "Locality Unknown",
        "NA",
        "BETWEEN"
    )


    localitiesClean <- removeWords(localities, stopwords)
    localitiesClean[nchar(localitiesClean) > 3] <-
        paste(localitiesClean[nchar(localitiesClean) > 3], "Australia", sep = ", ")
    print(length(localities))

    for (j in 1:400) {
        # length(localities)


        geoCode <- suppressMessages(geocode(localitiesClean[j]))
        print(paste(j, "is", localitiesClean[j], geoCode$lat, geoCode$lon))

        if (!is.na(geoCode)) {
            print(paste("changing records:", dim(GBIF_Data[GBIF_Data$locality == localities[j],][1])))

            GBIF_Data[GBIF_Data$locality == localities[j],]$projectedLatitude <-
                geoCode$lat
            GBIF_Data[GBIF_Data$locality == localities[j],]$projectedLongitude <-
                geoCode$lon
            GBIF_Data$coordinatesProjected <- 1
        }

    }

    GBIF_Data


}
