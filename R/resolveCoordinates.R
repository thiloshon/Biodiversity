#' Resolve coordinates
#'
#' Resolve coordinates which are not pointed at the correct country.
#'
#' @import tm
#' @param gbif_data Dataframe from GBIF or object of type gbif.
#' @seealso [resolve_taxon_inspect()]
#' @return The original dataframe with added columns.
resolveCoordinates <- function(gbif_data) {

    gbif_data <- format_checking(gbif_data,
    c("locality"))

    gbif_data$projectedLatitude <- NA
    gbif_data$projectedLongitude <- NA
    gbif_data$coordinatesProjected <- 0

    localities <-
        names(sort(table(gbif_data$locality), decreasing = T))

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
    # print(length(localities))

    for (j in 1:400) {
        # length(localities)


        geoCode <- suppressMessages(geocode(localitiesClean[j]))
        # print(paste(j, "is", localitiesClean[j], geoCode$lat, geoCode$lon))

        if (!is.na(geoCode)) {
            # print(paste("changing records:", dim(gbif_data[gbif_data$locality == localities[j],][1])))

            gbif_data[gbif_data$locality == localities[j],]$projectedLatitude <-
                geoCode$lat
            gbif_data[gbif_data$locality == localities[j],]$projectedLongitude <-
                geoCode$lon
            gbif_data$coordinatesProjected <- 1
        }

    }

    gbif_data


}
