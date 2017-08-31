#' Summarize time needed to run resolve_taxonrank
#'
#' Calculate time required and identify species to reslove even before running resolve_taxonrank.
#'
#' This function calculates the time required to run resolve_taxonrank in a specific dataset and gives a summary of
#' the names to resolve and how important the resolving the names is. Run this prior to resolve_taxonrank and decide
#' whether the simple workflow or comprehensive workflow to run.
#' @export
#' @param GBIF_Data Dataframe from GBIF or object of type gbif.
#' @seealso [resolve_taxonrank]
#' @return A list with multiple outputs and texts.
resolve_taxon_inspect <- function(GBIF_Data,
                                  ...) {
    t <- Sys.time()

    GBIF_Data <- format_checking(
        GBIF_Data,
        c(
            "taxonRank",
            "scientificName",
            "decimalLatitude",
            "decimalLongitude"
        )
    )

    # --------- Subsetting unresolved Data -----------#
    otherRankData <-
        GBIF_Data[GBIF_Data$taxonRank != "SPECIES" &
                      GBIF_Data$taxonRank != "SUBSPECIES"
                  & GBIF_Data$taxonRank != "VARIETY",]

    # --------- End of Subsetting unresolved Data -----------#

    if (dim(otherRankData)[1] == 0) {
        stop("No names to resolve")
    }

    # --------- Percentage Table -----------#

    names <-
        c(
            "Total unresolved data",
            "Unresolved genus data",
            "Unresolved family data",
            "Unresolved order data",
            "Unresolved class data "
        )

    count <-
        c(
            NROW(otherRankData),
            NROW(otherRankData[otherRankData$taxonRank == "GENUS",]),
            NROW(otherRankData[otherRankData$taxonRank == "FAMILY",]),
            NROW(otherRankData[otherRankData$taxonRank == "ORDER",]),
            NROW(otherRankData[otherRankData$taxonRank == "CLASS",])
        )

    percentageTable <- data.frame(count, row.names = names)

    total <- NROW(GBIF_Data)
    unresolved <- NROW(otherRankData)

    percentageTable$percentageOfTotalData <-
        round(percentageTable$count / total * 100, 2)
    percentageTable$percentageOfUnresolvedData <-
        round(percentageTable$count / unresolved * 100, 2)


    # --------- End of Percentage Table -----------#

    # --------- Names Table -----------#

    namesToResolve <-
        sort(table(otherRankData$scientificName), decreasing = T) # Sorting the names by frequency,
    # so that the names to be resolved first, are greater part of the complete data

    count <-
        as.vector(namesToResolve) # the count of records for each scientific names

    taxon <- sapply(names(namesToResolve),  function(name) {
        t <- otherRankData[otherRankData$scientificName == name,]
        t <- t[1,]
        t$taxonRank
    })

    namesPercentageTable <-
        data.frame(
            count,
            percentOfUnresolvedData = round(count / unresolved * 100, 2),
            currentTaxonRank = taxon,
            row.names = (names(namesToResolve))
        )

    # --------- End of Names Table -----------#

    string <-
        paste(
            "Your data has",
            dim(namesPercentageTable)[1],
            "names to resolve which will take roughly" ,
            round((dim(namesPercentageTable)[1] * 7) / 60, 2),
            "hours to finish on an average of 7 minutes per name. But it
            might vary depending on your internet connection speed and various other factors"
        )

    output <-
        list(
            estimation = string,
            percentageTable = percentageTable,
            namesPercentageTable = namesPercentageTable

        )

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(output)

    # End of Building the Answer

}
