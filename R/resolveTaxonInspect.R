resolveTaxonInspect <- function(GBIF_Data,
                                gnr_score = 0.75,
                                ...) {
    t <- Sys.time()
    require(spocc)
    require(taxize)
    require(rgbif)
    require(rworldmap)

    if (class(GBIF_Data) == "dwca_gbif") {
        GBIF_Data <- GBIF_Data$data$occurrence.txt
    } else if (class(GBIF_Data) == "data.frame") {
        # Nothing to do
    } else {
        stop("Incorrect input type")
    }



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

    taxon <- sapply(names(namesToResolve),  function(name){
        t <- otherRankData[otherRankData$scientificName==name,]
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
            round((dim(
                namesPercentageTable
            )[1] * 7) / 60, 2),
            "hours to finish on an average of 7 minutes per name.
            But it might vary depending on your internet connection speed and various other factors"
        )

    output <-
        list(
            estimation = string,
            percentageTable = percentageTable,
            namesPercentageTable = namesPercentageTable

        )

    print(Sys.time() - t)
    output

    # End of Building the Answer

}
