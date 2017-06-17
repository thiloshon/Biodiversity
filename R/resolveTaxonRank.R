




resolveTaxonRank <- function(GBIF_Data, ...) {
    start <- Sys.time()
    require(spocc)
    require(taxize)
    require(rgbif)


    # --------- Subsetting unresolved Data -----------#
    otherRankData <-
        GBIF_Data[GBIF_Data$taxonRank != "SPECIES" &
                      GBIF_Data$taxonRank != "SUBSPECIES"
                  & GBIF_Data$taxonRank != "VARIETY",]

    # --------- End of Subsetting unresolved Data -----------#



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
        sort(table(otherRankData$scientificName), decreasing = T)



    count <- as.vector(namesToResolve)

    namesPercentageTable <-
        data.frame(
            count,
            percentOfUnresolvedData = round(count / unresolved * 100, 2),
            row.names = (names(namesToResolve))
        )



    # --------- End of Names Table -----------#


    # --------- Misspellings -----------#

    misspellingsData <- list()

    beforeSpellings <- Sys.time() - start

    print(beforeSpellings)
    print("Entering Misspellings Loop")

    misspellingsData <-
        sapply(names(namesToResolve)[1:3],  function(name) {
            namelookup <-
                rgbif::name_lookup(query = name)

            unique(namelookup$data$scientificName)

        })

    print("Leaving Misspellings Loop")
    afterSpellings <- Sys.time() - beforeSpellings

    print(afterSpellings)

    # --------- End of Misspellings -----------#


    # --------- Resolving -----------#

    #print("Entering Resolving Loop")
    answer <- sapply(names(namesToResolve)[1:3], function(name) {
        # Finding range for later use
        lat <-
            as.numeric(otherRankData[otherRankData$scientificName == name,]$decimalLatitude)
        lat <- lat[lat != 0]

        long <-
            as.numeric(otherRankData[otherRankData$scientificName == name,]$decimalLongitude)
        long <- long[long != 0]

        originLat <-
            range(lat, na.rm = T)
        originLong <-
            range(long, na.rm = T)

        # End of Finding range for later use

        # Stripping only the first two parts of scientific name
        name <-
            sub("^(\\S*\\s+\\S+).*", "\\1", (name)) # TODO fix tailing ',' problem



        # Finding probable species names
        gnrResults <- gnr_resolve(name)
        print(gnrResults)
        candidateList <- unique(gnrResults$matched_name)

        #print("Entering possibleSpecies Loop")

        possibleSpecies <-
            sapply(candidateList, function(candidate) {
                namelookup <-
                    tryCatch(
                        name_lookup(query = candidate),
                        error = function(e)
                            e
                    )  # filter using parameters

                unique(namelookup$data[namelookup$data$rank == "SPECIES", ]$scientificName)
            })


        #print("Leaving possibleSpecies Loop")


        possibleSpecies <-
            unique(as.vector(unlist(possibleSpecies)))

        possibleSpecies <-  possibleSpecies[!is.na(possibleSpecies)]

        # End of Finding probable species names


        # print("Entering ranges Loop")


        # Getting data for probable species
        ranges <-
            sapply(possibleSpecies, function(name) {
                #print(name)

                s <- Sys.time()



                key <- name_backbone(name = name)[1]$usageKey
                count <- occ_count(taxonKey = key)

                occspocc <-
                    occ(
                        query = name,
                        from = c( "vertnet"),
                        has_coords = T
                    )
                occspoccDF <- occ2df(occspocc)

                lat <-
                    range(as.numeric(occspoccDF$latitude) , na.rm = TRUE)
                long <-
                    range(as.numeric(occspoccDF$longitude) , na.rm = TRUE)

                t <-
                    data.frame(
                        latOriginHigh = originLat[1],
                        latOriginLow = originLat[2],
                        longOriginHigh = originLong[1],
                        longOriginLow = originLong[2],
                        lathigh = lat[1],
                        latLow = lat[2],
                        longhigh = long[1],
                        longLow = long[2],
                        otherSources = dim(occspoccDF)[1],
                        GBIFRecords = count,
                        time = Sys.time() - s
                    )

                #print(Sys.time() - s)

                t
            })
        #print("Leaving ranges Loop")

        # End of Getting data for probable species

        ranges <- as.data.frame(t(ranges))

        ranges <- as.data.frame(lapply(ranges, unlist))
        ranges <- ranges[order(ranges[, 9], decreasing = T ),]

        #print(str(ranges))
        #print(ranges)

        list(ranges)

    })



    print("Leaving Resolving Loop")

    #save(answer, file = "answer.RData")

    #print(answer)


    # Building the Answer

    ans <-
        list(
            percentageTable = percentageTable,
            namesPercentageTable = list(head(namesPercentageTable), tail(namesPercentageTable)),
            possibleMisspellings = misspellingsData,
            resolveResults = answer
        )

    afterResolve <- Sys.time() - afterSpellings

    print(afterResolve)

    #print(ans)

    ans

    # End of Building the Answer

}
