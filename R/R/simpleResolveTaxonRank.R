simpleResolveTaxonRank <- function(GBIF_Data,
                                   gnr_score = 0.75,
                                   resolve = dim(GBIF_Data)[1],
                                   ...) {
    t <- Sys.time()
    require(spocc)
    require(taxize)
    require(rgbif)
    require(rworldmap)

    if (class(GBIF_Data) == "dwca_gbif") {
        #print("in one")
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
                  & GBIF_Data$taxonRank != "VARIETY", ]

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
            NROW(otherRankData[otherRankData$taxonRank == "GENUS", ]),
            NROW(otherRankData[otherRankData$taxonRank == "FAMILY", ]),
            NROW(otherRankData[otherRankData$taxonRank == "ORDER", ]),
            NROW(otherRankData[otherRankData$taxonRank == "CLASS", ])
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

    if (resolve == dim(GBIF_Data)[1]) {
        resolve <- length(count)
    }

    print(resolve)

    namesPercentageTable <-
        data.frame(
            count,
            percentOfUnresolvedData = round(count / unresolved * 100, 2),
            row.names = (names(namesToResolve))
        )


    # --------- End of Names Table -----------#


    # --------- Misspellings -----------#

    misspellingsData <-
        sapply(names(namesToResolve)[1:resolve],  function(name) {
            namelookup <- tryCatch(
                # this error catch is because name_lookup throws error and halts
                # program when unknown query is provided
                rgbif::name_lookup(query = name),
                error = function(e)
                    e
            )

            unique(namelookup$data$scientificName)

        })

    # --------- End of Misspellings -----------#
    #print(Sys.time() - t)

    # --------- Resolving -----------#

    answer <-
        sapply(names(namesToResolve)[1:resolve], simplify = FALSE, USE.NAMES = TRUE,  function(nameToResolve) {
            s <- Sys.time()


            # Stripping only the first two parts of scientific name. This is because,
            # gnr_resolve works well only when giving first 2 parts

            originName <- nameToResolve

            nameFirstPart <-
                unlist(strsplit(originName, split = ' ', fixed = TRUE))[1]

            similarNamesOfOrigin <- tryCatch(
                # this error catch is because name_lookup throws error and halts
                # program when 'Error in `$<-.data.frame`(`*tmp*`, "taxonrank", value = character(0)) :
                # replacement has 0 rows, data has 1 '
                suppressMessages(sci2comm(nameFirstPart)),
                error = function(e)
                    e
            )  # TODO: filter using parameters

            similarNamesOfOrigin <- similarNamesOfOrigin[[1]]

            #print(class(similarNamesOfOrigin))


            remarks <-
                otherRankData[otherRankData$scientificName == originName, ]
            remarks <- remarks$occurrenceRemarks
            remarks <- remarks[remarks != ""]
            remarks <-
                remarks[remarks != "occurrenceRemarks withheld"]
            remarks <- names(summary(as.factor(remarks)))

            #print(class(remarks))

            nameToResolve <-
                sub("^(\\S*\\s+\\S+).*", "\\1", (nameToResolve)) # TODO fix tailing ',' problem

            k <- Sys.time()


            # Finding probable species names
            gnrResults <- gnr_resolve(nameToResolve)
            gnrResults <-
                gnrResults[gnrResults$score >= gnr_score,]
            candidateList <- unique(gnrResults$matched_name)

            possibleSpecies <-
                lapply(candidateList, function(candidate) {
                    namelookup <-
                        tryCatch(
                            # this error catch is because name_lookup throws error and halts
                            # program when unknown query is provided
                            name_lookup(query = candidate),
                            error = function(e)
                                e
                        )  # TODO: filter using parameters
                    dat <-
                        namelookup$data[namelookup$data$rank == "SPECIES",]

                    dat <-
                        sub("^(\\S*\\s+\\S+).*", "\\1", (dat$scientificName))


                    unique(dat)
                })




            possibleSpecies <-
                unique(as.vector(unlist(possibleSpecies)))

            possibleSpecies <-
                possibleSpecies[!is.na(possibleSpecies)] # sometimes name_lookup gives NA as a possible name

            # End of Finding probable species names


            #print(remarks)
            #print(similarNamesOfOrigin)



            # Getting data from other sources for probable species
            ranges <-
                sapply(possibleSpecies, function(name) {
                    #print(name)
                    key <-
                        name_backbone(name = name)[1]$usageKey
                    count <- occ_count(taxonKey = key)

                    similarNames <- tryCatch(
                        # this error catch is because name_lookup throws error and halts
                        # program when 'Error in `$<-.data.frame`(`*tmp*`, "taxonrank", value = character(0)) :
                        # replacement has 0 rows, data has 1 '
                        suppressMessages(sci2comm(name)),
                        error = function(e)
                            e
                    )  # TODO: filter using parameters



                    similarNames <- similarNames[[1]]
                    list(similarNames)
                })

            #print((ranges))

            k <- list(
                RemarksOnOriginalData = remarks,
                CommonNamesOfOriginalData = similarNamesOfOrigin,
                CommonNamesOfSpecies = ranges
            )


            k

        })

    # Building the Answer

    #print(str(answer))

    output <-
        list(
            percentageTable = percentageTable,
            namesPercentageTable = namesPercentageTable,
            possibleMisspellings = misspellingsData,
            resolveResults = answer
        )

    print(Sys.time() - t)
    output

    # End of Building the Answer
}
