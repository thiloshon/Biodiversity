#' Resolve taxonranks
#'
#' Resolve taxonranks which are not identified at the Species, Subspecies or Variety rank.
#'
#' This function resolves the records which are not identified at the specie, subscpecie or variety level.
#' It checks for similar records from other sources and tries to build series of world maps with original amnd suggested species distributions.
#' The simple version tries to do the same but by taking synonyms and other names into account
#' @export
#' @import spocc taxize rgbif rworldmap ggmap
#' @param GBIF_Data Dataframe from GBIF or object of type gbif.
#' @param gnr_resolve The score to consider as qualifier in the gnr_resolve internal function
#' @param tidy_output When the output is very long, whether to cut off unimportant parts
#' @param resolve The amount of names to resolve. Default is all. If number of names is high, can use this.
#' @param location The location to consider when plotting maps for the suggested names
#' @param zoom The zoom value to consider when plotting the worldmap centered at the location parameter. 4 means city and so on...
#' @param simplify Which workflow to follow. simplyfied workflow runs faster than the other but is less accurate.
#' @seealso [resolve_taxon_inspect()]
#' @return A list with multiple outputs.
resolve_taxonrank <- function(GBIF_Data,
                              gnr_score = 0.75,
                              tidy_output = TRUE,
                              resolve = dim(GBIF_Data)[1],
                              location = "Australia",
                              zoom = 4,
                              simplify = FALSE,
                              ...) {
    t <- Sys.time()

    if (class(GBIF_Data) == "dwca_gbif") {
        GBIF_Data <- GBIF_Data$data$occurrence.txt
    } else if (class(GBIF_Data) != "data.frame") {
        stop("Incorrect input type")
    }

    # --------- Subsetting unresolved Data -----------#
    otherRankData <-
        GBIF_Data[GBIF_Data$taxonRank != "SPECIES" &
                      GBIF_Data$taxonRank != "SUBSPECIES"
                  &
                      GBIF_Data$taxonRank != "VARIETY", ]

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

    namesPercentageTable <-
        data.frame(
            count,
            percentOfUnresolvedData = round(count / unresolved * 100, 2),
            row.names = (names(namesToResolve))
        )


    # --------- End of Names Table -----------#


    # --------- Misspellings -----------#

    misspellingsData <-
        sapply(names(namesToResolve)[1:resolve], function(name) {
            namelookup <- tryCatch(
                # this error catch is because name_lookup throws error and halts
                # program when unknown query is provided
                rgbif::name_lookup(query = name),
                error = function(e)
                    e
            )

            return(unique(namelookup$data$scientificName))
        })

    # --------- End of Misspellings -----------#


    # --------- Resolving -----------#

    if (simplify == FALSE) {
        answer <-
            sapply(names(namesToResolve)[1:resolve], function(nameToResolve) {
                s <- Sys.time()

                # Finding range for later use
                lat <-
                    as.numeric(otherRankData[otherRankData$scientificName == nameToResolve, ]$decimalLatitude)
                lat <-
                    lat[lat != 0]  # some records have 0.0000 as the NA equivalent. So its not the
                # actual 0.00 coordinate value but representation of missing value

                long <-
                    as.numeric(otherRankData[otherRankData$scientificName == nameToResolve, ]$decimalLongitude)
                long <-
                    long[long != 0]# some records have 0.0000 as the NA equivalent. So its not the
                # actual 0.00 coordinate value but representation of missing value

                df <- data.frame(long, lat)

                originLat <-
                    range(lat, na.rm = T)
                originLong <-
                    range(long, na.rm = T)

                # End of Finding range for later use

                # Stripping only the first two parts of scientific name. This is because,
                # gnr_resolve works well only when giving first 2 parts
                originName <- nameToResolve
                nameToResolve <-
                    sub("^(\\S*\\s+\\S+).*", "\\1", (nameToResolve)) # TODO fix tailing ',' problem


                # Finding probable species names
                gnrResults <- gnr_resolve(nameToResolve)
                gnrResults <-
                    gnrResults[gnrResults$score >= gnr_score, ]
                candidateList <- unique(gnrResults$matched_name)

                possibleSpecies <-
                    sapply(candidateList, function(candidate) {
                        namelookup <-
                            tryCatch(
                                # this error catch is because name_lookup throws error and halts
                                # program when unknown query is provided
                                name_lookup(query = candidate),
                                error = function(e)
                                    e
                            )  # TODO: filter using parameters
                        dat <-
                            namelookup$data[namelookup$data$rank == "SPECIES" |
                                                namelookup$data$rank == "SUBSPECIES", ]

                        unique(dat$scientificName)
                    })

                possibleSpecies <-
                    unique(as.vector(unlist(possibleSpecies)))

                possibleSpecies <-
                    possibleSpecies[!is.na(possibleSpecies)] # sometimes name_lookup gives NA as a possible name

                # --------- End of Finding probable species names

                # --------- Plotting the origin data

                latLonCenter <-
                    suppressMessages(geocode(location))

                newmap <- suppressMessages(get_map(
                    location = c(lon = latLonCenter$lon, lat = latLonCenter$lat),
                    zoom = zoom
                ))
                plot <- ggmap(newmap)



                plot <-
                    plot + geom_point(
                        data = df,
                        aes(long, lat),
                        color = "blue",
                        size = 4
                    )

                suppressMessages(print(plot))
                # newmap <- getMap(resolution = "high")
                # plot(
                #     newmap,
                #     xlim = c(-180, 180),
                #     ylim = c(-90, 90),
                #     asp = 1
                # )
                #
                # points(long,
                #        lat,
                #        col = "blue",
                #        cex = .9)

                dir.create("TaxonResults")

                path <-
                    paste("TaxonResults//",
                          originName,
                          sep = "")

                dir.create(path)

                path <-
                    paste("TaxonResults//",
                          originName,
                          "//",
                          originName,
                          ".jpg",
                          sep = "")

                dev.print(png,
                          path,
                          width = 1500,
                          height = 1500)



                # Getting data from other sources for probable species
                ranges <-
                    sapply(possibleSpecies, function(name) {
                        key <-
                            name_backbone(name = name)[1]$usageKey
                        count <- occ_count(taxonKey = key)

                        occspocc <-
                            occ(
                                query = name,
                                from = c("vertnet", "bison", "ecoengine"),
                                limit = 10000
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

                        if (dim(occspoccDF)[1] > 15) {
                            suppressMessages(newmap <-
                                                 get_map(
                                                     location = c(lon = latLonCenter$lon, lat = latLonCenter$lat),
                                                     zoom = zoom
                                                 ))
                            plot <- ggmap(newmap)


                            occspoccDF$longitude <-
                                as.numeric(occspoccDF$longitude)
                            occspoccDF$latitude <-
                                as.numeric(occspoccDF$latitude)

                            plot <-
                                plot + geom_point(
                                    data = occspoccDF,
                                    aes(longitude, latitude),
                                    color = "red",
                                    size = 4
                                )

                            suppressMessages(print(plot))


                            # newmap <- getMap(resolution = "high")
                            # plot(
                            #     newmap,
                            #     xlim = c(-180, 180),
                            #     ylim = c(-90, 90),
                            #     asp = 1
                            # )
                            #
                            # points(
                            #     occspoccDF$longitude,
                            #     occspoccDF$latitude,
                            #     col = "red",
                            #     cex = .9
                            # )

                            path <-
                                paste("TaxonResults//",
                                      originName,
                                      "//",
                                      name,
                                      ".jpg",
                                      sep = "")

                            dev.print(png,
                                      path,
                                      width = 1500,
                                      height = 1500)
                        }

                        t
                    })

                # End of Getting data for probable species

                ranges <-
                    as.data.frame(t(ranges)) # converting from matrix to data frame
                ranges <- as.data.frame(lapply(ranges, unlist))
                ranges <-
                    ranges[order(ranges[, 9], decreasing = T), ] # sorting by otherSources data count

                return(list(ranges))
            })
    } else {
        answer <-
            sapply(names(namesToResolve)[1:resolve], simplify = FALSE, USE.NAMES = TRUE, function(nameToResolve) {
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
                    gnrResults[gnrResults$score >= gnr_score, ]
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
                            namelookup$data[namelookup$data$rank == "SPECIES", ]

                        dat <-
                            sub("^(\\S*\\s+\\S+).*",
                                "\\1",
                                (dat$scientificName))


                        unique(dat)
                    })

                possibleSpecies <-
                    unique(as.vector(unlist(possibleSpecies)))

                possibleSpecies <-
                    possibleSpecies[!is.na(possibleSpecies)] # sometimes name_lookup gives NA as a possible name

                # End of Finding probable species names

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

                k <- list(
                    RemarksOnOriginalData = remarks,
                    CommonNamesOfOriginalData = similarNamesOfOrigin,
                    CommonNamesOfSpecies = ranges
                )
                return(k)
            })
    }

    # Building the Answer

    if (tidy_output) {
        size <- length(namesToResolve)
        if (size > 5) {
            misspellingsData <- misspellingsData[1:5]
            namesPercentageTable <- namesPercentageTable[1:5, ]
        } else {
            misspellingsData = misspellingsData[1:size]
            namesPercentageTable <-
                namesPercentageTable[1:size, ]
        }
    }

    output <-
        list(
            percentageTable = percentageTable,
            namesPercentageTable = namesPercentageTable,
            possibleMisspellings = misspellingsData,
            resolveResults = answer
        )

    print(Sys.time() - t)
    return(output)

    # End of Building the Answer
}