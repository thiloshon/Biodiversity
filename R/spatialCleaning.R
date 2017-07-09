coordinatesDecimalMismatch <- function(GBIF_DataFrame) {
    t <- Sys.time()
    lat <- sapply(GBIF_DataFrame$decimalLatitude, function(lat) {
        list <- strsplit(sub('0+$', '', as.character.numeric_version(lat)),
                         ".",
                         fixed = TRUE)

        if (length(list[[1]]) < 2) {
            return (0)
        } else {
            return (nchar(list[[1]][[2]]))
        }
    })

    long <- sapply(GBIF_DataFrame$decimalLongitude, function(long) {
        list <- strsplit(sub('0+$', '', as.character.numeric_version(long)),
                         ".",
                         fixed = TRUE)
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


repeatingDigits <- function(GBIF_Data) {
    t <- Sys.time()


    latRepeat <- sapply(GBIF_Data$decimalLatitude, function(lat) {
        x <- as.character.numeric_version(lat)
        rmword <- grepl("(\\w)\\1{2, }", x)
        return(rmword)
    })

    longRepeat <-
        sapply(GBIF_Data$decimalLongitude, function(long) {
            x <- as.character.numeric_version(long)
            rmword <- grepl("(\\w)\\1{2, }", x)
            return(rmword)
        })

    require(data.table)

    GBIF_Data$latRepeatCount  <-
        sapply(1:dim(GBIF_Data)[1], function(counter) {
            if (latRepeat[counter]) {
                lat <-
                    as.character.numeric_version(GBIF_Data[counter, c("decimalLatitude")])

                list = as.vector(strsplit(lat, ""))
                table <- as.data.table(list)
                frameCount <-
                    table[, count := sequence(.N), by = rleid(V1)][V1 == "No", count := 0][]

                max(frameCount$count)
            } else{
                0
            }
        })


    GBIF_Data$longRepeatCount <-
        sapply(1:dim(GBIF_Data)[1], function(counter) {
            if (longRepeat[counter]) {
                long <-
                    as.character.numeric_version(GBIF_Data[counter, c("decimalLongitude")])

                list = as.vector(strsplit(long, ""))
                table <- as.data.table(list)
                frameCount <-
                    table[, count := sequence(.N), by = rleid(V1)][V1 == "No", count := 0][]

                max(frameCount$count)
            } else{
                0
            }
        })

    print(Sys.time() - t)
    return(GBIF_Data)


}

georeferenceProtocolFlag <- function(GBIF_Data) {
    # > sumFac(y$georeferenceProtocolFlag)
    # High     Low  Medium    NA's
    #  24     409    3961 1753799

    t <- Sys.time()
    values <-
        c(
            "",
            "MaNIS/HerpNet/ORNIS Georeferencing Guidelines, GBIF Best Practices",
            "GEO MACN v. 0.1",
            "not recorded",
            "VertNet Georeferencing Guidelines",
            "MaNIS georeferencing guidelines" ,
            "Collector assigned",
            "GEOLocate",
            "unknown-migration",
            "collector",
            "MaNIS",
            "unknown",
            "digital resource",
            "unspecified",
            "GeoLocate",
            "BioGeoMancer",
            "GPS",
            "estimated from other samples"
        )

    ## TODO: refine this list after input from experts
    flags <- c(
        NA,
        "Medium",
        "Medium",
        "Low",
        "Medium",
        "Medium",
        "Low",
        "Medium",
        "Low",
        "Low",
        "Medium",
        NA,
        "Medium",
        NA,
        "Medium",
        "Medium",
        "High",
        "Low"
    )

    GBIF_Data$georeferenceProtocolFlag <-
        flags[match(GBIF_Data$georeferenceProtocol, values)]

    print(Sys.time() - t)

    return(GBIF_Data)

}

georeferenceVerificationStatusFlag <- function(GBIF_Data) {
    # > sumFac(y$georeferenceVerificationStatusFlag)
    # High     Low    NA's
    #  22    3949 1754222

    t <- Sys.time()
    values <-
        c(
            "",
            "not georeferenced",
            "Not verified",
            "requires verification",
            "unknown",
            "unverified" ,
            "verified by curator"
        )

    ## TODO: refine this list after input from experts
    flags <- c(NA,
               "Low",
               "Low",
               "Low",
               NA,
               "Low",
               "High")

    GBIF_Data$georeferenceVerificationStatusFlag <-
        flags[match(GBIF_Data$georeferenceVerificationStatus, values)]

    print(Sys.time() - t)

    return(GBIF_Data)

}

georeferencePostOccurrenceFlag <- function(GBIF_Data) {
    t <- Sys.time()

    require(parsedate)

    logical <- GBIF_Data$georeferencedDate != ""
    subset <- GBIF_Data[logical,]

    eventDate <- parse_date(subset$eventDate)
    referencedDate <- parse_date(subset$georeferencedDate)

    diffInYears <- as.numeric(referencedDate - eventDate) / 365.242
    GBIF_Data$georeferencePostOccurrenceFlag <- NA
    GBIF_Data$georeferencePostOccurrenceFlag[logical] <- diffInYears

    print(Sys.time() - t)

    return(GBIF_Data)
}

coordinatePrecisionOutofRangeFlag <- function(GBIF_Data) {
    t <- Sys.time()

    GBIF_Data$coordinatePrecisionOutofRangeFlag <-
        GBIF_Data$coordinatePrecision < 0 |
        GBIF_Data$coordinatePrecision > 1

    print(Sys.time() - t)

    return(GBIF_Data)
}

uncertaintyOutofRangeFlag <- function(GBIF_Data) {
    t <- Sys.time()

    GBIF_Data$uncertaintyOutofRangeFlag <-
        GBIF_Data$coordinateUncertaintyInMeters %% 1 != 0 |
        GBIF_Data$coordinateUncertaintyInMeters < 0

    print(Sys.time() - t)

    return(GBIF_Data)
}

localityCoordinateMismatchFlag <- function(GBIF_Data) {
    t <- Sys.time()

    localities <- unique(GBIF_Data$locality)

    stopwords = c(
        "CAPTIVE" ,
        "BRED" ,
        "Captive" ,
        "Bred" ,
        "captive" ,
        "bred" ,
        "-" ,
        "Locality Unknown" ,
        "NA",
        "BETWEEN",
        "locality withheld"
    )

    require("tm")

    localitiesClean <- removeWords(localities, stopwords)
    logical <- nchar(localitiesClean) > 3
    localities <- localities[logical]
    localitiesClean <- localitiesClean[logical]

    #print(localitiesClean)

    GBIF_Data$localityCoordinateMismatchFlag <- NA

    require(ggmap)

    if(length(localitiesClean)>0){
        localitiesClean <-
            paste(localitiesClean, "Australia", sep = ", ")

        print(length(localitiesClean))



        for (count in 1:length(localitiesClean)) {
            print(localitiesClean[count])

            coordCenter <- suppressMessages(geocode(localitiesClean[count]))
            if (!is.na(coordCenter$lon)) {
                logic <- GBIF_Data$locality == localities[count]

                GBIF_Data[logic, ]$localityCoordinateMismatchFlag <-
                    ((
                        GBIF_Data[logic, ]$decimalLatitude < as.integer(coordCenter$lat) + 1
                    ) & (
                        GBIF_Data[logic, ]$decimalLatitude > as.integer(coordCenter$lat) - 1
                    ) & (
                        GBIF_Data[logic, ]$decimalLongitude < as.integer(coordCenter$lon) + 1
                    ) & (
                        GBIF_Data[logic, ]$decimalLongitude > as.integer(coordCenter$lon) - 1
                    )
                    )
            }
        }
    }


    #print(localitiesClean)





    print(Sys.time() - t)

    return(GBIF_Data)
}


countyCoordinateMismatchFlag <- function(GBIF_Data) {
    t <- Sys.time()

    counties <- unique(GBIF_Data$county)

    stopwords = c(
        "N/A" ,
        "not applicable" ,
        "not recorded"
    )

    require("tm")

    countiesClean <- removeWords(counties, stopwords)
    logical <- nchar(countiesClean) > 3
    counties <- counties[logical]
    countiesClean <- countiesClean[logical]

    countiesClean <-
        paste(countiesClean, "Australia", sep = ", ")

    GBIF_Data$countyCoordinateMismatchFlag <- NA

    require(ggmap)

    for (count in 1:length(countiesClean)) {

        coordCenter <- suppressMessages(geocode(countiesClean[count]))
        if (!is.na(coordCenter$lon)) {
            logic <- GBIF_Data$county == counties[count]

            GBIF_Data[logic, ]$countyCoordinateMismatchFlag <-
                ((
                    GBIF_Data[logic, ]$decimalLatitude < as.integer(coordCenter$lat) + 1
                ) & (
                    GBIF_Data[logic, ]$decimalLatitude > as.integer(coordCenter$lat) - 1
                ) & (
                    GBIF_Data[logic, ]$decimalLongitude < as.integer(coordCenter$lon) + 1
                ) & (
                    GBIF_Data[logic, ]$decimalLongitude > as.integer(coordCenter$lon) - 1
                )
                )
        }
    }

    print(Sys.time() - t)

    return(GBIF_Data)
}

stateProvinceCoordinateMismatchFlag <- function(GBIF_Data) {
    t <- Sys.time()

    states <- unique(GBIF_Data$stateProvince)

    stopwords = c(
        "N/A" ,
        "not applicable" ,
        "not recorded"
    )

    require("tm")

    statesClean <- removeWords(states, stopwords)
    logical <- nchar(statesClean) > 3
    states <- counties[logical]
    statesClean <- statesClean[logical]

    statesClean <-
        paste(statesClean, "Australia", sep = ", ")

    GBIF_Data$stateProvinceCoordinateMismatchFlag <- NA

    require(ggmap)

    for (count in 1:length(statesClean)) {

        coordCenter <- suppressMessages(geocode(statesClean[count]))
        if (!is.na(coordCenter$lon)) {
            logic <- GBIF_Data$county == states[count]

            GBIF_Data[logic, ]$stateProvinceCoordinateMismatchFlag <-
                ((
                    GBIF_Data[logic, ]$decimalLatitude < as.integer(coordCenter$lat) + 1
                ) & (
                    GBIF_Data[logic, ]$decimalLatitude > as.integer(coordCenter$lat) - 1
                ) & (
                    GBIF_Data[logic, ]$decimalLongitude < as.integer(coordCenter$lon) + 1
                ) & (
                    GBIF_Data[logic, ]$decimalLongitude > as.integer(coordCenter$lon) - 1
                )
                )
        }
    }

    print(Sys.time() - t)

    return(GBIF_Data)
}

countryCodeUnknownFlag <- function(GBIF_Data) {

    t <- Sys.time()

    require(countrycode)

    GBIF_Data$countryCodeUnknownFlag <- NA

    countries <- unique(GBIF_Data$countryCode)

    #print(countries)

    for (count in 1:length(countries)) {
        check <- countries[count] %in% countrycode_data$iso2c
        GBIF_Data[GBIF_Data$countryCode==countries[count],]$countryCodeUnknownFlag <- check
    }

    print(Sys.time() - t)

    return(GBIF_Data)
}
