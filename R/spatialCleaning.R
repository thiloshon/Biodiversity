# 01
# Latitude Longitude decimal points mismatch
# "Here, the coordinates with more than 3 decimal point difference can be flagged

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

# 02
# Repeated values at the end of coordinate
# This might mean errors in conversions

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

# 03
# DwC:georeferenceProtocol
# used to determine the latitude and longitude. Generally, coordinates determined from GPS have an uncertainty of a few metres,
# especially if the Horizontal Dilution of Precision (HDOP) is also recorded. Coordinates determined from modern satellite maps,
# for example Google Maps/Earth are also quite precise (although beware of satellite image rectification issues). On the other hand
# coordinates determined from say 1:1 million maps, or from gazetteers accurate to one minute, are much less precise.

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

# 04
# DwC:georeferenceVerificationStatus
# Where an occurrence record is on the edge of its range, or is a range extension, then it is useful to know whether the
# location coordinates have been verified

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

# 05
# GEOREFERENCE_POST_OCCURRENCE
# The record was georeferenced after the event date
georeferencePostOccurrenceFlag <- function(GBIF_Data) {
    t <- Sys.time()

    require(parsedate)

    logical <- GBIF_Data$georeferencedDate != ""
    subset <- GBIF_Data[logical, ]

    eventDate <- parse_date(subset$eventDate)
    referencedDate <- parse_date(subset$georeferencedDate)

    diffInYears <- as.numeric(referencedDate - eventDate) / 365.242
    GBIF_Data$georeferencePostOccurrenceFlag <- NA
    GBIF_Data$georeferencePostOccurrenceFlag[logical] <- diffInYears

    print(Sys.time() - t)
    return(GBIF_Data)
}

# 06
# PRECISION_RANGE_MISMATCH
# The coordinate precision (dwc:coordinatePrecision), as a decimal representation, is outside the range of zero (minimum) and
# one (maximum) coordinatePrecision /=>0<=1

coordinatePrecisionOutofRangeFlag <- function(GBIF_Data) {
    t <- Sys.time()

    GBIF_Data$coordinatePrecisionOutofRangeFlag <-
        GBIF_Data$coordinatePrecision < 0 |
        GBIF_Data$coordinatePrecision > 1

    print(Sys.time() - t)
    return(GBIF_Data)
}

# 07
# UNCERTAINTY_RANGE_MISMATCH
# Geopoint uncertainty (dwc:coordinateUncertaintyInMeters) should be a whole number and greater than zero (meters)
# coordinateUncertaintyInMeters=integer<0

uncertaintyOutofRangeFlag <- function(GBIF_Data) {
    t <- Sys.time()

    GBIF_Data$uncertaintyOutofRangeFlag <-
        GBIF_Data$coordinateUncertaintyInMeters %% 1 != 0 |
        GBIF_Data$coordinateUncertaintyInMeters < 0

    print(Sys.time() - t)
    return(GBIF_Data)
}

# 08
# Locality-Coordinate Mismatch

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

    if (length(localitiesClean) > 0) {
        localitiesClean <-
            paste(localitiesClean, "Australia", sep = ", ")

        print(length(localitiesClean))


        for (count in 1:length(localitiesClean)) {
            print(localitiesClean[count])

            coordCenter <-
                suppressMessages(geocode(localitiesClean[count]))
            if (!is.na(coordCenter$lon)) {
                logic <- GBIF_Data$locality == localities[count]

                GBIF_Data[logic,]$localityCoordinateMismatchFlag <-
                    ((
                        GBIF_Data[logic,]$decimalLatitude < as.integer(coordCenter$lat) + 1
                    ) & (
                        GBIF_Data[logic,]$decimalLatitude > as.integer(coordCenter$lat) - 1
                    ) & (
                        GBIF_Data[logic,]$decimalLongitude < as.integer(coordCenter$lon) + 1
                    ) & (
                        GBIF_Data[logic,]$decimalLongitude > as.integer(coordCenter$lon) - 1
                    )
                    )
            }
        }
    }

    print(Sys.time() - t)
    return(GBIF_Data)
}

# 09
# County-Coordinate Mismatch

countyCoordinateMismatchFlag <- function(GBIF_Data) {
    t <- Sys.time()

    counties <- unique(GBIF_Data$county)

    stopwords = c("N/A" ,
                  "not applicable" ,
                  "not recorded")

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

            GBIF_Data[logic,]$countyCoordinateMismatchFlag <-
                ((
                    GBIF_Data[logic,]$decimalLatitude < as.integer(coordCenter$lat) + 1
                ) & (
                    GBIF_Data[logic,]$decimalLatitude > as.integer(coordCenter$lat) - 1
                ) & (
                    GBIF_Data[logic,]$decimalLongitude < as.integer(coordCenter$lon) + 1
                ) & (
                    GBIF_Data[logic,]$decimalLongitude > as.integer(coordCenter$lon) - 1
                )
                )
        }
    }

    print(Sys.time() - t)
    return(GBIF_Data)
}

# 10
# StateProvince-Coordinate Mismatch

stateProvinceCoordinateMismatchFlag <- function(GBIF_Data) {
    t <- Sys.time()

    states <- unique(GBIF_Data$stateProvince)

    stopwords = c("N/A" ,
                  "not applicable" ,
                  "not recorded")

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

            GBIF_Data[logic,]$stateProvinceCoordinateMismatchFlag <-
                ((
                    GBIF_Data[logic,]$decimalLatitude < as.integer(coordCenter$lat) + 1
                ) & (
                    GBIF_Data[logic,]$decimalLatitude > as.integer(coordCenter$lat) - 1
                ) & (
                    GBIF_Data[logic,]$decimalLongitude < as.integer(coordCenter$lon) + 1
                ) & (
                    GBIF_Data[logic,]$decimalLongitude > as.integer(coordCenter$lon) - 1
                )
                )
        }
    }

    print(Sys.time() - t)
    return(GBIF_Data)
}

# 11
# COUNTRY_NAME_UNKNOWN
# Country name (dwc:country) not in vocabulary country not in vocabulary. changed to countrycode as current format doesnt have filed country

countryCodeUnknownFlag <- function(GBIF_Data) {
    t <- Sys.time()

    require(countrycode)

    GBIF_Data$countryCodeUnknownFlag <- NA

    countries <- unique(GBIF_Data$countryCode)

    #print(countries)

    for (count in 1:length(countries)) {
        check <- countries[count] %in% countrycode_data$iso2c
        GBIF_Data[GBIF_Data$countryCode == countries[count], ]$countryCodeUnknownFlag <-
            check
    }

    print(Sys.time() - t)
    return(GBIF_Data)
}

# 12
# UNCERTAINTY_IN_PRECISION
# coordinateUncertaintyInMeters and coordinatePrecision appear swapped as precision is integer > 0 and uncertainty is 0-<=1
# coordinateUncertaintyInMeters<0 and coordinatePrecision integer>0

precisionUncertaintyMismatch <- function(GBIF_Data) {
    t <- Sys.time()

    GBIF_Data$precisionUncertaintyMismatchFlag <-
        (
            GBIF_Data$coordinatePrecision > 0 &
                GBIF_Data$coordinatePrecision %% 1 == 0 &
                GBIF_Data$coordinateUncertaintyInMeters >= 0 &
                GBIF_Data$coordinateUncertaintyInMeters <= 1
        )

    print(Sys.time() - t)
    return(GBIF_Data)
}

# 13
# COORDINATES_CENTRE_OF_COUNTRY
# "Supplied geographic coordinates are within a defined buffer of the centre of the country
# decimalLatitude/decimalLongitude=spatial buffered centre of country
# If decimalLatitude=-29.5 and decimalLongitude=145.4, then the location is likely defaulted to centre of Australia

centerofTheCountryCoordinatesFlag <- function(GBIF_Data) {
    t <- Sys.time()

    require(ggmap)

    center <- geocode("Australia")

    lat <- as.integer(center$lat)
    lon <- as.integer(center$lon)

    GBIF_Data$centerofTheCountryCoordinatesFlag <-
        (
            as.integer(GBIF_Data$decimalLatitude) == lat &
                as.integer(GBIF_Data$decimalLongitude) == lon

            # > sumFac(australianMammals$decimalLatitude==-24  & australianMammals$decimalLongitude==134)
            # FALSE    TRUE    NA's
            # 1722404      10   35779

            # > geocode("Australia")
            # lon      lat
            # 1 133.7751 -25.2744

            # (GBIF_Data$decimalLatitude) == lat &
            #     (GBIF_Data$decimalLongitude) == lon
        )

    print(Sys.time() - t)
    return(GBIF_Data)
}

# 14
# DwC:establishmentMeans / DwC:occurrenceStatus
# "It is important to know if an occurrence is natural, rather than an escapee from captivity, or say, a plant cultivated in a park,
# or indeed if the occurrence is extralimital to is normal range, e.g. a vagrant migratory bird that has drifted way off-course.
# Usually these records are excluded from spatial analyses. Pertinent information to this may be contained in Darwin Core fields:
# DwC:establishmentMeans (e.g. cultivated, invasive, escaped from captivity) and DwC:occurrenceStatus (e.g. present, absent).
# These fields could be used to record extralimital occurrences.
# Not feasible to use in the current dataset :(
#    but locality has words CAPTIVE BRED and so on which can be used. But meager in number.

occurrenceEstablishmenFlag <- function(GBIF_Data) {
    t <- Sys.time()

    values <-
        c("",
          "MANAGED",
          "NATIVE",
          "UNCERTAIN")

    ## TODO: refine this list after input from experts
    flags <- c(NA,
               TRUE,
               FALSE,
               TRUE,
               TRUE,
               FALSE)

    GBIF_Data$occurrenceEstablishmentFlag <-
        flags[match(GBIF_Data$establishmentMeans, values)]

    tryCatch(
        GBIF_Data[GBIF_Data$occurrenceStatus == "present",]$occurrenceEstablishmentFlag <-
            TRUE,
        error = function(e)
            e
    )

    tryCatch(
        GBIF_Data[GBIF_Data$occurrenceStatus == "absent",]$occurrenceEstablishmentFlag <-
            FALSE,
        error = function(e)
            e
    )
    print(Sys.time() - t)
    return(GBIF_Data)
}

# 15
# COORDINATES_CORRECTED_FOR COUNTRY
# Supplied geographic coordinates were transposed or the sign reversed (negated) to place the record in the supplied country
# decimalLatitude/decimalLongitude /=country, needs swapped or negated

coordinateNegatedFlag <- function(GBIF_Data) {
    t <- Sys.time()

    require(maps)

    logical <-  !is.na(GBIF_Data$decimalLatitude)

    subset <- GBIF_Data[logical,]

    countries <-
        map.where(database = "world",
                  subset$decimalLongitude,
                  subset$decimalLatitude)

    false <- !grepl("Australia", countries)

    records <- subset[false,]
    #print(records)

    flags <- sapply(records, function(record) {
        print(record)
    })

    #GBIF_Data$occurrenceEstablishmentFlag <-
    #flags[match(GBIF_Data$establishmentMeans, values)]

    print(Sys.time() - t)

    return(GBIF_Data)
}
