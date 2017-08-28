#' Flag repeating digits
#'
#' Runs quality check of finding repeated digits and flags accordingly.
#'
#' The function runs a quality check on two fields of GBIF data - decimalLatitude and decimalLongitude.
#' Checks if the decimal values have repeated values at the end of the value. Repeated values might
#' mean error in digitisation or conversions of records
#' @import  data.table
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; decimalLatitude and decimalLongitude.
#' @return Same dataframe with two additional columns; latRepeatCount and longRepeatCount. Both shows the number of digits that are repeating
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- repeating_digits(dat$data)
repeating_digits <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("decimalLatitude", "decimalLongitude"))

    # -------------- Finding records with repeated digits ---------------------------------------------- #
    latRepeat <-
        sapply(gbif_data$decimalLatitude, function(lat) {
            x <- as.character.numeric_version(lat)
            rmword <- grepl("(\\w)\\1{2, }", x)
            return(rmword)
        })

    longRepeat <-
        sapply(gbif_data$decimalLongitude, function(long) {
            x <- as.character.numeric_version(long)
            rmword <- grepl("(\\w)\\1{2, }", x)
            return(rmword)
        })

    # -------------- End of Finding records with repeated digits --------------------------------------- #

    # -------------- Finding number of repeated digits of Latitude and Flagging------------------------- #
    gbif_data$latRepeatCount  <-
        sapply(1:dim(gbif_data)[1], function(counter) {
            if (latRepeat[counter]) {
                lat <-
                    as.character.numeric_version(gbif_data[counter, c("decimalLatitude")])

                list = as.vector(strsplit(lat, ""))
                table <- data.table::as.data.table(list)
                frameCount <-
                    table[, count := sequence(.N), by = rleid(V1)][V1 == "No", count := 0][]

                max(frameCount$count)
            } else{
                0
            }
        })

    # -------------- End of Finding number of repeated digits of Latitude and Flagging-------------------- #

    # -------------- Finding number of repeated digits of Longitude and Flagging------------------------- #
    gbif_data$longRepeatCount <-
        sapply(1:dim(gbif_data)[1], function(counter) {
            if (longRepeat[counter]) {
                long <-
                    as.character.numeric_version(gbif_data[counter, c("decimalLongitude")])

                list = as.vector(strsplit(long, ""))
                table <- data.table::as.data.table(list)
                frameCount <-
                    table[, count := sequence(.N), by = rleid(V1)][V1 == "No", count := 0][]

                max(frameCount$count)
            } else{
                0
            }
        })
    # -------------- End of Finding number of repeated digits of Longitude and Flagging------------------- #

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}




#' Flag questionable georeference protocol records
#'
#' Runs quality check of checking authenticity of georeference protocols and flags accordingly.
#'
#' The function runs a quality check on georeference protocol of GBIF data.
#' Checks if the georeferenceProtocol is valid. Generally, coordinates determined from GPS have an uncertainty of a few metres,
#' especially if the Horizontal Dilution of Precision (HDOP) is also recorded. Coordinates determined from modern satellite maps,
#' for example Google Maps/Earth are also quite precise (although beware of satellite image rectification issues). On the other hand
#' coordinates determined from say 1:1 million maps, or from gazetteers accurate to one minute, are much less precise.
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with one mandatory field; georeferenceProtocol
#' @return Same dataframe with one additional column; georeferenceProtocolFlag
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- georeference_protocol_flag(dat$data)
georeference_protocol_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(
        gbif_data,
        c("georeferenceProtocol")
    )

    # -------------- List of possible georeference protocols ---------------------------------------------- #

    protocols <-
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

    # -------------- Flags of the above georeference protocols -------------------------------------------- #

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

    # -------------- Mapping georeference protocols and suitable Flags  ------------------------------------ #
    gbif_data$georeferenceProtocolFlag <-
        flags[match(gbif_data$georeferenceProtocol, protocols)]

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}



#' Flag coordinates with decimal points mismatch.
#'
#' Runs quality check of checking coordinates with more than 3 decimal point difference.
#'
#' The function runs a quality check on coordinates of GBIF data to check if latitude and longitude have
#' different number of decimal points. If coordinate was recorded by an accepted protocol, its unlikely to
#' identify both coordinate with varying precision.
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; decimalLatitude, decimalLongitude
#' @return Same dataframe with one additional column; decimalPointDifference
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- coordinates_decimal_mismatch(dat$data)
coordinates_decimal_mismatch <- function(gbif_dataFrame) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_dataFrame,
                                 c("decimalLatitude", "decimalLongitude"))


    lat <- sapply(gbif_dataFrame$decimalLatitude, function(lat) {
        list <- strsplit(sub('0+$', '', as.character.numeric_version(lat)),
                         ".",
                         fixed = TRUE)

        if (length(list[[1]]) < 2) {
            return (0)
        } else {
            return (nchar(list[[1]][[2]]))
        }
    })

    long <- sapply(gbif_dataFrame$decimalLongitude, function(long) {
        list <- strsplit(sub('0+$', '', as.character.numeric_version(long)),
                         ".",
                         fixed = TRUE)
        if (length(list[[1]]) < 2) {
            return (0)
        } else {
            return (nchar(list[[1]][[2]]))
        }
    })

    gbif_dataFrame$lat <- lat
    gbif_dataFrame$lon <- long

    gbif_dataFrame$decimalPointDifference <- abs(lat - long)

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_dataFrame)
}



#' Flag unverified georeferenced records
#'
#' Runs quality check of checking verification of georeferences.
#'
#' The function runs a quality check on georeference protocol of GBIF data.
#' Checks if the georeferenceVerificationStatus is valid.
#' Where an occurrence record is on the edge of its range, or is a range extension, then it is useful to know whether the
#' location coordinates have been verified
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with one mandatory field; georeferenceVerificationStatus
#' @return Same dataframe with one additional column; georeferenceVerificationStatusFlag
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- georeference_verification_status_flag(dat$data)
georeference_verification_status_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("georeferenceVerificationStatus"))

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

    gbif_data$georeferenceVerificationStatusFlag <-
        flags[match(gbif_data$georeferenceVerificationStatus, values)]

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}



#' Flag records georeferenced after the event date
#'
#' Runs quality check of checking if georeferencing was done after event date.
#'
#' The function collects the time in years between the occurrence date and the georeferenced date.
#' If the record was georeferenced on the day it was occurred, then the record will most likely to be correct. But,
#' if the record was georeferenced several years later, then the reliability will be low. This flag brings that out.
#' @export
#' @import parsedate
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; georeferencedDate and eventDate
#' @return Same dataframe with one additional column; georeferencePostOccurrenceFlag
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- georeference_post_occurrence_flag(dat$data)
georeference_post_occurrence_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("georeferencedDate", "eventDate"))

    logical <- gbif_data$georeferencedDate != "" & !is.na(gbif_data$georeferencedDate)
    subset <- gbif_data[logical, ]


    eventDate <- parse_date(subset$eventDate)
    referencedDate <- parse_date(subset$georeferencedDate)

    diffInYears <- as.numeric(referencedDate - eventDate) / 365.242
    diffInYears <- as.integer(diffInYears)
    gbif_data$georeferencePostOccurrenceFlag <- NA
    gbif_data$georeferencePostOccurrenceFlag[logical] <- diffInYears

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}


#' Flag records with incorrect coordinatePrecision
#'
#' Runs quality check of checking if coordinatePrecision is wthin possible range.
#'
#' The coordinate precision (dwc:coordinatePrecision), as a decimal representation, is outside the range of zero (minimum) and
#' one (maximum) coordinatePrecision /=>0<=1.
#' coordinatePrecision is a measure of precision of the coordinates. It can take only values between 0 and 1 for 1 being
#' highly precise and 0 being no precise at all. This check will flag all records with precision out of this range.
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with one mandatory field; coordinatePrecision
#' @return Same dataframe with one additional column; coordinatePrecisionOutofRangeFlag
#' @examples
#' \dontrun{
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- coordinate_precision_outofrange_flag(dat$data)
#' }
coordinate_precision_outofrange_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("coordinatePrecision"))

    gbif_data$coordinatePrecisionOutofRangeFlag <-
        gbif_data$coordinatePrecision < 0 |
        gbif_data$coordinatePrecision > 1

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}


#' Flag records with incorrect coordinateuncertainity
#'
#' Runs quality check of checking if coordinateUncertaintyInMeters is wthin possible range.
#'
#' The Geopoint uncertainty (dwc:coordinateUncertaintyInMeters) should be a whole number and greater than zero (meters)
#' coordinateUncertaintyInMeters=integer<0
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with one mandatory field; coordinateUncertaintyInMeters
#' @return Same dataframe with one additional column; uncertaintyOutofRangeFlag
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- uncertainty_outofrange_flag(dat$data)
uncertainty_outofrange_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("coordinateUncertaintyInMeters"))

    gbif_data$uncertaintyOutofRangeFlag <-
        gbif_data$coordinateUncertaintyInMeters %% 1 != 0 |
        gbif_data$coordinateUncertaintyInMeters < 0

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}


#' Flag records with contradicting locality to the coordinates
#'
#' Runs quality check of checking if locality field and coordinates point same location.
#'
#' @export
#' @import tm ggmap
# #' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with three mandatory field; decimalLatitude, decimalLongitude, locality
#' @return Same dataframe with two additional columns; localityCoordinateMismatchFlag, generatedLocalityCoordinate
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- locality_coordinate_mismatch_flag(dat$data)
locality_coordinate_mismatch_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("decimalLatitude", "decimalLongitude", "locality"))

    localities <- unique(gbif_data$locality)

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

    localitiesClean <- removeWords(localities, stopwords)
    logical <- nchar(localitiesClean) > 3 & !is.na(localitiesClean)
    localities <- localities[logical]
    localitiesClean <- localitiesClean[logical]


    gbif_data$localityCoordinateMismatchFlag <- NA
    gbif_data$generatedLocalityCoordinate <- NA




    if (length(localitiesClean) > 0) {
        localitiesClean <-
            paste(localitiesClean, "Australia", sep = ", ")

        for (count in 1:length(localitiesClean)) {

            coordCenter <-
                suppressMessages(geocode(localitiesClean[count]))

            if (!is.na(coordCenter$lon)) {
                logic <- gbif_data$locality == localities[count] & !is.na(gbif_data$locality)
                gbif_data[logic,]$generatedLocalityCoordinate <-
                    paste(coordCenter$lat, coordCenter$lon)

                gbif_data[logic,]$localityCoordinateMismatchFlag <-
                    !((
                        gbif_data[logic,]$decimalLatitude < as.integer(coordCenter$lat) + 1
                    ) & (
                        gbif_data[logic,]$decimalLatitude > as.integer(coordCenter$lat) - 1
                    ) & (
                        gbif_data[logic,]$decimalLongitude < as.integer(coordCenter$lon) + 1
                    ) & (
                        gbif_data[logic,]$decimalLongitude > as.integer(coordCenter$lon) - 1
                    )
                    )
            }
        }
    }

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}

#' Flag records with contradicting county to the coordinates
#'
#' Runs quality check of checking if county field and coordinates point same location.
#'
#' @export
#' @import tm ggmap
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with three mandatory field; decimalLatitude, decimalLongitude, county
#' @return Same dataframe with one additional column; countyCoordinateMismatchFlag
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- county_coordinate_mismatch_flag(dat$data)
county_coordinate_mismatch_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("decimalLatitude", "decimalLongitude", "county"))

    counties <- unique(gbif_data$county)

    stopwords = c("N/A" ,
                  "not applicable" ,
                  "not recorded")


    countiesClean <- removeWords(counties, stopwords)
    logical <- nchar(countiesClean) > 3 & !is.na(countiesClean)
    counties <- counties[logical]
    countiesClean <- countiesClean[logical]

    countiesClean <-
        paste(countiesClean, "Australia", sep = ", ")

    gbif_data$countyCoordinateMismatchFlag <- NA


    for (count in 1:length(countiesClean)) {
        coordCenter <- suppressMessages(geocode(countiesClean[count]))
        if (!is.na(coordCenter$lon)) {
            logic <- gbif_data$county == counties[count]& !is.na(gbif_data$county)

            gbif_data[logic,]$countyCoordinateMismatchFlag <-
                ((
                    gbif_data[logic,]$decimalLatitude < as.integer(coordCenter$lat) + 1
                ) & (
                    gbif_data[logic,]$decimalLatitude > as.integer(coordCenter$lat) - 1
                ) & (
                    gbif_data[logic,]$decimalLongitude < as.integer(coordCenter$lon) + 1
                ) & (
                    gbif_data[logic,]$decimalLongitude > as.integer(coordCenter$lon) - 1
                )
                )
        }
    }

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}

#' Flag records with contradicting stateProvince to the coordinates
#'
#' Runs quality check of checking if stateProvince field and coordinates point same location.
#'
#' @export
#' @import tm ggmap
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with three mandatory field; decimalLatitude, decimalLongitude, stateProvince
#' @return Same dataframe with one additional column; stateProvinceCoordinateMismatchFlag
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- stateProvinceCoordinateMismatchFlag(dat$data)
stateProvinceCoordinateMismatchFlag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("decimalLatitude", "decimalLongitude", "stateProvince"))

    states <- unique(gbif_data$stateProvince)

    stopwords = c("N/A" ,
                  "not applicable" ,
                  "not recorded")


    statesClean <- removeWords(states, stopwords)
    logical <- nchar(statesClean) > 3 & !is.na(statesClean)
    states <- states[logical]
    statesClean <- statesClean[logical]

    statesClean <-
        paste(statesClean, "Australia", sep = ", ")

    gbif_data$stateProvinceCoordinateMismatchFlag <- NA

    for (count in 1:length(statesClean)) {
        coordCenter <- suppressMessages(geocode(statesClean[count]))
        if (!is.na(coordCenter$lon)) {
            logic <- gbif_data$stateProvince == states[count] & !is.na(gbif_data$stateProvince)

            gbif_data[logic,]$stateProvinceCoordinateMismatchFlag <-
                ((
                    gbif_data[logic,]$decimalLatitude < as.integer(coordCenter$lat) + 1
                ) & (
                    gbif_data[logic,]$decimalLatitude > as.integer(coordCenter$lat) - 1
                ) & (
                    gbif_data[logic,]$decimalLongitude < as.integer(coordCenter$lon) + 1
                ) & (
                    gbif_data[logic,]$decimalLongitude > as.integer(coordCenter$lon) - 1
                )
                )
        }
    }

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}


#' Flag records with unidentified country codes / country
#'
#' Runs quality check of checking if country code / country is recognized.
#'
#' @export
#' @import countrycode
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with one mandatory field; countryCode
#' @return Same dataframe with one additional column; countryCodeUnknownFlag
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- country_code_unknown_flag(dat$data)
country_code_unknown_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("countryCode"))

    gbif_data$countryCodeUnknownFlag <- NA

    countries <- unique(gbif_data$countryCode)


    for (count in 1:length(countries)) {
        check <- countries[count] %in% countrycode_data$iso2c
        gbif_data[gbif_data$countryCode == countries[count], ]$countryCodeUnknownFlag <-
            check
    }

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}


#' Flag records with swapped precision and uncertainty
#'
#' CoordinateUncertaintyInMeters and coordinatePrecision appear swapped as precision is integer > 0 and uncertainty is 0-<=1
#'
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with one mandatory field; coordinatePrecision
#' @return Same dataframe with one additional column; precisionUncertaintyMismatchFlag
#' @examples
#' \dontrun{
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- precision_uncertainty_mismatch_flag(dat$data)
#' }
precision_uncertainty_mismatch_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("coordinatePrecision"))

    gbif_data$precisionUncertaintyMismatchFlag <-
        (
            gbif_data$coordinatePrecision > 0 &
                gbif_data$coordinatePrecision %% 1 == 0 &
                gbif_data$coordinateUncertaintyInMeters >= 0 &
                gbif_data$coordinateUncertaintyInMeters <= 1
        )

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}


#' Flag records with coordinates of center of the country
#'
#' Flag records with decimalLatitude/decimalLongitude=spatial buffered centre of country
#'
#' When coordinates are not known auto georefences makes the center of the country the coordinate. This flag brings that out
#' @export
#' @import ggmap
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; "decimalLatitude", "decimalLongitude"
#' @return Same dataframe with one additional column; centerofTheCountryCoordinatesFlag
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- center_of_the_country_coordinates_flag(dat$data)
center_of_the_country_coordinates_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("decimalLatitude", "decimalLongitude"))

    center <- geocode("Australia")

    lat <- as.integer(center$lat)
    lon <- as.integer(center$lon)

    gbif_data$centerofTheCountryCoordinatesFlag <-
        (
            as.integer(gbif_data$decimalLatitude) == lat &
                as.integer(gbif_data$decimalLongitude) == lon

            # > sumFac(australianMammals$decimalLatitude==-24  & australianMammals$decimalLongitude==134)
            # FALSE    TRUE    NA's
            # 1722404      10   35779

            # > geocode("Australia")
            # lon      lat
            # 1 133.7751 -25.2744

            # (gbif_data$decimalLatitude) == lat &
            #     (gbif_data$decimalLongitude) == lon
        )

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}


#' Flag records with questionable occurance status or establishment.
#'
#' It is important to know if an occurrence is natural, rather than an escapee from captivity, or say, a plant cultivated in a park,
#' or indeed if the occurrence is extralimital to is normal range, e.g. a vagrant migratory bird that has drifted way off-course.
#' Usually these records are excluded from spatial analyses. Pertinent information to this may be contained in Darwin Core fields:
#' DwC:establishmentMeans (e.g. cultivated, invasive, escaped from captivity) and DwC:occurrenceStatus (e.g. present, absent).
#' These fields could be used to record extralimital occurrences
#'
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; ""establishmentMeans", "occurrenceStatus"
#' @return Same dataframe with one additional column; occurrenceEstablishmentFlag
#' @examples
#' \dontrun{
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- occurrence_establishment_flag(dat$data)
#' }
occurrence_establishment_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("establishmentMeans", "occurrenceStatus"))

    values <-
        c("",
          "MANAGED",
          "NATIVE",
          "UNCERTAIN")

    ## TODO: refine this list after input from experts, Use Locality too
    flags <- c(NA,
               TRUE,
               FALSE,
               TRUE)

    gbif_data$occurrenceEstablishmentFlag <-
        flags[match(gbif_data$establishmentMeans, values)]

    tryCatch(
        gbif_data[gbif_data$occurrenceStatus == "present",]$occurrenceEstablishmentFlag <-
            FALSE,
        error = function(e)
            e
    )

    tryCatch(
        gbif_data[gbif_data$occurrenceStatus == "absent",]$occurrenceEstablishmentFlag <-
            FALSE,
        error = function(e)
            e
    )
    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}


#' Flag records with coordinates to be corrected to match country
#'
#' Supplied geographic coordinates have to be transposed or the sign reversed (negated) to place the record in the supplied country
#'
#' @export
#' @import maps
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; "decimalLatitude", "decimalLongitude"
#' @return Same dataframe with one additional column; coordinateNegatedFlag
#' @examples
#' @dontrun{
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- coordinate_negated_flag(dat$data)
#' }
coordinate_negated_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("decimalLatitude", "decimalLongitude"))
    # TODO: automate country finding from hardcoded to country field

    gbif_data$coordinateNegatedFlag <- NA

    logical <-  !is.na(gbif_data$decimalLatitude)
    subset <- gbif_data[logical,]

    countries <-
        map.where(database = "world",
                  subset$decimalLongitude,
                  subset$decimalLatitude)

    false <- !grepl("Australia", countries)
    records <- subset[false,]

    for (counter in 1:dim(records)[1]) {
        # - lat
        country1 <-
            map.where(database = "world",
                      records[counter,]$decimalLongitude,
                      records[counter,]$decimalLatitude * (-1))

        # - lon
        country2 <-
            map.where(database = "world",
                      records[counter,]$decimalLongitude * (-1),
                      records[counter,]$decimalLatitude)

        # - lat lon
        country3 <-
            map.where(database = "world",
                      records[counter,]$decimalLongitude * (-1),
                      records[counter,]$decimalLatitude * (-1))

        if (any(grepl("Australia", c(country1, country2, country3)))) {
            records[counter,]$coordinateNegatedFlag <- TRUE
        } else {
            records[counter,]$coordinateNegatedFlag <- FALSE
        }
    }

    subset[false,] <- records
    gbif_data[logical,]  <- subset

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}


#' Flag records with coordinates mismatching country
#'
#' Geographic coordinates fall outside the area defined by the referenced terrestrial boundary of the country
#'
#' @export
#' @import maps
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; "decimalLatitude", "decimalLongitude"
#' @return Same dataframe with two additional columns; countryCoordinateMismatchFlag, generatedCountries
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- country_coordinate_mismatch_flag(dat$data)
country_coordinate_mismatch_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("decimalLatitude", "decimalLongitude"))

    gbif_data$countryCoordinateMismatchFlag <- NA
    gbif_data$generatedCountries <- NA

    logical <- !is.na(gbif_data$decimalLatitude)

    gbif_data[logical, ]$generatedCountries <-
        map.where(database = "world",
                  gbif_data[logical, ]$decimalLongitude,
                  gbif_data[logical, ]$decimalLatitude)

    gbif_data[logical, ]$countryCoordinateMismatchFlag <-
        !grepl("Australia", gbif_data[logical, ]$generatedCountries)

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}


#' Flag records with incorrect depth
#'
#' Runs quality check of checking if depth is wthin possible range.
#'
#' The depth (dwc:depth) should be less than zero (0) or maximum depth is greater than 11,000 meters
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with one mandatory field; depth
#' @return Same dataframe with one additional column; depthOutofRangeFlag
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- uncertainty_outofrange_flag(dat$data)
depth_out_of_range_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
                                 c("depth"))


    gbif_data$depthOutofRangeFlag <-
        gbif_data$depth < 0 |
        gbif_data$depth > 11000

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}
