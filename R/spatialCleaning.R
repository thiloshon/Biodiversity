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

  # -------------- Finding records with repeated digits ---------------------------------------------- #
  latRepeat <- sapply(gbif_data$decimalLatitude, function(lat) {
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

  print(Sys.time() - t)
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

  print(Sys.time() - t)
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

  print(Sys.time() - t)
  return(gbif_dataFrame)
}



# 04
# DwC:georeferenceVerificationStatus
# Where an occurrence record is on the edge of its range, or is a range extension, then it is useful to know whether the
# location coordinates have been verified

georeferenceVerificationStatusFlag <- function(gbif_data) {
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

  gbif_data$georeferenceVerificationStatusFlag <-
    flags[match(gbif_data$georeferenceVerificationStatus, values)]

  print(Sys.time() - t)
  return(gbif_data)
}

# 05
# GEOREFERENCE_POST_OCCURRENCE
# The record was georeferenced after the event date
georeferencePostOccurrenceFlag <- function(gbif_data) {
  t <- Sys.time()

  require(parsedate)

  logical <- gbif_data$georeferencedDate != ""
  subset <- gbif_data[logical,]

  eventDate <- parse_date(subset$eventDate)
  referencedDate <- parse_date(subset$georeferencedDate)

  diffInYears <- as.numeric(referencedDate - eventDate) / 365.242
  diffInYears <- as.integer(diffInYears)
  gbif_data$georeferencePostOccurrenceFlag <- NA
  gbif_data$georeferencePostOccurrenceFlag[logical] <- diffInYears

  print(Sys.time() - t)
  return(gbif_data)
}

# 06
# PRECISION_RANGE_MISMATCH
# The coordinate precision (dwc:coordinatePrecision), as a decimal representation, is outside the range of zero (minimum) and
# one (maximum) coordinatePrecision /=>0<=1

coordinatePrecisionOutofRangeFlag <- function(gbif_data) {
  t <- Sys.time()

  gbif_data$coordinatePrecisionOutofRangeFlag <-
    gbif_data$coordinatePrecision < 0 |
    gbif_data$coordinatePrecision > 1

  print(Sys.time() - t)
  return(gbif_data)
}

# 07
# UNCERTAINTY_RANGE_MISMATCH
# Geopoint uncertainty (dwc:coordinateUncertaintyInMeters) should be a whole number and greater than zero (meters)
# coordinateUncertaintyInMeters=integer<0

uncertaintyOutofRangeFlag <- function(gbif_data) {
  t <- Sys.time()

  gbif_data$uncertaintyOutofRangeFlag <-
    gbif_data$coordinateUncertaintyInMeters %% 1 != 0 |
    gbif_data$coordinateUncertaintyInMeters < 0

  print(Sys.time() - t)
  return(gbif_data)
}

# 08
# Locality-Coordinate Mismatch

localityCoordinateMismatchFlag <- function(gbif_data) {
  t <- Sys.time()

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

  require("tm")

  localitiesClean <- removeWords(localities, stopwords)
  logical <- nchar(localitiesClean) > 3
  localities <- localities[logical]
  localitiesClean <- localitiesClean[logical]

  #print(localitiesClean)

  gbif_data$localityCoordinateMismatchFlag <- NA
  gbif_data$generatedLocalityCoordinate <- NA

  require(ggmap)

  if (length(localitiesClean) > 0) {
    localitiesClean <-
      paste(localitiesClean, "Australia", sep = ", ")

    #print(length(localitiesClean))


    for (count in 1:length(localitiesClean)) {
      #print(localitiesClean[count])

      coordCenter <-
        suppressMessages(geocode(localitiesClean[count]))

      if (!is.na(coordCenter$lon)) {
        logic <- gbif_data$locality == localities[count]
        gbif_data[logic, ]$generatedLocalityCoordinate <-
          paste(coordCenter$lat, coordCenter$lon)
        gbif_data[logic, ]$localityCoordinateMismatchFlag <-
          !((
            gbif_data[logic, ]$decimalLatitude < as.integer(coordCenter$lat) + 1
          ) & (
            gbif_data[logic, ]$decimalLatitude > as.integer(coordCenter$lat) - 1
          ) & (
            gbif_data[logic, ]$decimalLongitude < as.integer(coordCenter$lon) + 1
          ) & (
            gbif_data[logic, ]$decimalLongitude > as.integer(coordCenter$lon) - 1
          )
          )
      }
    }
  }

  print(Sys.time() - t)
  return(gbif_data)
}

# 09
# County-Coordinate Mismatch

countyCoordinateMismatchFlag <- function(gbif_data) {
  t <- Sys.time()

  counties <- unique(gbif_data$county)

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

  gbif_data$countyCoordinateMismatchFlag <- NA

  require(ggmap)

  for (count in 1:length(countiesClean)) {
    coordCenter <- suppressMessages(geocode(countiesClean[count]))
    if (!is.na(coordCenter$lon)) {
      logic <- gbif_data$county == counties[count]

      gbif_data[logic, ]$countyCoordinateMismatchFlag <-
        ((
          gbif_data[logic, ]$decimalLatitude < as.integer(coordCenter$lat) + 1
        ) & (
          gbif_data[logic, ]$decimalLatitude > as.integer(coordCenter$lat) - 1
        ) & (
          gbif_data[logic, ]$decimalLongitude < as.integer(coordCenter$lon) + 1
        ) & (
          gbif_data[logic, ]$decimalLongitude > as.integer(coordCenter$lon) - 1
        )
        )
    }
  }

  print(Sys.time() - t)
  return(gbif_data)
}

# 10
# StateProvince-Coordinate Mismatch

stateProvinceCoordinateMismatchFlag <- function(gbif_data) {
  t <- Sys.time()

  states <- unique(gbif_data$stateProvince)

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

  gbif_data$stateProvinceCoordinateMismatchFlag <- NA

  require(ggmap)

  for (count in 1:length(statesClean)) {
    coordCenter <- suppressMessages(geocode(statesClean[count]))
    if (!is.na(coordCenter$lon)) {
      logic <- gbif_data$county == states[count]

      gbif_data[logic, ]$stateProvinceCoordinateMismatchFlag <-
        ((
          gbif_data[logic, ]$decimalLatitude < as.integer(coordCenter$lat) + 1
        ) & (
          gbif_data[logic, ]$decimalLatitude > as.integer(coordCenter$lat) - 1
        ) & (
          gbif_data[logic, ]$decimalLongitude < as.integer(coordCenter$lon) + 1
        ) & (
          gbif_data[logic, ]$decimalLongitude > as.integer(coordCenter$lon) - 1
        )
        )
    }
  }

  print(Sys.time() - t)
  return(gbif_data)
}

# 11
# COUNTRY_NAME_UNKNOWN
# Country name (dwc:country) not in vocabulary country not in vocabulary. changed to countrycode as current format doesnt have filed country

countryCodeUnknownFlag <- function(gbif_data) {
  t <- Sys.time()

  require(countrycode)

  gbif_data$countryCodeUnknownFlag <- NA

  countries <- unique(gbif_data$countryCode)

  #print(countries)

  for (count in 1:length(countries)) {
    check <- countries[count] %in% countrycode_data$iso2c
    gbif_data[gbif_data$countryCode == countries[count],]$countryCodeUnknownFlag <-
      check
  }

  print(Sys.time() - t)
  return(gbif_data)
}

# 12
# UNCERTAINTY_IN_PRECISION
# coordinateUncertaintyInMeters and coordinatePrecision appear swapped as precision is integer > 0 and uncertainty is 0-<=1
# coordinateUncertaintyInMeters<0 and coordinatePrecision integer>0

precisionUncertaintyMismatch <- function(gbif_data) {
  t <- Sys.time()

  gbif_data$precisionUncertaintyMismatchFlag <-
    (
      gbif_data$coordinatePrecision > 0 &
        gbif_data$coordinatePrecision %% 1 == 0 &
        gbif_data$coordinateUncertaintyInMeters >= 0 &
        gbif_data$coordinateUncertaintyInMeters <= 1
    )

  print(Sys.time() - t)
  return(gbif_data)
}

# 13
# COORDINATES_CENTRE_OF_COUNTRY
# "Supplied geographic coordinates are within a defined buffer of the centre of the country
# decimalLatitude/decimalLongitude=spatial buffered centre of country
# If decimalLatitude=-29.5 and decimalLongitude=145.4, then the location is likely defaulted to centre of Australia

centerofTheCountryCoordinatesFlag <- function(gbif_data) {
  t <- Sys.time()

  require(ggmap)

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

  print(Sys.time() - t)
  return(gbif_data)
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

occurrenceEstablishmentFlag <- function(gbif_data) {
  t <- Sys.time()

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
    gbif_data[gbif_data$occurrenceStatus == "present", ]$occurrenceEstablishmentFlag <-
      FALSE,
    error = function(e)
      e
  )

  tryCatch(
    gbif_data[gbif_data$occurrenceStatus == "absent", ]$occurrenceEstablishmentFlag <-
      FALSE,
    error = function(e)
      e
  )
  print(Sys.time() - t)
  return(gbif_data)
}

# 15
# COORDINATES_CORRECTED_FOR COUNTRY
# Supplied geographic coordinates were transposed or the sign reversed (negated) to place the record in the supplied country
# decimalLatitude/decimalLongitude /=country, needs swapped or negated

coordinateNegatedFlag <- function(gbif_data) {
  t <- Sys.time()
  require(maps)

  gbif_data$coordinateNegatedFlag <- NA

  logical <-  !is.na(gbif_data$decimalLatitude)
  subset <- gbif_data[logical, ]

  countries <-
    map.where(database = "world",
              subset$decimalLongitude,
              subset$decimalLatitude)

  false <- !grepl("Australia", countries)
  records <- subset[false, ]

  for (counter in 1:dim(records)[1]) {
    # - lat
    country1 <-
      map.where(database = "world",
                records[counter, ]$decimalLongitude,
                records[counter, ]$decimalLatitude * (-1))

    # - lon
    country2 <-
      map.where(database = "world",
                records[counter, ]$decimalLongitude * (-1),
                records[counter, ]$decimalLatitude)

    # - lat lon
    country3 <-
      map.where(database = "world",
                records[counter, ]$decimalLongitude * (-1),
                records[counter, ]$decimalLatitude * (-1))

    if (any(grepl("Australia", c(country1, country2, country3)))) {
      records[counter, ]$coordinateNegatedFlag <- TRUE
    } else {
      records[counter, ]$coordinateNegatedFlag <- FALSE
    }
  }

  subset[false, ] <- records
  gbif_data[logical, ]  <- subset

  print(Sys.time() - t)
  return(gbif_data)
}

# 16
# Country-Coordinate Mismatch
# T"Geographic coordinates fall outside the area defined by the referenced terrestrial boundary of the country.
# decimalLatitude/decimalLongitude not within country boundaries.

countryCoordinateMismatchFlag <- function(gbif_data) {
  t <- Sys.time()
  require(maps)

  gbif_data$countryCoordinateMismatchFlag <- NA
  gbif_data$generatedCountries <- NA

  logical <- !is.na(gbif_data$decimalLatitude)

  gbif_data[logical,]$generatedCountries <-
    map.where(database = "world",
              gbif_data[logical,]$decimalLongitude,
              gbif_data[logical,]$decimalLatitude)
  print("Done")

  gbif_data[logical,]$countryCoordinateMismatchFlag <-
    !grepl("Australia", gbif_data[logical,]$generatedCountries)

  print(Sys.time() - t)
  return(gbif_data)
}

# 17
# DEPTH_OUT_OF_RANGE
# Minimum depth is less than zero (0) or maximum depth is greater than 11,000 meters

depthOutofRangeFlag <- function(gbif_data) {
  t <- Sys.time()

  gbif_data$depthOutofRangeFlag <-
    gbif_data$coordinatePrecision < 0 |
    gbif_data$coordinatePrecision > 11000

  print(Sys.time() - t)
  return(gbif_data)
}
