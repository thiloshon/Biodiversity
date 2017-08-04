#' Flag repeating digits
#'
#' Runs quality check of finding repeated digits and flags accordingly.
#'
#' The function runs a quality check on two fields of GBIF data - decimalLatitude and decimalLongitude.
#' Checks if the decimal values have repeated values at the end of the value. Repeated values might
#' mean error in digitisation or conversions of records
#' @importFrom  data.table as.data.table
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

  # -------------- Endof Finding number of repeated digits of Latitude and Flagging-------------------- #

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
