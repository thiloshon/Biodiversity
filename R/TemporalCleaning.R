#' Flag records with dates set to first day of the year
#'
#' Event date (month, day) is first of year eventDate=1978-01-01, or year=1978, month=1, day=1.
#'
#' The function runs a quality check to find if the date of occurance is set to the first day of the year.
#' This might mean the date was not recorded and ehile digitization the date was automatically set my system as the first day.
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; month and day.
#' @return Same dataframe with one column; firstOfYearFlag.
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- first_of_year_flag(dat$data)
first_of_year_flag <- function(GBIF_Data) {
    t <- Sys.time()

    GBIF_Data$firstOfYearFlag <-
        GBIF_Data$month == 1 & GBIF_Data$day == 1

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(GBIF_Data)
}


#' Flag records with incorrect event date, identification date pair.
#'
#' The date of identification (dwc:dateIdentified) is earlier than the event date dateIdentified < eventDate
#'
#' The function runs a quality check to find if the date of identification is before date of occurance.
#' This is not possible at all as identification can only be done on or after occurance.
#' @export
#' @import parsedate
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; dateIdentified and eventDate.
#' @return Same dataframe with two column; identifiedPreEventFlag, identifiedPreEventDiff
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- identified_pre_event_flag(dat$data)
identified_pre_event_flag <- function(GBIF_Data) {
    t <- Sys.time()

    GBIF_Data$identifiedPreEventFlag <- NA
    GBIF_Data$identifiedPreEventDiff <- NA

    log <-
        GBIF_Data$dateIdentified != "" & GBIF_Data$eventDate != "" &
        !is.na(GBIF_Data$dateIdentified) & !is.na(GBIF_Data$eventDate)

    # GBIF_Data <- GBIF_Data[log,] identifiedDate <- parse_iso_8601(GBIF_Data$dateIdentified) eventDate <-
    # parse_iso_8601(GBIF_Data$eventDate) GBIF_Data$identifiedPreEventFlag <- identifiedDate < eventDate
    # GBIF_Data$identifiedPreEventDiff <- identifiedDate - eventDate

    identifiedDate <-
        parse_iso_8601(GBIF_Data[log,]$dateIdentified)
    eventDate <- parse_iso_8601(GBIF_Data[log,]$eventDate)
    GBIF_Data[log,]$identifiedPreEventFlag <-
        identifiedDate < eventDate
    GBIF_Data[log,]$identifiedPreEventDiff <-
        identifiedDate - eventDate

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(GBIF_Data)
}



#' Flag records with incorrect identification date.
#'
#' The date of identification (dwc:dateIdentified) falls prior to Linnaeus (1753) or after
#' the current date. dateIdentified < 1753 or in future
#'
#' @export
#' @import parsedate
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; dateIdentified and eventDate.
#' @return Same dataframe with one column; dateIdentified
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- impropable_identified_date_flag(dat$data)
impropable_identified_date_flag <- function(GBIF_Data) {
    t <- Sys.time()

    GBIF_Data$impropableIdentifiedDateFlag <- NA

    log <- GBIF_Data$dateIdentified != "" & !is.na(GBIF_Data$dateIdentified)

    GBIF_Data <- GBIF_Data[log,]

    identifiedDate <- parse_iso_8601(GBIF_Data$dateIdentified)
    GBIF_Data$impropableIdentifiedDateFlag <-
        as.Date(identifiedDate) > Sys.Date()

    # identifiedDate <- parse_iso_8601(GBIF_Data[log, ]$dateIdentified) GBIF_Data[log, ]$impropableIdentifiedDateFlag <-
    # as.Date(identifiedDate) > Sys.Date()

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(GBIF_Data)
}
