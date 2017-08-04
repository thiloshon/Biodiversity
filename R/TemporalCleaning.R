
# 01 Event date (month, day) is first of year eventDate=1978-01-01, or year=1978, month=1, day=1
firstOfYearFlag <- function(GBIF_Data) {
    t <- Sys.time()

    GBIF_Data$firstOfYearFlag <- GBIF_Data$month == 1 & GBIF_Data$day == 1

    print(Sys.time() - t)
    return(GBIF_Data)
}



# 02 The date of identification (dwc:dateIdentified) is earlier than the event date dateIdentified < eventDate
# dateIdentified=2001-02-14, eventDate=2010-02-14
identifiedPreEventFlag <- function(GBIF_Data) {
    t <- Sys.time()

    require(parsedate)

    GBIF_Data$identifiedPreEventFlag <- NA
    GBIF_Data$identifiedPreEventDiff <- NA


    log <- GBIF_Data$dateIdentified != "" & GBIF_Data$eventDate != ""

    # GBIF_Data <- GBIF_Data[log,] identifiedDate <- parse_iso_8601(GBIF_Data$dateIdentified) eventDate <-
    # parse_iso_8601(GBIF_Data$eventDate) GBIF_Data$identifiedPreEventFlag <- identifiedDate < eventDate
    # GBIF_Data$identifiedPreEventDiff <- identifiedDate - eventDate

    identifiedDate <- parse_iso_8601(GBIF_Data[log, ]$dateIdentified)
    eventDate <- parse_iso_8601(GBIF_Data[log, ]$eventDate)
    GBIF_Data[log, ]$identifiedPreEventFlag <- identifiedDate < eventDate
    GBIF_Data[log, ]$identifiedPreEventDiff <- identifiedDate - eventDate

    print(Sys.time() - t)
    return(GBIF_Data)
}


# 03 The date of identification (dwc:dateIdentified) falls prior to Linnaeus (1753) or after the current date.  dateIdentified
# < 1753 or in future
impropableIdentifiedDateFlag <- function(GBIF_Data) {
    t <- Sys.time()

    require(parsedate)

    GBIF_Data$impropableIdentifiedDateFlag <- NA

    log <- GBIF_Data$dateIdentified != ""

    GBIF_Data <- GBIF_Data[log, ]

    identifiedDate <- parse_iso_8601(GBIF_Data$dateIdentified)
    GBIF_Data$impropableIdentifiedDateFlag <- as.Date(identifiedDate) > Sys.Date()

    # identifiedDate <- parse_iso_8601(GBIF_Data[log, ]$dateIdentified) GBIF_Data[log, ]$impropableIdentifiedDateFlag <-
    # as.Date(identifiedDate) > Sys.Date()

    print(Sys.time() - t)
    return(GBIF_Data)
}
