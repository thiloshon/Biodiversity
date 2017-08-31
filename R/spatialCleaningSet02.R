#' Flag records with GBIF Issues
#'
#' Finds records with issues field filled.
#'
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with one mandatory field; issue.
#' @return Same dataframe with four additional columns; countryDerivedFromCoordinatesFlag, geodeticDatumConvertedFlag,
#' geodeticDatumInvalidFlag, geodeticDatumAssumedFlag.
#' @examples
#' \dontrun{
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- gbif_issues_flag(dat$data)
#' }
gbif_issues_flag <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
    c("issue"))

    gbif_data$countryDerivedFromCoordinatesFlag <-
        grepl("COUNTRY_DERIVED_FROM_COORDINATES",
              australianMammals$issue)
    gbif_data$geodeticDatumConvertedFlag <-
        grepl("COORDINATE_REPROJECTED", australianMammals$issue)
    gbif_data$geodeticDatumInvalidFlag <-
        grepl("GEODETIC_DATUM_INVALID", australianMammals$issue)
    gbif_data$geodeticDatumAssumedFlag <-
        grepl("GEODETIC_DATUM_ASSUMED_WGS84", australianMammals$issue)

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}

#' Flag records with species that are invasive.
#'
#' It is important to know if an occurrence is natural, rather than an escapee from captivity, or say, a plant cultivated in a park,
#' or indeed if the occurrence is extralimital to is normal range, e.g. a vagrant migratory bird that has drifted way off-course.
#' Usually these records are excluded from spatial analyses.
#'
#' @export
#' @import originr taxize dplyr
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with one mandatory fields; scientificName
#' @return Same dataframe with one additional column; invasiveFlags
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- invasive_flags(dat$data)
invasive_flags <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
    c("scientificName"))

    gbif_data$invasiveFlags <- NA


    namesToResolve <-
        names(sort(table(gbif_data$scientificName), decreasing = T))  # Sorting the names by frequency,
    # so that the names to be resolved first, are greater part of the complete data

    # namesToResolve <- head(namesToResolve)
    #print(length(namesToResolve))

    names <- taxize::gbif_parse(namesToResolve)
    names <- names$canonicalname


    result <- originr::gisd(names, simplify = TRUE)

    result <- dplyr::rbind_all(result)
    # print(result) result <- unlist(result)

    for (counter in 1:dim(result)[1]) {
        # print(result[counter,'species'])
        logic <-
            grepl(result[counter, "species"], gbif_data$scientificName)
        # print(logic) print(result[counter,'status'])
        gbif_data[logic, "invasiveFlags"] <-
            result[counter, "status"]
    }

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}


#' Flag records with species that are not native to a geographical area.
#'
#' It is important to know if an occurrence is natural, rather than an escapee from captivity, or say, a plant cultivated in a park,
#' or indeed if the occurrence is extralimital to is normal range, e.g. a vagrant migratory bird that has drifted way off-course.
#' Usually these records are excluded from spatial analyses.
#'
#' @export
#' @import originr taxize dplyr
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with one mandatory fields; scientificName
#' @return Same dataframe with three additional columns; nativeFlags, isIntroduced, isCultivated
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- native_flags(dat$data)
native_flags <- function(gbif_data) {
    t <- Sys.time()

    gbif_data <- format_checking(gbif_data,
    c("scientificName"))

    gbif_data$nativeFlags <- NA
    gbif_data$isIntroduced <- NA
    gbif_data$isCultivated <- NA

    namesToResolve <-
        names(sort(table(gbif_data$scientificName), decreasing = T))  # Sorting the names by frequency,
    # so that the names to be resolved first, are greater part of the complete data

    # namesToResolve <- head(namesToResolve)

    names <- taxize::gbif_parse(namesToResolve)
    names <- names$canonicalname

    result <- originr::nsr(names, country = "United States")

    # result <- rbind_all(result)

    # result <- unlist(result)

    if (dim(result)[1] > 0) {
        for (counter in 1:dim(result)[1]) {
            # print(result[counter, "species"])
            logic <-
                grepl(result[counter, "species"], gbif_data$scientificName)
            # print(logic) print(result[counter,'status'])
            gbif_data[logic, "nativeFlags"] <-
                result[counter, "native_status"]
            gbif_data[logic, "isIntroduced"] <-
                result[counter, "isIntroduced"]
            gbif_data[logic, "isCultivated"] <-
                result[counter, "isCultivated"]
        }
    }
    # print(namesToResolve)

    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}


#' Internal function to check validity of input GBIF dataset
#'
#' Finds is input object is one of these, dataframe, gbif_data, dwca_gbif. And also checks if
#' the needed columns are present in the dataset.
#'
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Object to check validity.
#' @param variable_vector Vector containing required filed names.
#' @return Same object stripped upto dataframe
format_checking <- function(gbif_data, variable_vector = NULL) {
    class <- class(gbif_data)[1]
    if ( class == "dwca_gbif") {
        gbif_data <- gbif_data$data$occurrence.txt
    }else if (class == "gbif_data"){
        gbif_data <- gbif_data$data
    } else if (class != "data.frame" & class != "tbl_df") {
        stop(paste("Incorrect input type, input a dataframe. Current type", class, sep = " "))
    }

    if (!is.null(variable_vector)) {
        sapply(variable_vector, function(value) {
            # print(value %in% colnames(gbif_data))
            if (!(value %in% colnames(gbif_data))) {
                stop(paste("Missing column", value, sep = " "))
            }
        })
    }

    return(gbif_data)

}

remove_unwanted_date_records <- function() {

}

taxonrank_flag <- function() {

}
