gbifIssuesFlag <- function(GBIF_Data) {
    t <- Sys.time()

    GBIF_Data$countryDerivedFromCoordinatesFlag <-
        grepl("COUNTRY_DERIVED_FROM_COORDINATES",
              australianMammals$issue)
    GBIF_Data$geodeticDatumConvertedFlag <-
        grepl("COORDINATE_REPROJECTED", australianMammals$issue)
    GBIF_Data$geodeticDatumInvalidFlag <-
        grepl("GEODETIC_DATUM_INVALID", australianMammals$issue)
    GBIF_Data$geodeticDatumAssumedFlag <-
        grepl("GEODETIC_DATUM_ASSUMED_WGS84", australianMammals$issue)

    print(Sys.time() - t)
    return(GBIF_Data)
}

invasiveFlags <- function(GBIF_Data) {
    t <- Sys.time()

    require(originr)
    require(taxize)
    require(dplyr)

    GBIF_Data$invasiveFlags <- NA


    namesToResolve <-
        names(sort(table(GBIF_Data$scientificName), decreasing = T))  # Sorting the names by frequency,
    # so that the names to be resolved first, are greater part of the complete data

    # namesToResolve <- head(namesToResolve)
    print(length(namesToResolve))

    names <- gbif_parse(namesToResolve)
    names <- names$canonicalname


    result <- gisd(names, simplify = TRUE)

    result <- rbind_all(result)
    # print(result) result <- unlist(result)

    for (counter in 1:dim(result)[1]) {
        # print(result[counter,'species'])
        logic <-
            grepl(result[counter, "species"], GBIF_Data$scientificName)
        # print(logic) print(result[counter,'status'])
        GBIF_Data[logic, "invasiveFlags"] <-
            result[counter, "status"]
    }




    # print(namesToResolve)


    print(Sys.time() - t)
    return(GBIF_Data)
}


nativeFlags <- function(GBIF_Data) {
    t <- Sys.time()

    require(originr)
    require(taxize)
    require(dplyr)

    GBIF_Data$nativeFlags <- NA
    GBIF_Data$isIntroduced <- NA
    GBIF_Data$isCultivated <- NA

    namesToResolve <-
        names(sort(table(GBIF_Data$scientificName), decreasing = T))  # Sorting the names by frequency,
    # so that the names to be resolved first, are greater part of the complete data

    # namesToResolve <- head(namesToResolve)
    print(length(namesToResolve))

    names <- gbif_parse(namesToResolve)
    names <- names$canonicalname


    result <- nsr(names, country = "United States")


    # result <- rbind_all(result)
    print(result)
    # result <- unlist(result)

    if (dim(result)[1] > 0) {
        for (counter in 1:dim(result)[1]) {
            print(result[counter, "species"])
            logic <-
                grepl(result[counter, "species"], GBIF_Data$scientificName)
            # print(logic) print(result[counter,'status'])
            GBIF_Data[logic, "nativeFlags"] <-
                result[counter, "native_status"]
            GBIF_Data[logic, "isIntroduced"] <-
                result[counter, "isIntroduced"]
            GBIF_Data[logic, "isCultivated"] <-
                result[counter, "isCultivated"]
        }
    }
    # print(namesToResolve)


    print(Sys.time() - t)
    return(GBIF_Data)
}

remove_unwanted_date_records <- function() {

}

taxonrank_flag <- function() {

}

format_checking <- function(GBIF_Data, variable_vector = NULL) {
    class <- class(GBIF_Data)[1]
    if ( class == "dwca_gbif" | class == "gbif_data") {
        GBIF_Data <- GBIF_Data$data$occurrence.txt
    } else if (class != "data.frame" & class != "tbl_df") {
        stop(paste("Incorrect input type, input a dataframe. Current type", class, sep = " "))
    }

    if (!is.null(variable_vector)) {
        sapply(variable_vector, function(value) {
            # print(value %in% colnames(GBIF_Data))
            if (!(value %in% colnames(GBIF_Data))) {
                stop(paste("Missing column", value, sep = " "))
            }
        })
    }

    return(GBIF_Data)

}
