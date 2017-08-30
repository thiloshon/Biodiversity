#' Run a questionnaire to get user preferences.
#'
#' Runs a questionnaire to get the user preferences to use in all the quality checks.
#'
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
runQuickCheck <- function () {
    # Aswins function to read user input
    run_questionnaire()

    # Aswins function to load answer to global environment
    read_config_variables()

    # config values as of now,
    #
    # lowest taxonomic level
    # mismatched names
    # spatial resolution
    # region of your interest
    # dates of your observations
    # earliest date of your observations
    # temporal resolution

    # Lets

    ## Running quality checks which takes less time
    data <- data %>%
        georeference_protocol_flag(limit = spatial_resolution) %>%
        georeference_verification_status_flag() %>%
        coordinate_precision_outofrange_flag(limit = spatial_resolution) %>%
        uncertainty_outofrange_flag(limit = spatial_resolution) %>%
        country_code_unknown_flag(limit = Region_of_interest) %>%
        precisionUncertaintyMismatch(limit = spatial_resolution) %>%
        occurrenceEstablishmentFlag() %>%
        depthOutofRangeFlag() %>%
        gbifIssuesFlag()


    ## Cleaning the data after initial check. Here Ashwins flagging system comes
    data <- clean_data()


    ## Running quality checks which takes considerable amount of time
    data <- data %>%
        repeating_digits() %>%
        coordinates_decimal_mismatch() %>%
        locality_coordinate_mismatch_flag(limit = spatial_resolution) %>%
        county_coordinate_mismatch_flag(limit = spatial_resolution) %>%
        stateProvinceCoordinateMismatchFlag(limit = spatial_resolution) %>%
        centerofTheCountryCoordinatesFlag(limit = Region_of_interest) %>%
        coordinateNegatedFlag(limit = Region_of_interest) %>%
        countryCoordinateMismatchFlag(limit = Region_of_interest) %>%
        invasiveFlags() %>%
        nativeFlags()


    ## Running quality checks concerned with time
    if (Dates == "yes") {
        data <- data %>%
            remove_unwanted_date_records(earliestDate = earliest_date) %>%
            georeference_post_occurrence_flag() %>%
            identifiedPreEventFlag() %>%
            impropableIdentifiedDateFlag() %>%
            firstOfYearFlag()
    }


    ## Cleaning the data after second check. Here Ashwins flagging system comes
    data <- clean_data()


    ## Running quality checks which takes huge amount of time
    if (Mismatched_Names == "match") {
        data <- data %>%
            resolve_taxonrank(upto = Taxonomic_Level)
    } else {
        data <- data %>%
            taxonrank_flag(upto = Taxonomic_Level)
    }


    ## Cleaning the data after final check. Here Ashwins flagging system comes
    data <- clean_data()

    ## my function to create two reports on what happened to the original data.
    create_report()

    return(data)


}
