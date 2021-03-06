% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatialCleaning.R
\name{country_coordinate_mismatch_flag}
\alias{country_coordinate_mismatch_flag}
\title{Flag records with coordinates mismatching country}
\usage{
country_coordinate_mismatch_flag(gbif_data)
}
\arguments{
\item{gbif_data}{Dataframe from GBIF with two mandatory fields; "decimalLatitude", "decimalLongitude"}
}
\value{
Same dataframe with two additional columns; countryCoordinateMismatchFlag, generatedCountries
}
\description{
Geographic coordinates fall outside the area defined by the referenced terrestrial boundary of the country
}
\examples{
dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
flagged_dat <- country_coordinate_mismatch_flag(dat$data)
}
\author{
thiloshon <thiloshon@gmail.com>
}
