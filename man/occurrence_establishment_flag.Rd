% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatialCleaning.R
\name{occurrence_establishment_flag}
\alias{occurrence_establishment_flag}
\title{Flag records with questionable occurance status or establishment.}
\usage{
occurrence_establishment_flag(gbif_data)
}
\arguments{
\item{gbif_data}{Dataframe from GBIF with two mandatory fields; ""establishmentMeans", "occurrenceStatus"}
}
\value{
Same dataframe with one additional column; occurrenceEstablishmentFlag
}
\description{
It is important to know if an occurrence is natural, rather than an escapee from captivity, or say, a plant cultivated in a park,
or indeed if the occurrence is extralimital to is normal range, e.g. a vagrant migratory bird that has drifted way off-course.
Usually these records are excluded from spatial analyses. Pertinent information to this may be contained in Darwin Core fields:
DwC:establishmentMeans (e.g. cultivated, invasive, escaped from captivity) and DwC:occurrenceStatus (e.g. present, absent).
These fields could be used to record extralimital occurrences
}
\examples{
\dontrun{
dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
flagged_dat <- occurrence_establishment_flag(dat$data)
}
}
\author{
thiloshon <thiloshon@gmail.com>
}
