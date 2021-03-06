% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Grouping.R
\name{applyPartitions}
\alias{applyPartitions}
\title{Apply partitions of the partition tree to the data}
\usage{
applyPartitions(tree, data, return = "list")
}
\arguments{
\item{tree}{The tree node to which partition should be listed.}

\item{data}{The data to do grouping}

\item{return}{To return list or the dataframe.}
}
\value{
a list with all possible partitions in the partition history. Or a dataframe with just final partition.
}
\description{
Apply partitions of the partition tree to the data
}
\examples{
\dontrun{
allGroups <- addPartition(grouping = "all")
applyPartitions(allGroups)
}
}
\author{
thiloshon <thiloshon@gmail.com>
}
