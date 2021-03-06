% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_segments.R
\name{as.linseg}
\alias{as.linseg}
\title{Coerce to linear segment object}
\usage{
as.linseg(x, top = "top", bottom = "bottom", names = "name", name = NA)
}
\arguments{
\item{x}{A \code{data.frame} with top/bottom/segment name columns}

\item{top}{The name of the colunn containing the tops (as a string)}

\item{bottom}{The name of the column containing the bottom (as a string)}

\item{names}{The name of the column containing the names (as a string)}

\item{name}{The name associated with the linear segments (optional)}
}
\value{
A \code{linseg} object.
}
\description{
Takes a \code{data.frame} with top/bottom/segment name information and returns a \code{linseg}
object.
}
\examples{
zonesdf <- data.frame(unit_top=c(0, 2, 4, 6, 8),
                      unit_bottom=c(2, 4, 6, 8, 10),
                      unit_name=c("Unit 1", "Unit 2", "Unit 3", "Unit 4", "Unit 5"),
                      unit_litho=c("sand", "silt", "clay", "sand", "clay"))
lsobj <- as.linseg(zonesdf, top="unit_top", bottom="unit_bottom", names="unit_name")

}

