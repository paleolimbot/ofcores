% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qtag.R
\name{qualifiers}
\alias{qualifiers}
\alias{qualifiers.data.frame}
\alias{qualifiers.grouped_df}
\alias{qualifiers.qtag}
\title{Extract value qualifier names from a qualifier/tag structure}
\usage{
qualifiers(x, ...)

\method{qualifiers}{qtag}(x, ...)

\method{qualifiers}{grouped_df}(x, ...)

\method{qualifiers}{data.frame}(x, ..., quiet = FALSE)
}
\arguments{
\item{x}{A \link{qtag} object}
}
\value{
A vector of qualifier column names
}
\description{
Extract value qualifier names from a qualifier/tag structure
}
\examples{
data(pocmaj)
pocmaj <- as.qtag(pocmaj, qualifiers=c("core", "depth"))
qualifiers(pocmaj)

}

