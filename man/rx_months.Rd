\name{rx_months}
\alias{rx_months}
\title{Returns list of available monthly prescription datasets}
\description{
Returns a dataframe of the prescriptions datasets for each month available for download from OpenDataNI.

These are numbered with the most recent available month being '1'. The range to return can also be set using the \code{month_range} argument. The same range of months would be that used in the argument to \code{find_combine()}
}
\usage{
rx_months(month_range = NULL)
}
\arguments{
 \item{month_range}{A \code{range} of months to return, 1 being the most recent available monthly dataset (i.e. \code{rx_months(1:1)}).}
}
\examples{
rx_months()
rx_months(4:8)
}