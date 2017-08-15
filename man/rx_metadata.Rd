\name{rx_metadata}
\alias{rx_metadata}
\title{Returns GP Prescriptions metadata from OpenDataNI CKAN API}
\usage{
rx_metadata()
}
\description{
Returns the metadata json for the Northern Ireland GP Prescriptions dataset as a list object.

This contains information such as update frequency, license information, contact details and a list of the resources (individual monthly data files) available.
}
\examples{
rx_metadata()
metadata <- rx_metadata()
}