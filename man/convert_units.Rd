% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_helpers.R
\name{convert_units}
\alias{convert_units}
\title{convert_units()
Converts units either from dataframe (in target columns) or directly from a numeric value
according to which current unit is selected in the application}
\usage{
convert_units(data, colnames, unit_from = "kWh", unit_to)
}
\arguments{
\item{data}{the dataframe containing the columns where to convert units}

\item{colnames}{the colnames where to convert units}

\item{unit_from}{the unit to convert from}

\item{unit_to}{the unit to convert to. Choice between "kWh", "MWh", "GWh", "TJ"}
}
\value{
the same dataframe with updated units on target colnames
}
\description{
convert_units()
Converts units either from dataframe (in target columns) or directly from a numeric value
according to which current unit is selected in the application
}
