% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/documentation.R
\docType{data}
\name{regener_datasets}
\alias{regener_datasets}
\alias{regener_cons_ae_aff}
\alias{regener_cons_ae_use}
\alias{regener_cons_ae_year}
\alias{regener_needs}
\alias{regener_misc}
\alias{regener_doc}
\title{RegEner datasets}
\format{
A tibble with various rows and cols which are not detailed yet

An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 13563 rows and 7 columns.

An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 6908 rows and 6 columns.

An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 3612 rows and 5 columns.

An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 903 rows and 9 columns.

An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 17 rows and 4 columns.
}
\source{
<https://www.vd.ch/toutes-les-autorites/departements/departement-de-la-jeunesse-de-lenvironnement-et-de-la-securite-djes/direction-generale-de-lenvironnement-dge/diren-energie>
}
\usage{
regener_cons_ae_aff

regener_cons_ae_use

regener_cons_ae_year

regener_needs

regener_misc

regener_doc
}
\description{
These datasets all originate from different aggregations of the original
RegEner (registre énergétique des bâtiments vaudois) which is created,
updated, and maintained by DGE-DIREN. This dataset provides estimates of
the energy consumed for heating and hot water of all heated VD buildings.
`regener_cons_ae_aff` and `regener_cons_ae_use` show consumption by energy source
and affectation (respectively final use) by commune. `regener_needs` shows
heating needs, `regener_misc` shows non-energetic data (surface, number of buildings, etc.)
and finally `regener_doc` documents the important variables.
Energy-units should be expressed in kWh, and CO2 in tons.
}
\keyword{datasets}
