% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_helpers.R
\name{create_rg_needs_table_dt}
\alias{create_rg_needs_table_dt}
\title{create_rg_needs_table_dt()}
\usage{
create_rg_needs_table_dt(data, unit, DT_dom = "Bfrtip")
}
\arguments{
\item{data}{Specific regener needs dataset to transform to datatable
Must follow specific data format which can be found in /data}

\item{unit}{Unit currently selected inside the app}

\item{DT_dom}{datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip}
}
\value{
a DT table with export functionnalities
}
\description{
create_rg_needs_table_dt()
}
