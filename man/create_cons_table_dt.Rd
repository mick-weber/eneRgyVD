% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_helpers.R
\name{create_cons_table_dt}
\alias{create_cons_table_dt}
\title{create_cons_table_dt()}
\usage{
create_cons_table_dt(data, unit, DT_dom = "Bfrtip")
}
\arguments{
\item{data}{Specific electricity consumption, DGE-DIREN data to transform to datatable
Must follow specific data format which can be found in /data}

\item{unit}{Unit currently selected inside the app}

\item{DT_dom}{datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip}
}
\value{
A DT table with export functionalities
}
\description{
create_cons_table_dt()
}
